package okay.blob

import okay.{!, +, Async, Chunk, Produce, async, pure}
import okay.given
import okay.persist.{Record, Segments, Topic}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Cold-segment tiering (specs/persist.md, stage 3: "segment offload
 * to object storage — our log's cold tail IS our lake"). Backup
 * made the copies; offload makes them the ONLY copy of the cold
 * tail:
 *
 *  - `evict` deletes local CLOSED segments — oldest first, only
 *    those whose blob copy is VERIFIED byte-for-byte in size, only
 *    while the local total exceeds the declared budget. On the
 *    next open, `begin` has advanced exactly as under retention;
 *    nothing about the engine changes.
 *  - `read` serves history from the blob: the documented segment
 *    format parses anywhere (persist's public `Segments` reader),
 *    so a read below the local `begin` fetches the covering copies
 *    and answers the same records the disk once held.
 *  - `Tiered` composes the two roads behind one Async read — the
 *    access-path rule: the engine SPI stays sync and local, what
 *    crosses a wire speaks Async.
 *
 * Like Backup, this lives on the blob side of the seam and touches
 * only the LAYOUT convention plus the documented format. `evict`
 * runs against a CLOSED store root (an operations window, exactly
 * like restore) — a live FileStore holds its segment list in
 * memory and must not have files deleted under it.
 */
object Offload {

  /** verified-then-evict: answers the local paths deleted. Safety
   * first — a segment leaves the disk only when the blob's copy
   * matches its size, and the newest (active) file of each
   * partition never leaves. */
  def evict(root: Path, blob: Blob, prefix: String = "persist",
            keepLocalBytes: Long): Vector[String] ! Async =
    val closed = closedByAge(root)
    def totalBytes: Long =
      if !Files.isDirectory(root) then 0L
      else Files.walk(root).iterator.asScala
        .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".log"))
        .map(Files.size).sum

    def go(rest: List[(Path, String)], acc: Vector[String]): Vector[String] ! Async =
      rest match
        case Nil => pure(acc)
        case (path, rel) :: more =>
          if totalBytes <= keepLocalBytes then pure(acc)
          else
            blob.head(s"$prefix/$rel").flatMap {
              case Some(meta) if meta.size == Files.size(path) =>
                async { Files.delete(path) }.flatMap(_ => go(more, acc :+ rel))
              case _ =>
                // not safely in the blob: NOT eligible — the budget
                // may stay exceeded rather than data leave unsafely
                go(more, acc)
            }
    go(closed, Vector.empty)

  /** history from the blob: every stored segment of this topic and
   * partition whose records can cover `from`, parsed through the
   * documented format, filtered and bounded like a local read */
  def read(blob: Blob, prefix: String, topic: String, partition: Int,
           from: Long, max: Int): Vector[Record] ! Async =
    drainList(blob.list(s"$prefix/$topic/$partition/")).flatMap { metas =>
      val sorted = metas.sortBy(_.key)
      def go(rest: List[Meta], acc: Vector[Record]): Vector[Record] ! Async = rest match
        case Nil => pure(acc)
        case m :: more =>
          if acc.length >= max then pure(acc)
          else
            fetchBytes(blob, m.key).flatMap { bytes =>
              val parsed = Segments.parse(bytes)
              val wanted = parsed.records.filter(_.offset >= from).take(max - acc.length)
              go(more, acc ++ wanted)
            }
      go(sorted.toList, Vector.empty)
    }

  /**
   * One read over both roads: at or past the local `begin`, the
   * local engine answers; below it, the blob does — TooEarly stops
   * meaning "gone" and starts meaning "cold", which is the entire
   * point of the tier.
   */
  final class Tiered(local: Topic, blob: Blob, prefix: String, topic: String):
    def read(partition: Int, from: Long, max: Int): Vector[Record] ! Async =
      async(local.read(partition, from, max)).flatMap {
        case Topic.Read.Records(rs) => pure(rs)
        case Topic.Read.TooEarly(b) =>
          // the blob may still hold copies of segments that are ALSO
          // local (backup covers everything closed); the cold road
          // serves strictly below the local begin, so the two roads
          // never overlap
          Offload.read(blob, prefix, topic, partition, from, max)
            .map(_.filter(_.offset < b)).flatMap { cold =>
            if cold.length >= max then pure(cold)
            else async(local.read(partition, b, max - cold.length)).map {
              case Topic.Read.Records(rs) => cold ++ rs
              case _ => cold
            }
          }
      }
    def end(partition: Int): Long ! Async = async(local.end(partition))

  // ── plumbing ─────────────────────────────────────────────────────

  /** closed segments across the tree, OLDEST first per partition
   * (base order); the newest file of a partition is active */
  private def closedByAge(root: Path): List[(Path, String)] =
    if !Files.isDirectory(root) then Nil
    else
      val logs = Files.walk(root).iterator.asScala
        .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".log"))
        .toVector
      logs.groupBy(_.getParent).toList.flatMap { (_, files) =>
        files.sortBy(_.getFileName.toString).init
      }.sortBy(_.getFileName.toString)
        .map(p => (p, root.relativize(p).toString))

  /** the whole object as bytes (the Backup walkGet shape: chunks
   * are the produced values, the ANSWER is the outcome — a Left
   * names the key and throws here, since an offloaded read has no
   * damage-is-data story to tell about a MISSING copy) */
  private[blob] def fetchBytes(blob: Blob, key: String): Array[Byte] ! Async =
    import okay.!.*
    def walk(p: Either[String, Unit] ! (Produce + Async),
             acc: Vector[Array[Byte]]): Vector[Array[Byte]] ! Async =
      // typed by the tree (the Backup walker's shape): a produced X
      // is a chunk by `produced`'s one claim
      (p.resume: @unchecked) match
        case Pure(a) => a match
          case Left(why) => throw IllegalStateException(s"offload read '$key': $why")
          case Right(()) => okay.pure(acc)
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => okay.effect(a).map(_ => acc)
          case Right(c) => okay.pure(acc :+ okay.produced[Chunk[Byte]](c).toArray)
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => okay.effect(a).flatMap(x => walk(k(x), acc))
          case Right(c) => walk(k(c), acc :+ okay.produced[Chunk[Byte]](c).toArray)
    walk(blob.get(key), Vector.empty).map { parts =>
      val out = new Array[Byte](parts.map(_.length).sum)
      var at = 0
      for a <- parts do { System.arraycopy(a, 0, out, at, a.length); at += a.length }
      out
    }

  private def drainList(p: Chunk[Meta] ! (Produce + Async)): Vector[Meta] ! Async =
    val S = summon[okay.Stream[[X] =>> X ! (Produce + Async), Async]]
    def go(rest: Chunk[Meta] ! (Produce + Async)): Vector[Meta] ! Async =
      S.uncons(rest).flatMap {
        case None => pure(Vector.empty)
        case Some((c, more)) => go(more).map(c.toVector ++ _)
      }
    go(p)
}
