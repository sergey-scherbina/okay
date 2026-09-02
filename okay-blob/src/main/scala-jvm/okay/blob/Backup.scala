package okay.blob

import okay.{!, +, Async, Chunk, Produce, async, effect, pure}
import okay.given
import java.nio.file.{Files, Path}
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.*

/**
 * Append-only makes backup boring, which is the point
 * (specs/persist.md): a CLOSED segment never changes, so incremental
 * backup is copying the files a store does not already hold — to any
 * Blob engine (fs today, S3 when the deployment says so). This lives
 * on the BLOB side of the seam because it knows only the layout
 * convention: files in directories, the NEWEST file of each
 * directory is active and stays home until it rolls. RESTORE is
 * placing files back and letting recovery scan them — the same code
 * path as every startup; okay-persist's Doctor answers "is this copy
 * restorable" offline, before the incident.
 */
object Backup {

  /** copy every closed segment the blob does not hold (same size =
   * already there; closed segments never change); answers the keys
   * copied THIS run — the incremental story is that a second run
   * answers nothing */
  def copy(root: Path, blob: Blob, prefix: String = "persist"): Vector[String] ! Async =
    val closed = closedSegments(root)
    def go(rest: List[(Path, String)], acc: Vector[String]): Vector[String] ! Async = rest match
      case Nil => pure(acc)
      case (path, key) :: more =>
        blob.head(key).flatMap {
          case Some(meta) if meta.size == Files.size(path) => go(more, acc)
          case _ =>
            blob.put(key, stream(path)).flatMap(_ => go(more, acc :+ key))
        }
    go(closed.map((p, rel) => (p, s"$prefix/$rel")), Vector.empty)

  /** place the copied files back under `root` — recovery does the
   * rest, exactly as on every startup */
  def restore(blob: Blob, root: Path, prefix: String = "persist"): Vector[String] ! Async =
    drainList(blob.list(s"$prefix/")).flatMap { metas =>
      def go(rest: List[Meta], acc: Vector[String]): Vector[String] ! Async = rest match
        case Nil => pure(acc)
        case m :: more =>
          val rel = m.key.stripPrefix(s"$prefix/")
          val target = root.resolve(rel)
          async { Files.createDirectories(target.getParent) }.flatMap { _ =>
            fetch(blob, m.key, target).flatMap(_ => go(more, acc :+ rel))
          }
      go(metas.toList, Vector.empty)
    }

  /** every segment file that is NOT the newest of its partition */
  private def closedSegments(root: Path): List[(Path, String)] =
    if !Files.isDirectory(root) then Nil
    else
      val logs = Files.walk(root).iterator.asScala
        .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".log"))
        .toVector
      logs.groupBy(_.getParent).values.flatMap { part =>
        part.sortBy(_.getFileName.toString).dropRight(1)   // the newest stays active
      }.toList.map(p => (p, root.relativize(p).toString.replace('\\', '/')))

  private def stream(path: Path): Chunk[Byte] ! (Produce + Async) =
    type F = Produce + Async
    effect[F, java.io.InputStream](Async.Run(() => Files.newInputStream(path))).flatMap { in =>
      def go: Chunk[Byte] ! F =
        effect[F, Chunk[Byte] | Null](Async.Run { () =>
          val buf = new Array[Byte](64 * 1024)
          val n = in.read(buf)
          if n < 0 then { in.close(); null }
          else ArraySeq.unsafeWrapArray(if n == buf.length then buf else buf.take(n))
        }).flatMap {
          case null => pure(okay.Chunks.emptyChunk)
          case c => effect[F, Chunk[Byte]](c).flatMap(_ => go)
        }
      go
    }

  private def fetch(blob: Blob, key: String, target: Path): Unit ! Async =
    async(Files.newOutputStream(target)).flatMap { out =>
      walkGet(blob.get(key), c => out.write(c.toArray)).map { outcome =>
        out.close()
        outcome match
          case Left(why) => throw IllegalStateException(s"restore '$key': $why")
          case Right(()) => ()
      }
    }

  // the produce-walking helpers (the Poll.drain shape)
  private def drainList(p: Chunk[Meta] ! (Produce + Async)): Vector[Meta] ! Async =
    val S = summon[okay.Stream[[X] =>> X ! (Produce + Async), Async]]
    def go(rest: Chunk[Meta] ! (Produce + Async)): Vector[Meta] ! Async =
      S.uncons(rest).flatMap {
        case None => pure(Vector.empty)
        case Some((c, more)) => go(more).map(c.toVector ++ _)
      }
    go(p)

  private def walkGet(p: Either[String, Unit] ! (Produce + Async),
                      each: Chunk[Byte] => Unit): Either[String, Unit] ! Async =
    import okay.!.*
    // typed by the tree: the split gives an Async[X] or a produced
    // X (Produce is the identity signature — the op IS its answer);
    // that the produced values are chunks is `produced`'s one claim
    (p.resume: @unchecked) match
      case Pure(a) => okay.pure(a)
      case Effect(e) => okay.<|>[Async, Produce](e) match
        case Left(a) => effect(a)
        case Right(c) =>
          each(okay.produced[Chunk[Byte]](c))
          okay.pure(c)
      case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
        case Left(a) => effect(a).flatMap(x => walkGet(k(x), each))
        case Right(c) =>
          each(okay.produced[Chunk[Byte]](c))
          walkGet(k(c), each)
}
