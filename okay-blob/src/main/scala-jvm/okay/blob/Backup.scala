package okay.blob

import okay.{!, Async, Chunk, Source, Writer, async, pure}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Append-only makes backup boring, which is the point
 * (specs/persist.md): a CLOSED segment never changes, so incremental
 * backup is copying the files a store does not already hold — to any
 * Blob engine (fs today, S3 when the deployment says so). This lives
 * on the BLOB side of the seam because it knows only the layout
 * convention: files in directories, the NEWEST file of each directory
 * is the one still being appended to. RESTORE is
 * placing files back and letting recovery scan them — the same code
 * path as every startup; okay-persist's Doctor answers "is this copy
 * restorable" offline, before the incident.
 */
object Backup {

  /**
   * Copy every segment the blob does not already hold, by size —
   * closed segments never change, so a second run answers nothing
   * about them.
   *
   * AND THE ACTIVE ONE, unless a caller says otherwise
   * (backup-active-segment). Copying only closed segments bounds a
   * backup by `segmentBytes` of unsaved books: a shop that appends and
   * then backs up gets everything EXCEPT what it just wrote, which is
   * the part it would miss most. The active file is copied under its
   * own natural key, so nothing about restore changes, and it is
   * copied again whenever it has grown — until it rolls, after which
   * the complete copy replaces the partial one and it never moves
   * again.
   *
   * A COPY OF A LIVE FILE ENDS MID-FRAME, and that is already a shape
   * this store understands: recovery's own rule is that a torn tail on
   * the LAST segment of a partition is the ordinary crash artifact,
   * restorable and named (`Doctor`). A backup of a running store is a
   * crash that did not happen.
   *
   * `active = false` is the old behaviour, for a caller that wants the
   * strict incremental property — a second run answering NOTHING —
   * more than it wants the newest books.
   */
  def copy(root: Path, blob: Blob, prefix: String = "persist",
           active: Boolean = true): Vector[String] ! Async =
    val closed = segments(root, active)
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
    Writer.collect(blob.list(s"$prefix/")).map((chunks, _) => chunks.flatMap(_.toVector)).flatMap { metas =>
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

  /** the segment files to copy: the closed ones always, and the
   * newest of each partition when the caller wants what was written
   * since the last roll */
  private def segments(root: Path, active: Boolean): List[(Path, String)] =
    if !Files.isDirectory(root) then Nil
    else
      val logs = Files.walk(root).iterator.asScala
        .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".log"))
        .toVector
      logs.groupBy(_.getParent).values.flatMap { part =>
        val ordered = part.sortBy(_.getFileName.toString)
        if active then ordered else ordered.dropRight(1)
      }.toList.map(p => (p, root.relativize(p).toString.replace('\\', '/')))

  /** the file as `put` takes — `Bytes.file`, which was this, private,
   * until a consumer copied it verbatim */
  private def stream(path: Path): Source[Chunk[Byte]] = Bytes.file(path)

  // Writer % Chunk[Byte]'s split test is unchecked under erasure — sound
  // by construction (Say is Writer's ONLY constructor), the TypeableK
  // caveat Writer.scala documents on Writer.run
  private def fetch(blob: Blob, key: String, target: Path): Unit ! Async =
    async(Files.newOutputStream(target)).flatMap { out =>
      val sink: okay.Fold[Chunk[Byte], Unit] = okay.Fold(())((_, c) => out.write(c.toArray))
      Writer.fold[Chunk[Byte], Unit, Either[String, Unit], Async](blob.get(key))(using summon)(using summon, sink)
        .map { (_, outcome) =>
          out.close()
          outcome match
            case Left(why) => throw IllegalStateException(s"restore '$key': $why")
            case Right(()) => ()
        }
    }
}
