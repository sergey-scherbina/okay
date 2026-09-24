package okay.blob

import okay.{!, +, %, Async, Chunk, Source, Writer, effect, pure}
import okay.Row.plus
import java.nio.file.{Files, Path}
import scala.collection.immutable.ArraySeq

/**
 * Bytes from the outside world as the stream `Blob.put` takes
 * (specs/blob.md, the Source road — the only road since
 * producer-to-writer-carrier stage 2).
 *
 * The 64 KB read loop lived in `Backup`, private, until a consumer
 * (okay-watch) needed exactly it and copied it verbatim — the third
 * caller would have too. It is the one piece of the telling algebra
 * a caller had to write by hand to store a file, and the piece where
 * a `pure`/`tell` mistake would live, so it is here once and public.
 *
 * Constant memory: one buffer of `chunk` bytes, however large the
 * file. The stream is OPENED when the program runs, not when it is
 * built, and closed at end of input — a program is a value and may be
 * built long before, or never run.
 */
object Bytes:

  val DefaultChunk: Int = 64 * 1024

  /** a file, `chunk` bytes at a time */
  def file(path: Path, chunk: Int = DefaultChunk): Source[Chunk[Byte]] =
    stream(Files.newInputStream(path), chunk)

  /** any InputStream, opened when the program runs and closed at its
   * end; `open` is by name for exactly that reason */
  def stream(open: => java.io.InputStream, chunk: Int = DefaultChunk): Source[Chunk[Byte]] =
    type F = Writer % Chunk[Byte] + Async
    effect[F, java.io.InputStream](Async.Run(() => open)).flatMap { in =>
      def go: Source[Chunk[Byte]] =
        effect[F, Chunk[Byte] | Null](Async.Run { () =>
          val buf = new Array[Byte](chunk)
          val n = in.read(buf)
          if n < 0 then { in.close(); null }
          else ArraySeq.unsafeWrapArray(if n == buf.length then buf else buf.take(n))
        }).flatMap {
          case null => pure(())
          case c => Writer.tell(c).plus[Async].flatMap(_ => go)
        }
      go
    }

/** a file into a blob, which is what most callers of `put` mean */
extension (b: Blob)
  def putFile(key: String, path: Path, chunk: Int = Bytes.DefaultChunk): Etag ! Async =
    b.put(key, Bytes.file(path, chunk))
