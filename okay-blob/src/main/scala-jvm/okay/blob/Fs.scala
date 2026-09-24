package okay.blob

import okay.{!, +, %, Async, Chunk, Source, Writer, async, effect, pure}
import okay.Row.plus
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.collection.immutable.ArraySeq

/**
 * The filesystem engine (stage 0): a rooted directory, keys as
 * paths — tests, local runs, honest single-machine deployments; the
 * same trait, so nothing above notices a promotion to S3 later.
 *
 * A key resolves STRICTLY under the root: `..` and friends refuse
 * rather than escape. Puts land in a `.tmp` sibling and MOVE into
 * place, so a reader never sees a half-written object; `.tmp`
 * leftovers of a crash are invisible to get/head/list. The etag is
 * engine-defined (size and mtime) — content hashes arrive with S3,
 * where the protocol carries them.
 */
final class Fs(root: Path, chunkSize: Int = 64 * 1024) extends Blob {

  private type FB = Writer % Chunk[Byte] + Async

  private def resolve(key: String): Either[String, Path] =
    val p = root.resolve(key).normalize()
    if key.isEmpty || key.endsWith("/") then Left(s"'$key' is not an object key")
    else if !p.startsWith(root) then Left(s"key '$key' escapes the store")
    else if key.endsWith(".tmp") then Left(s"key '$key' is reserved")
    else Right(p)

  private def metaOf(key: String, p: Path): Meta =
    val size = Files.size(p)
    val mtime = Files.getLastModifiedTime(p).toMillis
    Meta(key, size, Etag(s"$size-$mtime"), mtime)

  // Writer % Chunk[Byte]'s split test is unchecked under erasure — sound
  // by construction (Say is Writer's ONLY constructor), the TypeableK
  // caveat Writer.scala documents on Writer.run
  def put(key: String, bytes: Source[Chunk[Byte]]): Etag ! Async =
    resolve(key) match
      case Left(why) => throw IllegalArgumentException(why)   // a broken caller, not hostile data
      case Right(path) =>
        val tmp = path.resolveSibling(path.getFileName.toString + ".tmp")
        async {
          Files.createDirectories(path.getParent)
          Files.newOutputStream(tmp)
        }.flatMap { out =>
          val sink: okay.Fold[Chunk[Byte], Unit] = okay.Fold(())((_, c) => out.write(c.toArray))
          Writer.fold[Chunk[Byte], Unit, Unit, Async](bytes)(using summon, sink).flatMap { _ =>
            async {
              out.close()
              Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING,
                StandardCopyOption.ATOMIC_MOVE)
              metaOf(key, path).etag
            }
          }
        }

  def get(key: String, range: Option[(Long, Long)] = None)
  : Either[String, Unit] ! FB =
    resolve(key) match
      case Left(why) => pure(Left(why))
      case Right(path) =>
        effect[FB, Boolean](Async.Run(() => Files.isRegularFile(path))).flatMap {
          case false => pure(Left(s"no such key '$key'"))
          case true =>
            val (from, until) = range.getOrElse((0L, Long.MaxValue))
            effect[FB, java.io.InputStream](Async.Run { () =>
              val in = Files.newInputStream(path)
              var toSkip = from
              while toSkip > 0 do
                val s = in.skip(toSkip)
                if s <= 0 then toSkip = 0 else toSkip -= s
              in
            }).flatMap(in => stream(in, until - from))
        }

  /** tell chunks until `remaining` runs out or the stream ends */
  private def stream(in: java.io.InputStream, remaining: Long)
  : Either[String, Unit] ! FB =
    if remaining <= 0 then effect[FB, Unit](Async.Run(() => in.close())).map(_ => Right(()))
    else
      effect[FB, Chunk[Byte] | Null](Async.Run { () =>
        val want = math.min(chunkSize.toLong, remaining).toInt
        val buf = new Array[Byte](want)
        val n = in.read(buf)
        if n < 0 then { in.close(); null }
        else ArraySeq.unsafeWrapArray(if n == buf.length then buf else buf.take(n))
      }).flatMap {
        case null => pure(Right(()))
        case c =>
          Writer.tell(c).plus[Async].flatMap(_ => stream(in, remaining - c.length))
      }

  def head(key: String): Option[Meta] ! Async =
    resolve(key) match
      case Left(_) => pure(None)
      case Right(path) => async {
        if Files.isRegularFile(path) then Some(metaOf(key, path)) else None
      }

  def list(prefix: String): Source[Chunk[Meta]] =
    type FM = Writer % Chunk[Meta] + Async
    effect[FM, Vector[Meta]](Async.Run { () =>
      if !Files.isDirectory(root) then Vector.empty
      else
        val all = scala.jdk.CollectionConverters.IteratorHasAsScala(
          Files.walk(root).iterator).asScala
          .filter(Files.isRegularFile(_))
          .map(p => root.relativize(p).toString.replace('\\', '/'))
          .filterNot(_.endsWith(".tmp"))
          .filter(_.startsWith(prefix))
          .toVector.sorted
        all.map(k => metaOf(k, root.resolve(k)))
    }).flatMap { metas =>
      // page the answer: 512 keys per told chunk, like a real
      // engine's ListObjectsV2 page
      def page(rest: Vector[Meta]): Source[Chunk[Meta]] =
        if rest.isEmpty then pure(())
        else
          val (c, more) = rest.splitAt(512)
          Writer.tell(ArraySeq.unsafeWrapArray(c.toArray[Meta])).plus[Async]
            .flatMap(_ => page(more))
      page(metas)
    }

  def delete(key: String): Unit ! Async =
    resolve(key) match
      case Left(_) => pure(())
      case Right(path) => async { val _ = Files.deleteIfExists(path) }
}

object Fs:
  def apply(root: Path): Fs = new Fs(root.toAbsolutePath.normalize())
