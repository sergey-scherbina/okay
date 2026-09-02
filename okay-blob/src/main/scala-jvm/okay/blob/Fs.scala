package okay.blob

import okay.{!, +, Async, Chunk, Produce, Stream, async, effect, pure}
import okay.given
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

  private type F = Produce + Async

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

  def put(key: String, bytes: Chunk[Byte] ! F): Etag ! Async =
    resolve(key) match
      case Left(why) => throw IllegalArgumentException(why)   // a broken caller, not hostile data
      case Right(path) =>
        val tmp = path.resolveSibling(path.getFileName.toString + ".tmp")
        async {
          Files.createDirectories(path.getParent)
          Files.newOutputStream(tmp)
        }.flatMap { out =>
          val S = summon[Stream[[X] =>> X ! F, Async]]
          def sink(rest: Chunk[Byte] ! F): Unit ! Async =
            S.uncons(rest).flatMap {
              case None => pure(())
              case Some((c, more)) =>
                async(out.write(c.toArray)).flatMap(_ => sink(more))
            }
          sink(bytes).flatMap { _ =>
            async {
              out.close()
              Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING,
                StandardCopyOption.ATOMIC_MOVE)
              metaOf(key, path).etag
            }
          }
        }

  def get(key: String, range: Option[(Long, Long)] = None)
  : Either[String, Unit] ! F =
    resolve(key) match
      case Left(why) => pure(Left(why))
      case Right(path) =>
        effect[F, Boolean](Async.Run(() => Files.isRegularFile(path))).flatMap {
          case false => pure(Left(s"no such key '$key'"))
          case true =>
            val (from, until) = range.getOrElse((0L, Long.MaxValue))
            effect[F, java.io.InputStream](Async.Run { () =>
              val in = Files.newInputStream(path)
              var toSkip = from
              while toSkip > 0 do
                val s = in.skip(toSkip)
                if s <= 0 then toSkip = 0 else toSkip -= s
              in
            }).flatMap(in => stream(in, until - from))
        }

  /** produce chunks until `remaining` runs out or the stream ends */
  private def stream(in: java.io.InputStream, remaining: Long)
  : Either[String, Unit] ! F =
    if remaining <= 0 then effect[F, Unit](Async.Run(() => in.close())).map(_ => Right(()))
    else
      effect[F, Chunk[Byte] | Null](Async.Run { () =>
        val want = math.min(chunkSize.toLong, remaining).toInt
        val buf = new Array[Byte](want)
        val n = in.read(buf)
        if n < 0 then { in.close(); null }
        else ArraySeq.unsafeWrapArray(if n == buf.length then buf else buf.take(n))
      }).flatMap {
        case null => pure(Right(()))
        case c =>
          effect[F, Chunk[Byte]](c).flatMap(_ => stream(in, remaining - c.length))
      }

  def head(key: String): Option[Meta] ! Async =
    resolve(key) match
      case Left(_) => pure(None)
      case Right(path) => async {
        if Files.isRegularFile(path) then Some(metaOf(key, path)) else None
      }

  def list(prefix: String): Chunk[Meta] ! F =
    effect[F, Vector[Meta]](Async.Run { () =>
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
      // page the answer: 512 keys per produced chunk, like a real
      // engine's ListObjectsV2 page
      def page(rest: Vector[Meta]): Chunk[Meta] ! F =
        if rest.isEmpty then pure(okay.Chunks.emptyChunk)
        else
          val (c, more) = rest.splitAt(512)
          effect[F, Chunk[Meta]](ArraySeq.unsafeWrapArray(c.toArray[Meta]))
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
