package okay.blob

import okay.{!, +, %, Async, Chunk, Source, Writer, async}
import okay.RowLift.plus
import scala.collection.immutable.ArraySeq

/**
 * The object-store seam (specs/blob.md): bytes and streams in the
 * engine, meaning at the edge. Keys are opaque strings with `/` as
 * the conventional prefix separator — the S3 model, which is the
 * model. Streams both directions at constant memory: a 10GB segment
 * never lives in the heap. Multipart is an engine detail under the
 * same `put`; conditional put joins when a consumer needs optimistic
 * commit over blobs.
 */
final case class Etag(value: String)

final case class Meta(key: String, size: Long, etag: Etag, modified: Long)

trait Blob:
  /** stream bytes in; the etag identifies what landed */
  def put(key: String, bytes: Source[Chunk[Byte]]): Etag ! Async

  /** stream bytes out — the chunks are told, the ANSWER is the
   * outcome: an absent key is a Left naming it, never a throw
   * (producer-to-writer-carrier, stage 2: this was `Produce`, where
   * the element sat in the ANSWER position and `pure(chunk)` type-
   * checked and emitted nothing — specs/blob.md's old "Source road"
   * paragraph, now the only road) */
  def get(key: String, range: Option[(Long, Long)] = None)
  : Either[String, Unit] ! Writer % Chunk[Byte] + Async

  /** size, etag, modified — no body */
  def head(key: String): Option[Meta] ! Async

  /** every key under the prefix, once, in key order, told — paged
   * underneath where the engine pages */
  def list(prefix: String): Source[Chunk[Meta]]

  /** idempotent — deleting the absent is a no-op */
  def delete(key: String): Unit ! Async

  // ── the plain road: what most callers actually hold ─────────────

  /** one chunk, whole */
  def putChunk(key: String, chunk: Chunk[Byte]): Etag ! Async =
    put(key, Writer.tell(chunk).plus[Async])

  /** an array, whole — the way a small object is usually in hand */
  def putBytes(key: String, bytes: Array[Byte]): Etag ! Async =
    putChunk(key, ArraySeq.unsafeWrapArray(bytes))

  /** the whole object in memory, or the Left naming the key. For an
   * object that fits; the streaming `get` is the road for one that
   * does not, and this is a consumer of it, not a second engine call */
  // Writer % Chunk[Byte]'s split test is unchecked under erasure — sound
  // by construction (Say is Writer's ONLY constructor), the TypeableK
  // caveat Writer.scala documents on Writer.run
  def getBytes(key: String, range: Option[(Long, Long)] = None)
  : Either[String, Array[Byte]] ! Async =
    async(java.io.ByteArrayOutputStream()).flatMap { out =>
      val sink: okay.Fold[Chunk[Byte], Unit] = okay.Fold(())((_, c) => out.write(c.toArray))
      Writer.fold[Chunk[Byte], Unit, Either[String, Unit], Async](get(key, range))(using summon, sink)
        .map((_, outcome) => outcome.map(_ => out.toByteArray))
    }

object Blob:
  /** an engine's standing as a value (specs/data.md, adapter-stats):
   * calls per operation, misses and failures — one definition for
   * every engine, counted by the seam */
  final case class Stats(engine: String, puts: Long, gets: Long, misses: Long, heads: Long,
                         lists: Long, deletes: Long, failures: Long) derives okay.codec.Schema

  def counted(engine: String, inner: Blob): Counted = new Counted(engine, inner)

  final class Counted(engine: String, inner: Blob) extends Blob:
    import java.util.concurrent.atomic.AtomicLong
    private val puts, gets, misses, heads, lists, deletes, failures = AtomicLong(0L)

    private def counting[B](n: AtomicLong)(p: B ! Async): B ! Async =
      n.incrementAndGet()
      Async.await[B] { k =>
        okay.Async.runAsync(p).onComplete {
          case scala.util.Success(b) => k(Right(b))
          case scala.util.Failure(t) => failures.incrementAndGet(); k(Left(t))
        }(using scala.concurrent.ExecutionContext.parasitic)
        () => ()
      }

    def put(key: String, bytes: Source[Chunk[Byte]]): Etag ! Async = counting(puts)(inner.put(key, bytes))
    def get(key: String, range: Option[(Long, Long)] = None): Either[String, Unit] ! Writer % Chunk[Byte] + Async =
      gets.incrementAndGet()
      inner.get(key, range).map { r => if r.isLeft then misses.incrementAndGet(): Unit; r }
    def head(key: String): Option[Meta] ! Async = counting(heads)(inner.head(key))
    def list(prefix: String): Source[Chunk[Meta]] =
      lists.incrementAndGet()
      inner.list(prefix)
    def delete(key: String): Unit ! Async = counting(deletes)(inner.delete(key))

    def stats: Stats = Stats(engine, puts.get, gets.get, misses.get, heads.get, lists.get, deletes.get, failures.get)
