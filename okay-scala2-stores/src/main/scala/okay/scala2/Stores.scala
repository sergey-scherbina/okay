package okay.scala2

import java.nio.file.Path
import okay.Chunk
import okay.blob.{Backup, Blob, Bytes, Etag, Meta}
import okay.cache.{Cache, Invalidations, View, WriteThrough}
import okay.codec.Schema
import okay.docs.{Cond, Docs, PutResult, TopicDocs}
import okay.persist.Topic

/*
 * okay-cache, okay-blob and okay-docs for Scala 2.13
 * (specs/scala2-facade.md, stage 15.4).
 *
 * Probed first. Each store is BUILT from Scala 2 with its own
 * constructors — `Cache.memory(regime, max)`, `Fs(root)`,
 * `S3.wired(...)`, `new TopicDocs[A](topic)`, `View(topic)(key)(fold)` —
 * and its plain values (`Regime`, `Etag`, `Meta`, `Cond`, `PutResult`,
 * the `Stats`) are used directly. What Scala 2 cannot use is every
 * OPERATION: each answers an `A ! Async` or a `Source` of chunks. The
 * three objects here are those operations over `Eff[Async, A]` and
 * `Source[A]`. They are named so that none collides with `Cache`,
 * `Blob` or `Docs` under two wildcard imports.
 */

/** okay-cache's operations */
object Caches {
  def get[K, V](c: Cache[K, V], k: K): Eff[Async, Option[V]] = Async.lift(c.get(k))
  def put[K, V](c: Cache[K, V], k: K, v: V): Eff[Async, Unit] = Async.lift(c.put(k, v))
  def invalidate[K, V](c: Cache[K, V], k: K): Eff[Async, Unit] = Async.lift(c.invalidate(k))

  /** the read most callers should use: on a miss ONE load per key runs,
   * and concurrent callers wait for it instead of loading again */
  def getOrLoad[K, V](c: Cache[K, V], k: K)(load: K => Eff[Async, V]): Eff[Async, V] =
    Async.lift(c.getOrLoad(k)(key => Async.core(load(key))))

  /** run the committing write, then invalidate `k`: the order is held
   * here, not at every call site */
  def writeThrough[K, V, A](c: Cache[K, V], k: K)(commit: Eff[Async, A]): Eff[Async, A] =
    Async.lift(WriteThrough.write(c, k)(Async.core(commit)))

  /** invalidate every key published on `topic` from `from` on; the
   * answer is the next offset to drain from */
  def drain[K, V](topic: Topic, c: Cache[K, V], keyOf: String => K, from: Long, max: Int = 512): Eff[Async, Long] =
    Async.lift(Invalidations.drain(topic, c, keyOf, from, max))

  /** a view's newest value for `k` */
  def latest[K, V](v: View[K, V], k: K): Eff[Async, Option[V]] = Async.lift(v.latest(k))
  def refresh[K, V](v: View[K, V]): Eff[Async, Unit] = Async.lift(v.refresh())
}

/** okay-blob's operations */
object Blobs {
  def put(b: Blob, key: String, bytes: Source[Chunk[Byte]]): Eff[Async, Etag] = Async.lift(b.put(key, bytes.core))
  def putBytes(b: Blob, key: String, bytes: Array[Byte]): Eff[Async, Etag] = Async.lift(b.putBytes(key, bytes))

  /** a file, streamed in chunks */
  def putFile(b: Blob, key: String, path: Path, chunk: Int = Bytes.DefaultChunk): Eff[Async, Etag] =
    Async.lift(b.put(key, Bytes.file(path, chunk)))

  /** the whole object, or a Left naming the absent key */
  def getBytes(b: Blob, key: String, range: Option[(Long, Long)] = None): Eff[Async, Either[String, Array[Byte]]] =
    Async.lift(b.getBytes(key, range))

  /** the object streamed out in chunks; an absent key is an empty
   * stream (`head` tells absent from empty) */
  def stream(b: Blob, key: String, range: Option[(Long, Long)] = None): Source[Chunk[Byte]] =
    Source.of(b.get(key, range).map(_ => ()))

  def head(b: Blob, key: String): Eff[Async, Option[Meta]] = Async.lift(b.head(key))

  /** every key under the prefix, once, in key order */
  def list(b: Blob, prefix: String): Source[Meta] =
    Source.of(okay.Writer.expand[Chunk[Meta], Meta, Unit, okay.Async](b.list(prefix))(c => c))

  /** idempotent: deleting an absent key does nothing */
  def delete(b: Blob, key: String): Eff[Async, Unit] = Async.lift(b.delete(key))

  /** okay-persist's closed segments under `root`, copied to the blob;
   * the answer is the keys written */
  def backup(root: Path, b: Blob, prefix: String = "persist", active: Boolean = true): Eff[Async, Vector[String]] =
    Async.lift(Backup.copy(root, b, prefix, active))

  def restore(b: Blob, root: Path, prefix: String = "persist"): Eff[Async, Vector[String]] =
    Async.lift(Backup.restore(b, root, prefix))
}

/** okay-docs' operations. The defaults of `Docs`' methods are on a
 * trait, so Scala 2 cannot see them; they are restated here. */
object Documents {
  /** documents kept on an okay-persist topic, `indexes` naming the
   * fields `query` can look up. A factory rather than the constructor:
   * `new TopicDocs` from Scala 2 makes the TASTy reader complete the
   * whole class, and its `query` type is built on a Scala 3 union the
   * reader refuses ("Unsupported Scala 3 union in bounds of type +"). */
  def onTopic[A](topic: Topic, indexes: Map[String, A => String] = Map.empty[String, A => String])(implicit schema: Schema[A]): Docs[A] =
    new TopicDocs[A](topic, indexes)

  def get[A](d: Docs[A], id: String): Eff[Async, Option[Docs.Versioned[A]]] = Async.lift(d.get(id))

  /** write `a` under `id` if `cond` holds; `Stale` carries what is there now */
  def put[A](d: Docs[A], id: String, a: A, cond: Cond = Cond.Always): Eff[Async, PutResult] =
    Async.lift(d.put(id, a, cond))

  def delete[A](d: Docs[A], id: String, cond: Cond = Cond.Always): Eff[Async, PutResult] =
    Async.lift(d.delete(id, cond))

  /** every document whose indexed `field` equals `equals`, with its id */
  def query[A](d: Docs[A], field: String, equals: String, max: Int = 256): Source[(String, A)] =
    Source.of(okay.Writer.expand[Chunk[(String, A)], (String, A), Unit, okay.Async](d.query(field, equals, max))(c => c))
}
