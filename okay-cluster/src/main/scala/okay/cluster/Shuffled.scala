package okay.cluster

import okay.*
import okay.codec.{Codecs, Schema}
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
 * A KEYED STAGE WHOSE FINISH IS A SHUFFLE BETWEEN WORKERS, AND A
 * SECOND STAGE AFTER IT (specs/dataflow.md, stage 14).
 *
 * `Job` finishes its one keyed stage by the coordinator's MERGE, which
 * is right while the merged result fits the coordinator and is the
 * bottleneck of a large group-by — feature engineering over all the
 * data. Here the first stage, `key` and `agg`, is finished by
 * REDUCERS ON THE WORKERS, each owning a hash share of the keys and
 * fetching its bucket of every partition from the worker that holds
 * it; the reducers' `(key, value)` output then flows into `andThen`,
 * an ordinary `Wire`, whose partials are all the coordinator merges.
 *
 * Registered by name, as a `Job` is, and for Claim 3's reason: a
 * worker builds the plan from a name and Schema-encoded parameters,
 * so nothing that crosses is a function.
 */
abstract class Shuffled[P, R]:
  /** the source's element */
  type A
  /** the first stage's key, accumulator and presented value */
  type K
  type Acc
  type O

  def name: String
  def params: Schema[P]
  def answer: Schema[R]
  def flow(p: P, parts: Int): Flow[A]
  def key(p: P): A => K
  def agg(p: P): Aggregator[A, Acc, O]
  def keys: Schema[K]
  def accs: Schema[Acc]
  /** the second stage, over the first stage's `(key, value)` output —
   * a hash map's order, so it must not depend on order, and has no
   * event time to window over (refused by name) */
  def andThen(p: P): Wire[(K, O), R]

  private def bucket: Schema[Vector[(K, Acc)]] = Schema.SVector(() => Wire.pair(keys, accs))

  private[cluster] final def refuseWindows(second: Wire[?, ?]): Unit =
    if second.times.nonEmpty then
      throw IllegalArgumentException(
        s"'$name': the second stage of a shuffled job windows by event time, but its input " +
          "is the reducers' (key, value) output, which has no event-time order; window the " +
          "FIRST stage instead (specs/dataflow.md, stage 14)")

  /**
   * THE MAP SIDE of one partition: fold by key, then cut the
   * accumulators into `reducers` buckets.
   *
   * THE BUCKET IS A HASH OF THE KEY'S ENCODING, not of `key.##`. Two
   * processes must put a key in the same bucket, and `##` is only as
   * stable as the key's `hashCode` — an object without a value-based
   * one hashes by identity, which differs in every JVM and would
   * split one key over two reducers silently. The encoding is what
   * crosses anyway, and it is computed once per DISTINCT key per
   * partition, not per element.
   */
  final def mapAt(bytes: Array[Byte], part: Int, of: Int, reducers: Int)
  : Either[String, Vector[Array[Byte]]] =
    Codecs.cbor(params).decode(bytes).map { p =>
      val k = key(p)
      val g = agg(p)
      val m = mutable.HashMap.empty[K, Acc]
      val rows = Scope.using(sc => Chunks.foldLeft(Flows.partition(flow(p, of), part, 0L, sc))(0L)((n, a) =>
        val kk = k(a)
        m.update(kk, g.add(m.getOrElse(kk, g.init), a))
        n + 1))
      Meter.rows(rows)
      val ck = Codecs.cbor(keys)
      val out = Array.fill(reducers)(Vector.newBuilder[(K, Acc)])
      for (kk, acc) <- m do
        out(Math.floorMod(MurmurHash3.bytesHash(ck.encode(kk)), reducers)) += ((kk, acc))
      val cb = Codecs.cbor(bucket)
      out.toVector.map(b => cb.encode(b.result()))
    }

  /** THE REDUCE SIDE: one bucket of every partition, in partition
   * order, merged by key and run through the second stage */
  final def reduceAt(bytes: Array[Byte], buckets: Vector[Array[Byte]]): Either[String, Array[Byte]] =
    Codecs.cbor(params).decode(bytes).flatMap { p =>
      val g = agg(p)
      val second = andThen(p)
      refuseWindows(second)
      val cb = Codecs.cbor(bucket)
      val all = mutable.HashMap.empty[K, Acc]
      val decoded = buckets.map(cb.decode)
      val bad = decoded.collectFirst { case Left(why) => why }
      if bad.isEmpty then
        for case Right(b) <- decoded; (kk, acc) <- b do
          all.update(kk, all.get(kk).fold(acc)(g.merge(_, acc)))
      bad match
        case Some(why) => Left(s"a bucket did not decode: $why")
        case None =>
          val st = second.start(Vector.empty)
          for (kk, acc) <- all do second.step(st, (kk, g.present(acc)))
          Right(Codecs.cbor(second.wire).encode(second.finish(st)))
    }

/** the shuffled jobs this build knows, by name — `Jobs`' twin */
object Shuffled:
  private val known = mutable.LinkedHashMap.empty[String, Shuffled[?, ?]]
  def register(job: Shuffled[?, ?]): Unit = synchronized(known.update(job.name, job))
  def find(name: String): Option[Shuffled[?, ?]] = synchronized(known.get(name))
  def names: Vector[String] = synchronized(known.keys.toVector)
