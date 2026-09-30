package okay.cluster

import okay.*
import okay.given
import okay.Chunks.elements

/**
 * OUR ENGINE AS A PLATFORM (specs/streams-seam.md, lane 1): a
 * `Bulk[Flow]`, so a `Tables` program that names no platform runs on
 * the cluster engine unchanged — `Tables.run(FlowBulk(4))(p)` beside
 * `Tables.run(localBulk)(p)` and `Tables.run(SparkBulk(spark))(p)`.
 *
 * Inside the plan nothing runs: `of` slices the rows into `parts`
 * partitions, `map`/`filter`/`flatMap` are `Local` transformers per
 * partition, `read` is `Bulk`'s default (the splits spread by `of`,
 * each read by `flatMap` where it lands — one fibre per partition, in
 * parallel). The engine runs where the seam asks for a VALUE:
 * `aggregate` folds through `Flows.fold`, `cache` and `toChunks` collect
 * through `Flows.collect`, under the `Scheduler` and `Handler[Async]`
 * given at construction — `Bulk` answers plain values, so the instance
 * is where the program's `Async` ends.
 *
 * `join` is the seam's hash join (specs/bulk.md: "the equi-join only"):
 * the right side collected ONCE on first demand into a map, shared by
 * every partition of the left, which streams against it — Spark's
 * broadcast join, the road `SparkBulk` takes for a small right side.
 * `Tables`' rewrite turns the smaller side right by size. A
 * co-partitioned join of two large sides needs a binary node `Flow`
 * does not have; that is lane 2's `Flow.Join`, not this.
 */
final class FlowBulk(parts: Int,
                     lines: String => Iterator[String] = _ => Iterator.empty,
                     bytes: String => Option[Long] = _ => None)
                    (using Scheduler, Handler[Async]) extends Bulk[Flow]:
  require(parts >= 1, "a FlowBulk has at least one partition")

  private def force[X](p: X ! Async): X = p.runWith

  def of[A](xs: Iterable[A]): Flow[A] = Flow.slices(xs.toIndexedSeq, parts)

  /** one partition: a header-first CSV has no splits (a quoted newline
   * forbids cutting it blind); deferred, so every run re-reads */
  def csv(path: String): Flow[Csv.Row] =
    Flow.one(Chunks.defer(Chunks.fromIterator(Csv.rows(lines(path)))))

  /** pruned at the parser, as the local instance does */
  override def csv(path: String, columns: Option[Set[String]]): Flow[Csv.Row] =
    Flow.one(Chunks.defer(Chunks.fromIterator(Csv.rows(lines(path), columns))))

  override def size(path: String): Option[Long] = bytes(path)

  def map[A, B](d: Flow[A])(f: A => B): Flow[B] = d.map(f)
  def flatMap[A, B](d: Flow[A])(f: A => IterableOnce[B]): Flow[B] =
    Flow.Local(d, "flatMap", (c: Chunks[A]) => Chunks.fromIterator(c.elements.flatMap(f)))
  def filter[A](d: Flow[A])(p: A => Boolean): Flow[A] = d.filter(p)

  def join[K, A, B](l: Flow[(K, A)], r: Flow[(K, B)]): Flow[(K, (A, B))] =
    // collected on the first partition's demand, once for every partition
    // of the left and every run: the right side of a hash join is held
    // whole by contract, and holding it once is what `cache` promises
    lazy val right: Map[K, Seq[B]] = force(Flows.collect(r)).groupMap(_._1)(_._2)
    Flow.Local(l, "join", (c: Chunks[(K, A)]) =>
      Chunks.fromIterator(c.elements.flatMap((k, a) => right.getOrElse(k, Nil).iterator.map(b => (k, (a, b))))))

  def cache[A](d: Flow[A]): Flow[A] = Flow.slices(force(Flows.collect(d)), parts)

  def aggregate[A, Acc, Out](d: Flow[A])(agg: Aggregator[A, Acc, Out]): Out = force(Flows.fold(d, agg))

  /** deferred: the flow runs when this is pulled, once per run */
  def toChunks[A](d: Flow[A]): Chunks[A] =
    Chunks.defer(Chunks.fromIterator(force(Flows.collect(d)).iterator))
