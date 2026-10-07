package okay.cluster

import okay.{Answers, Chunks, Async, Bulk, Csv, Scheduler, Streamed, Tables}
import okay.freer.*
import okay.std.*
import okay.freer.given
import okay.std.given
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
 * through `Flows.collect`, under the `Scheduler` and `Answers[Async]`
 * given at construction — `Bulk` answers plain values, so the instance
 * is where the program's `Async` ends.
 *
 * `join` is the engine's own `Flow.Join` (lane 2): both sides exchanged
 * by key hash into `parts` buckets, each joined by its reducer — the
 * co-partitioned hash join. Lane 1's road, the right side collected once
 * and shared by every partition of the left (Spark's broadcast join), is
 * `broadcastJoin` below, for a right side known small.
 *
 * `streamed` answers the `Streamed` signatures NATIVELY: the sort-merge
 * and windowed joins as `Flow.Join`s, a window as `Flow.Windowed` with
 * the seeded watermark and `Finish.Auto`; `zip` alone goes through
 * `collect`, because a positional zip of two partitioned collections is
 * defined only when both are partitioned alike, which the seam cannot
 * promise.
 */
final class FlowBulk(parts: Int,
                     lines: String => Iterator[String] = _ => Iterator.empty,
                     bytes: String => Option[Long] = _ => None)
                    (using Scheduler, Answers[Async]) extends Bulk[Flow]:
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
    Flow.Join(bounded(l), bounded(r), parts, JoinHow.Hash())

  /**
   * A side that is itself a keyed stage (a join, a keyed or windowed
   * aggregation) cannot feed a `Flow.Join` directly: two keyed stages
   * need an exchange between them, which the in-process engine does not
   * have (`Flows` refuses it by name). Such a side becomes a MATERIALISED
   * BOUNDARY — run once, on first demand, its rows sliced into `parts`
   * partitions — the in-process stand-in for the shuffle between two
   * stages (streams-seam-docs, found by the one-job page: a chain of
   * three joins failed on the engine and nowhere else).
   */
  private def bounded[X](f: Flow[X]): Flow[X] =
    if !keyed(f) then f
    else
      lazy val rows: Vector[X] = force(Flows.collect(f))
      Flow.of(Vector.tabulate(parts)(i => () => {
        val (lo, hi) = (rows.length.toLong * i / parts, rows.length.toLong * (i + 1) / parts)
        Chunks.fromIterator(rows.iterator.slice(lo.toInt, hi.toInt))
      }))

  /** whether a flow ends in a keyed stage, through its local stages */
  @scala.annotation.tailrec private def keyed(f: Flow[?]): Boolean = f match
    case Flow.Src(_) => false
    case Flow.Local(in, _, _) => keyed(in)
    case Flow.Owned(in, _, _) => keyed(in)
    case _ => true

  /** merged per bucket: the co-partitioned sort-merge join */
  override def joinSorted[K, A, B](l: Flow[(K, A)], r: Flow[(K, B)])(using ord: Ordering[K]): Flow[(K, (A, B))] =
    Flow.Join(bounded(l), bounded(r), parts, JoinHow.Sorted(ord))

  /** the broadcast road: the right side collected once, on the first
   * partition's demand, and shared by every partition of the left */
  def broadcastJoin[K, A, B](l: Flow[(K, A)], r: Flow[(K, B)]): Flow[(K, (A, B))] =
    lazy val right: Map[K, Seq[B]] = force(Flows.collect(r)).groupMap(_._1)(_._2)
    Flow.Local(l, "broadcastJoin", (c: Chunks[(K, A)]) =>
      Chunks.fromIterator(c.elements.flatMap((k, a) => right.getOrElse(k, Nil).iterator.map(b => (k, (a, b))))))

  def cache[A](d: Flow[A]): Flow[A] = Flow.slices(force(Flows.collect(d)), parts)

  def aggregate[A, Acc, Out](d: Flow[A])(agg: Aggregator[A, Acc, Out]): Out = force(Flows.fold(d, agg))

  /** deferred: the flow runs when this is pulled, once per run */
  def toChunks[A](d: Flow[A]): Chunks[A] =
    Chunks.defer(Chunks.fromIterator(force(Flows.collect(d)).iterator))

  /**
   * The `Streamed` signatures answered by the engine, over the heap
   * `Tables.via` threads (the shape of okay-spark's `SparkBulk.sort`):
   * `FlowBulk(4).streamed(Tables.via(B)(p))` for a program in
   * `Tables + Streamed`, or `run(p)` below.
   */
  def streamed[A, F[+_]](p: A ! Streamed + F): A ! State % Tables.Heap[Flow] + F =
    import Row.plus
    type H = Tables.Heap[Flow]
    !.interpret(p):
      [X] => (e: Streamed[X]) => e match
        case Streamed.JoinSorted(l, r, ord) =>
          State.update[H, X](h => h.hold(Flow.Join(h.force(l)(this), h.force(r)(this), parts, JoinHow.Sorted(ord)))).plus[F]
        case Streamed.JoinWithin(l, r, within, lateness, atL, atR) =>
          State.update[H, X](h => h.hold(Flow.Join(h.force(l)(this), h.force(r)(this), parts,
            JoinHow.Within(within, lateness, atL, atR)))).plus[F]
        case Streamed.Windowed(t, size, slide, lateness, key, at, agg) =>
          State.update[H, X](h => h.hold(Flow.Windowed(h.force(t)(this), size, slide, lateness, key, at, agg,
            seeded = true, finish = Finish.Auto))).plus[F]
        case Streamed.Zip(l, r) =>
          State.update[H, X](h => h.hold(of(Chunks.zip(toChunks(h.force(l)(this)), toChunks(h.force(r)(this))).elements.toVector))).plus[F]

  /** a program in `Tables + Streamed`, run on the engine */
  def run[A](p: A ! Tables + Streamed): A =
    State.run(Tables.Heap.empty[Flow])(streamed(Tables.via(this)(p)))._2
