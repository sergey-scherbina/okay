package okay.cluster

import okay.{Aggregator, Chunks, Pane}

/**
 * HOW A KEYED STAGE FINISHES (specs/dataflow.md, stage 2).
 *
 * Every partition has already folded every key it saw, so what is
 * left is combining one accumulator per (key, window) per partition
 * into one per (key, window). The question is only WHERE that happens
 * — and, in a cluster, whether the result of it fits on the node
 * doing it.
 *
 *   - `Merge` puts every partial on one coordinator. Right whenever
 *     the merged result fits there, and it moves the least: one map
 *     per partition, no bucketing, no second hash.
 *   - `Shuffle(r)` gives each of r reducers a hash SHARE of the keys.
 *     The map side writes r buckets, the reduce side merges bucket
 *     by bucket — so no reducer holds more than its share, and the
 *     merging runs on r threads instead of one.
 *   - `Auto` writes buckets anyway (they cost one `%` per update) and
 *     picks the number of reducers when the partials are in and their
 *     size is known, rather than from a guess made before the run.
 *
 * Spark's `reduceByKey` and Flink's `keyBy` have no such choice to
 * make: the aggregation there is a user function the planner cannot
 * reason about, so the exchange is unconditional. Here it is an
 * `Aggregator`, whose merge is part of its type, so map-side combine
 * is always available and the exchange is a question rather than a
 * premise.
 */
enum Finish:
  case Merge
  case Shuffle(reducers: Int)
  case Auto

/**
 * THE PLAN, AS A VALUE (specs/dataflow.md, stage 1).
 *
 * The core already has one plan — `Pipeline`, a Catalyst-shaped tree
 * of local operators with rewrite rules and a compiler onto the
 * chunked transformers. This is the DISTRIBUTED one, and the only
 * things it adds are the ones a second machine forces: a source that
 * comes in partitions, a key, a window, and (from stage 2) an
 * exchange. Everything that is merely local stays local: a `Local`
 * node holds a `Chunks` transformer, so map/filter/take fusion below
 * a keyed stage is still the core's business and is not re-derived
 * here.
 *
 * WHY A PARTITION IS A THUNK. `Src` holds `() => Chunks[A]`, not
 * `Chunks[A]`, and the arrow is load-bearing. specs/cluster.md's
 * fault model is "recompute the partition on a survivor", and the
 * engine itself replays a partition whenever a windowed stage is
 * seeded (once to learn the partition's greatest event time, once to
 * fold it). A `Chunks` built from a pure source is replayable as a
 * value; one built over an `Iterator` is consumed by its first run
 * and answers nothing the second time. The thunk is what makes both
 * kinds honest, and it is where a recipe for re-reading a file, a
 * topic offset range or a table split will hang when stage 4 needs
 * them.
 *
 * WHAT IS STILL NOT HERE: a second keyed stage in one flow. The
 * exchange of stage 2 finishes a keyed stage on several reducers; it
 * does not yet FEED another keyed stage from them, which is what a
 * multi-stage plan needs. `Flows` says so by name when a plan asks
 * for two.
 */
enum Flow[A]:

  /** a PARTITIONED source, in the input's own order — which is what
   * lets a `Sequential` finish merge its slices in that order */
  case Src[A](parts: Vector[() => Chunks[A]]) extends Flow[A]

  /** anything per-partition: map, filter, take — held as a chunk
   * transformer, so the local plan owns this level */
  case Local[A, B](in: Flow[A], name: String, f: Chunks[A] => Chunks[B]) extends Flow[B]

  /** a keyed aggregation — the node the engine exists for */
  case Keyed[A, K, Acc, O](in: Flow[A], key: A => K,
                           agg: Aggregator[A, Acc, O],
                           finish: Finish) extends Flow[(K, O)]

  /** a keyed aggregation over event-time windows (`okay.Windows`);
   * `seeded` buys the exactness described in specs/dataflow.md at the
   * price of one pre-pass over the partition's timestamps */
  case Windowed[A, K, Acc, O](in: Flow[A], size: Long, slide: Long, lateness: Long,
                              key: A => K, at: A => Long,
                              agg: Aggregator[A, Acc, O],
                              seeded: Boolean, finish: Finish) extends Flow[Pane[K, O]]

  def map[B](f: A => B): Flow[B] =
    Flow.Local(this, "map", (c: Chunks[A]) => Chunks.map(c)(f))

  def filter(p: A => Boolean): Flow[A] =
    Flow.Local(this, "filter", (c: Chunks[A]) => Chunks.filter(c)(p))

  /** a keyed aggregation with no window: one accumulator per key */
  def keyBy[K, Acc, O](key: A => K, finish: Finish = Finish.Merge)
                      (agg: Aggregator[A, Acc, O]): Flow[(K, O)] =
    Flow.Keyed(this, key, agg, finish)

  /**
   * Windows that follow one another: every element in exactly one.
   *
   * `seeded = false` drops the pre-pass over the partition's
   * timestamps — one scan cheaper, and equal to the single-threaded
   * answer only on a feed where nothing is ever late. It is a
   * parameter rather than a default because a run that quietly
   * answers differently at parallelism 4 is the failure this engine
   * is meant not to have.
   */
  def tumbling[K, Acc, O](size: Long, lateness: Long, seeded: Boolean = true,
                          finish: Finish = Finish.Merge)
                         (key: A => K)(at: A => Long)
                         (agg: Aggregator[A, Acc, O]): Flow[Pane[K, O]] =
    Flow.Windowed(this, size, size, lateness, key, at, agg, seeded, finish)

  /** windows that overlap: every element in `size / slide` of them */
  def sliding[K, Acc, O](size: Long, slide: Long, lateness: Long, seeded: Boolean = true,
                         finish: Finish = Finish.Merge)
                        (key: A => K)(at: A => Long)
                        (agg: Aggregator[A, Acc, O]): Flow[Pane[K, O]] =
    Flow.Windowed(this, size, slide, lateness, key, at, agg, seeded, finish)

object Flow {

  /** a source already cut into partitions */
  def of[A](parts: Vector[() => Chunks[A]]): Flow[A] = Src(parts)

  /** one partition */
  def one[A](chunks: => Chunks[A]): Flow[A] = Src(Vector(() => chunks))

  /**
   * Cut an indexed collection into `parts` CONTIGUOUS slices of its
   * own order. Contiguity is not a convenience: a `Sequential`
   * summary merges consecutive slices, and a partitioning that
   * interleaved them would be summarising a different stream.
   */
  def slices[A](xs: IndexedSeq[A], parts: Int, chunk: Int = 256): Flow[A] =
    require(parts > 0, "a source has at least one partition")
    val n = xs.length
    Src((0 until parts).toVector.map { i =>
      val from = (n.toLong * i / parts).toInt
      val until = (n.toLong * (i + 1) / parts).toInt
      // `view.slice`, not `iterator.slice`: an Iterator's slice reaches
      // its start by DROPPING, which for the last of eight partitions
      // is seven eighths of the input stepped through and thrown away
      () => Chunks.fromIterator(xs.view.slice(from, until).iterator, chunk)
    })
}
