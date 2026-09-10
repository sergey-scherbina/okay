package okay.cluster

import okay.{Aggregator, Chunks, Pane}

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
 * WHAT STAGE 1 DOES NOT HAVE: an exchange, and therefore at most one
 * keyed stage per flow. `Flows` says so by name when a plan asks for
 * two.
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
                           agg: Aggregator[A, Acc, O]) extends Flow[(K, O)]

  /** a keyed aggregation over event-time windows (`okay.Windows`);
   * `seeded` buys the exactness described in specs/dataflow.md at the
   * price of one pre-pass over the partition's timestamps */
  case Windowed[A, K, Acc, O](in: Flow[A], size: Long, slide: Long, lateness: Long,
                              key: A => K, at: A => Long,
                              agg: Aggregator[A, Acc, O],
                              seeded: Boolean) extends Flow[Pane[K, O]]

  def map[B](f: A => B): Flow[B] =
    Flow.Local(this, "map", (c: Chunks[A]) => Chunks.map(c)(f))

  def filter(p: A => Boolean): Flow[A] =
    Flow.Local(this, "filter", (c: Chunks[A]) => Chunks.filter(c)(p))

  /** a keyed aggregation with no window: one accumulator per key */
  def keyBy[K, Acc, O](key: A => K)(agg: Aggregator[A, Acc, O]): Flow[(K, O)] =
    Flow.Keyed(this, key, agg)

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
  def tumbling[K, Acc, O](size: Long, lateness: Long, seeded: Boolean = true)
                         (key: A => K)(at: A => Long)
                         (agg: Aggregator[A, Acc, O]): Flow[Pane[K, O]] =
    Flow.Windowed(this, size, size, lateness, key, at, agg, seeded)

  /** windows that overlap: every element in `size / slide` of them */
  def sliding[K, Acc, O](size: Long, slide: Long, lateness: Long, seeded: Boolean = true)
                        (key: A => K)(at: A => Long)
                        (agg: Aggregator[A, Acc, O]): Flow[Pane[K, O]] =
    Flow.Windowed(this, size, slide, lateness, key, at, agg, seeded)

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
      () => Chunks.fromIterator(xs.iterator.slice(from, until), chunk)
    })
}
