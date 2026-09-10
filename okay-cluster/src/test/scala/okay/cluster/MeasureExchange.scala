package okay.cluster

import okay.Aggregator
import okay.given

/**
 * WHERE THE EXCHANGE STARTS TO PAY (specs/dataflow.md, stage 2).
 *
 * `Finish.Auto` has to choose between two roads, and the choice
 * should be a measurement rather than an opinion. What is actually
 * being chosen, in ONE PROCESS, is narrow and worth stating before
 * any number: the exchange saves no memory here — every partial is
 * already in this heap — it only lets the final merge run on several
 * threads instead of one. Its price is a second hash per update on
 * the map side and a fibre per reducer. So the question is whether
 * there is enough merging to pay for that, and the answer is a count
 * of ACCUMULATORS.
 *
 * The larger question an exchange answers in a cluster — whether the
 * merged result fits the node doing the merging — is stage 4's, and
 * nothing here may be quoted as an answer to it.
 *
 * The job is deliberately the cheapest possible per element (count
 * per key), so the table is about the map and merge machinery and not
 * about arithmetic; a job with a fatter accumulator moves the
 * crossover DOWN, since the merge gets more expensive while the
 * hand-off does not.
 *
 * Not JMH, for MeasureRFrame's reason: these are tens of milliseconds
 * per run over multi-million-element arrays, a fork per lane would
 * dominate, and the shape is what matters. Medians of five with two
 * warm-up rounds discarded, Live-tagged so a loaded box cannot turn
 * the gate red. The assertions are correctness only — both roads must
 * answer the same thing — never a time threshold.
 */
class MeasureExchange extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")

  final case class Row(key: Int)

  /**
   * ONE MILLION rows, not two, and the two roads ALTERNATE round by
   * round. The first cut of this harness ran all of one road then all
   * of the other over 2M rows in the module's 1 GB test heap, and the
   * table it produced was unusable: the merge lane read 61 ms at
   * 10 000 keys and 10 ms at 20 000, which is more work in less time
   * and therefore not a measurement of the work. It was collecting
   * the previous row's two million dead objects. A lane that swings
   * six-fold cannot price a two-fold effect.
   *
   * So: a smaller feed, a settle between rows, the roads interleaved
   * so drift hits both equally, and the reported number is the
   * MINIMUM of the timed rounds rather than the median — the minimum
   * is the round in which the machine interfered least, and the
   * spread beside it says whether to believe the row at all.
   */
  val N = 1_000_000
  val Parts = 8
  val Rounds = 7
  val Warmup = 3

  /** N rows over K distinct keys, scattered so a partition sees them
   * all rather than a contiguous block of them */
  def rows(k: Int): IndexedSeq[Row] =
    val xs = new Array[Row](N)
    var i = 0
    while i < N do
      xs(i) = Row(((i.toLong * 2654435761L) % k).toInt.abs)
      i += 1
    scala.collection.immutable.ArraySeq.unsafeWrapArray(xs)

  val per: Aggregator[Row, Long, Long] = Aggregator.count[Row]
  val terminal: Aggregator[(Int, Long), Long, Long] = Aggregator.count[(Int, Long)]

  def settle(): Unit =
    System.gc()
    Thread.sleep(200)

  test("the crossover: merge on one thread against an exchange on eight") {
    println(f"%n  $N%,d rows, $Parts partitions, count per key, MINIMUM of $Rounds alternating rounds%n")
    println("  distinct keys |  accumulators |     merge ms |   shuffle ms |  ratio")
    println("  --------------|---------------|--------------|--------------|-------")
    for k <- Vector(1_000, 10_000, 20_000, 30_000, 40_000, 50_000, 75_000,
                    100_000, 250_000, 500_000, 1_000_000) do
      val xs = rows(k)
      def merge(): Run[Long] =
        Flows.run(Flow.slices(xs, Parts).keyBy(_.key)(per), terminal).runWith
      def shuffle(): Run[Long] =
        Flows.run(Flow.slices(xs, Parts).keyBy(_.key, Finish.Shuffle(Parts))(per), terminal).runWith

      settle()
      for _ <- 0 until Warmup do { merge(): Unit; shuffle() }
      val ms = Array.newBuilder[Long]
      val ss = Array.newBuilder[Long]
      var m: Run[Long] | Null = null
      var s: Run[Long] | Null = null
      for _ <- 0 until Rounds do
        val t0 = System.nanoTime(); m = merge(); ms += (System.nanoTime() - t0) / 1000000L
        val t1 = System.nanoTime(); s = shuffle(); ss += (System.nanoTime() - t1) / 1000000L
      val mv = ms.result()
      val sv = ss.result()

      // the answer first: a road that computes something else is not
      // fast or slow, it is wrong
      assertEquals(s.nn.value, m.nn.value, s"$k keys: the two roads disagree")
      assertEquals(m.nn.reducers, 1)
      assertEquals(s.nn.reducers, Parts)

      // what Auto is deciding from: the accumulators that reach the
      // merge, which is min(parts x keys, rows) and not the row count
      val accs = math.min(Parts.toLong * k, N.toLong)
      val ratio = mv.min.toDouble / math.max(1L, sv.min).toDouble
      println(f"  $k%,13d | $accs%,13d | ${mv.min}%,5d (${mv.max}%,d) | " +
        f"${sv.min}%,5d (${sv.max}%,d) | $ratio%.2fx")
    println("%n  the number in brackets is the WORST round: where it is far".format())
    println("  from the first, the row is the machine and not the engine.\n")
  }

  test("slicing an IndexedSeq: does an Iterator reach its start by dropping?") {
    // `Flow.slices` cuts the input by index, and the last of eight
    // partitions starts seven eighths of the way in. Whether that
    // costs anything depends on something not worth guessing about:
    // whether the collection's own iterator overrides `drop`.
    val xs = rows(1_000)
    def viaIterator(): Long =
      var n = 0L
      val it = xs.iterator.slice(N * 7 / 8, N)
      while it.hasNext do { it.next(): Unit; n += 1 }
      n
    def viaView(): Long =
      var n = 0L
      val it = xs.view.slice(N * 7 / 8, N).iterator
      while it.hasNext do { it.next(): Unit; n += 1 }
      n
    settle()
    for _ <- 0 until Warmup do { viaIterator(): Unit; viaView() }
    var it = Long.MaxValue
    var vw = Long.MaxValue
    for _ <- 0 until Rounds do
      val t0 = System.nanoTime(); assertEquals(viaIterator(), (N / 8).toLong)
      it = math.min(it, System.nanoTime() - t0)
      val t1 = System.nanoTime(); assertEquals(viaView(), (N / 8).toLong)
      vw = math.min(vw, System.nanoTime() - t1)
    println(f"%n  last eighth of $N%,d: iterator.slice ${it / 1000}%,d us, " +
      f"view.slice ${vw / 1000}%,d us%n")
  }
