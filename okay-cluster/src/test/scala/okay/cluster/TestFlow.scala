package okay.cluster

import okay.{Aggregator, Pane, Sequential}
import okay.given

/**
 * Stage 1 of specs/dataflow.md: the plan is a value, the executor
 * runs it on a fibre per partition, and THE ANSWER DOES NOT MOVE.
 *
 * Every test here compares a parallel run with the SAME ENGINE at one
 * partition — which at one partition is a plain fold through
 * `okay.Windows`, i.e. the single-threaded program. A distributed
 * engine that answers something else at parallelism 4 is not faster,
 * it is wrong, and that is the only thing this suite is about.
 */
class TestFlow extends munit.FunSuite {

  final case class Ev(ts: Long, key: Int, v: Int)

  /** splitmix64 — the "randomness" is a pure function of the index */
  def mix(x: Long): Long =
    var z = x + 0x9e3779b97f4a7c15L
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)

  /**
   * A feed in ARRIVAL order whose event times run backwards by up to
   * `jitter`. With `jitter` under the window's lateness nothing is
   * ever late; over it, elements are dropped — and how many depends
   * on the watermark, which is the whole point of the seeding test.
   */
  def feed(n: Int, jitter: Long): IndexedSeq[Ev] =
    (0 until n).map { i =>
      val h = mix(i.toLong)
      val back = if jitter == 0 then 0L else math.floorMod(h, jitter)
      Ev(i * 10L - back, math.floorMod(h >>> 20, 16).toInt, math.floorMod(h >>> 40, 100).toInt)
    }

  val Size = 1000L
  val Slide = 250L
  val Late = 300L

  val value: Aggregator[Ev, Long, Long] = Aggregator.sum[Long].contramap[Ev](_.v.toLong)

  /** an ORDER-INDEPENDENT checksum of a result stream: two runs agree
   * when these agree, and a run at parallelism 4 emits its panes in a
   * different order than a run at 1 */
  final case class Sum(n: Long, total: Long, x: Long)
  val paneSum: Aggregator[Pane[Int, Long], Sum, Sum] =
    Aggregator[Pane[Int, Long], Sum, Sum](Sum(0, 0, 0))((s, p) =>
      Sum(s.n + 1, s.total + p.value, s.x ^ mix(p.start * 31 + p.key * 7 + p.value)))((a, b) =>
      Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity)
  val keySum: Aggregator[(Int, Long), Sum, Sum] =
    Aggregator[(Int, Long), Sum, Sum](Sum(0, 0, 0))((s, kv) =>
      Sum(s.n + 1, s.total + kv._2, s.x ^ mix(kv._1 * 31 + kv._2)))((a, b) =>
      Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity)

  test("a stateless flow answers the same at every parallelism") {
    val xs = feed(5000, 0)
    val answers = Vector(1, 2, 3, 8, 17).map { p =>
      Flows.fold(Flow.slices(xs, p).filter(_.v >= 50).map(_.v.toLong),
        Aggregator.sum[Long]).runWith
    }
    assertEquals(answers.distinct.length, 1, s"parallelism moved the answer: $answers")
    assertEquals(answers.head, xs.filter(_.v >= 50).map(_.v.toLong).sum)
  }

  test("collect: every element once and in order — the accumulator allocates per partition") {
    // the trap this pins: `Aggregator.apply(z)(…)` takes its zero BY
    // VALUE, so `init` answers the same object every time — invisible
    // for an immutable accumulator, and for a mutable one it hands
    // every partition's fibre the same buffer
    val xs = feed(1000, 0)
    for p <- Vector(1, 2, 4, 8) do
      val got = Flows.collect(Flow.slices(xs, p).map(_.v)).runWith
      assertEquals(got.length, xs.length, s"$p partitions")
      assertEquals(got, xs.map(_.v).toVector, s"$p partitions")
  }

  test("a keyed aggregation answers the same at every parallelism") {
    val xs = feed(5000, 0)
    val answers = Vector(1, 2, 4, 8).map { p =>
      Flows.fold(Flow.slices(xs, p).keyBy(_.key)(value), keySum).runWith
    }
    assertEquals(answers.distinct.length, 1, s"parallelism moved the answer: $answers")
    assertEquals(answers.head.n, 16L)
  }

  test("tumbling windows: the parallel answer equals the serial one, exactly") {
    val xs = feed(20000, Late - 1)
    val answers = Vector(1, 2, 3, 4, 8, 13).map { p =>
      Flows.run(Flow.slices(xs, p).tumbling(Size, Late)(_.key)(_.ts)(value), paneSum).runWith
    }
    assertEquals(answers.map(_.value).distinct.length, 1,
      s"parallelism moved the answer: ${answers.map(_.value)}")
    assertEquals(answers.map(_.dropped).distinct, Vector(0L))
    assert(answers.head.value.n > 100, s"too few panes to be a test: ${answers.head.value.n}")
  }

  test("sliding windows: every element in four panes, and the answer still does not move") {
    val xs = feed(20000, Late - 1)
    val answers = Vector(1, 2, 5, 8).map { p =>
      Flows.fold(Flow.slices(xs, p).sliding(Size, Slide, Late)(_.key)(_.ts)(value), paneSum).runWith
    }
    assertEquals(answers.distinct.length, 1, s"parallelism moved the answer: $answers")
    // four panes per element: the totals are four times the sum
    assertEquals(answers.head.total, 4 * feed(20000, Late - 1).map(_.v.toLong).sum)
  }

  // -----------------------------------------------------------------
  // stage 2: the exchange
  // -----------------------------------------------------------------

  test("the exchange answers what the merge answers — keyed, every parallelism and reducer count") {
    val xs = feed(20000, 0)
    val base = Flows.run(Flow.slices(xs, 1).keyBy(_.key)(value), keySum).runWith
    assertEquals(base.reducers, 1)
    for p <- Vector(1, 2, 4, 8); r <- Vector(1, 2, 3, 8, 16) do
      val got = Flows.run(Flow.slices(xs, p).keyBy(_.key, Finish.Shuffle(r))(value), keySum).runWith
      assertEquals(got.value, base.value, s"$p partitions, $r reducers")
      assertEquals(got.reducers, r, s"$p partitions, $r reducers")
  }

  test("the exchange answers what the merge answers — windowed, panes bucketed by (window, key)") {
    val xs = feed(20000, Late - 1)
    val base = Flows.fold(Flow.slices(xs, 1).tumbling(Size, Late)(_.key)(_.ts)(value), paneSum).runWith
    for p <- Vector(1, 4, 8); r <- Vector(2, 5, 16) do
      val got = Flows.run(
        Flow.slices(xs, p).tumbling(Size, Late, finish = Finish.Shuffle(r))(_.key)(_.ts)(value),
        paneSum).runWith
      assertEquals(got.value, base, s"$p partitions, $r reducers")
      assertEquals(got.dropped, 0L, s"$p partitions, $r reducers")
  }

  test("Auto: one reducer under the bound, the buckets over it") {
    // the bound is a count of ACCUMULATORS, so the two runs differ in
    // how many distinct keys they make, not in how much data they read
    val few = feed(20000, 0)
    val small = Flows.run(Flow.slices(few, 8).keyBy(_.key, Finish.Auto)(value), keySum).runWith
    assertEquals(small.reducers, 1, "16 keys is far under the bound")

    val n = (Flows.autoBound * 3 / 2).toInt
    val many = (0 until n).map(i => Ev(i.toLong, i, 1))        // every element its own key
    val wide = Flows.run(Flow.slices(many, 8).keyBy(_.key, Finish.Auto)(value), keySum).runWith
    assert(wide.reducers > 1, s"$n accumulators is over the ${Flows.autoBound} bound")
    assertEquals(wide.value.n, n.toLong)
    assertEquals(wide.reducers, 8, "Auto buckets as widely as the source is partitioned")

    // and the answer is the same whichever road it took
    val byHand = Flows.fold(Flow.slices(many, 8).keyBy(_.key)(value), keySum).runWith
    assertEquals(wide.value, byHand)
  }

  // -----------------------------------------------------------------
  // Claim 2: keyed STATE without a shuffle
  // -----------------------------------------------------------------

  /** the bus-bunching statistic of docs/benchmarks.md §20, as an
   * algebra: a slice is summarised by its ends and its inner count,
   * and merging asks whether the gap ACROSS the boundary is short */
  final case class Runs(first: Long, last: Long, bunches: Long, n: Long)
  val Gap = 25L
  val bunching: Sequential[Ev, Runs, Long] = new Sequential[Ev, Runs, Long]:
    def init: Runs = Runs(0, 0, 0, 0)
    def add(a: Runs, e: Ev): Runs =
      if a.n == 0 then Runs(e.ts, e.ts, 0, 1)
      else Runs(a.first, e.ts, a.bunches + close(a.last, e.ts), a.n + 1)
    def merge(a: Runs, b: Runs): Runs =
      if a.n == 0 then b else if b.n == 0 then a
      else Runs(a.first, b.last, a.bunches + b.bunches + close(a.last, b.first), a.n + b.n)
    def present(a: Runs): Long = a.bunches
  def close(prev: Long, next: Long): Long =
    val d = next - prev
    if d >= 0 && d < Gap then 1L else 0L

  test("a Sequential's merge is associative but NOT commutative") {
    val a = bunching.add(bunching.init, Ev(0, 0, 0))
    val b = bunching.add(bunching.init, Ev(10, 0, 0))
    val c = bunching.add(bunching.init, Ev(1000, 0, 0))
    // the boundary gap is 10 forwards and -10 backwards: one is a
    // bunch, the other is not
    assertEquals(bunching.merge(a, b).bunches, 1L)
    assertEquals(bunching.merge(b, a).bunches, 0L)
    // associative, though — which is what makes a merge TREE legal as
    // long as it keeps the order
    assertEquals(bunching.merge(bunching.merge(a, b), c), bunching.merge(a, bunching.merge(b, c)))
  }

  test("keyed state parallelises with no shuffle: the count does not move") {
    val xs = feed(20000, 0)
    val answers = Vector(1, 2, 4, 7, 16).map { p =>
      Flows.fold(Flow.slices(xs, p).keyBy(_.key)(bunching), keySum).runWith
    }
    assertEquals(answers.distinct.length, 1, s"parallelism moved the answer: $answers")
    assert(answers.head.total > 0, "a test with no bunches asserts nothing")
  }

  test("a Sequential keyed aggregator survives the EXCHANGE too") {
    // this is the asymmetry the type is for: a reducer owns a hash
    // share of the keys and merges its buckets partition by
    // partition, so the order a Sequential depends on is intact —
    // while the same aggregator used as the TERMINAL is refused
    val xs = feed(20000, 0)
    val base = Flows.fold(Flow.slices(xs, 1).keyBy(_.key)(bunching), keySum).runWith
    for p <- Vector(2, 4, 8); r <- Vector(2, 3, 8) do
      val got = Flows.fold(Flow.slices(xs, p).keyBy(_.key, Finish.Shuffle(r))(bunching), keySum).runWith
      assertEquals(got, base, s"$p partitions, $r reducers")
  }

  test("a Sequential TERMINAL over a keyed stage is refused by name") {
    val xs = feed(1000, 0)
    // the terminal folds (key, bunches) pairs in a hash map's order,
    // which is not the input's — an order-dependent fold over that is
    // wrong whether one reducer produced it or eight
    val terminal: Sequential[(Int, Long), Runs, Long] = new Sequential[(Int, Long), Runs, Long]:
      def init: Runs = Runs(0, 0, 0, 0)
      def add(a: Runs, kv: (Int, Long)): Runs = bunching.add(a, Ev(kv._2, kv._1, 0))
      def merge(a: Runs, b: Runs): Runs = bunching.merge(a, b)
      def present(a: Runs): Long = a.bunches
    val e = intercept[IllegalArgumentException](
      Flows.fold(Flow.slices(xs, 4).keyBy(_.key)(value), terminal).runWith)
    assert(e.getMessage.contains("Sequential"), e.getMessage)
    assert(e.getMessage.contains("hash map"), e.getMessage)

    // and it is ALLOWED on a stateless plan, where the engine really
    // does hand the terminal the input's order
    val ok = Flows.fold(Flow.slices(xs, 4).map(e2 => (e2.key, e2.ts)), terminal).runWith
    assert(ok >= 0L)
  }

  // -----------------------------------------------------------------
  // stage 3: one pass, many sinks
  // -----------------------------------------------------------------

  /**
   * THE SINGLE-STAGE ROAD MERGES WHAT THE FAN MERGES
   * (dataflow-run-complete-panes).
   *
   * The completeness rule was stage 3's and it went into the fan
   * only, so `Flows.run` merged every pane the job produced where the
   * fan merged the handful that span a partition boundary — 7.6x
   * apart on the Wrocław job, the same answer either way. The rule is
   * on both roads now, and the check is not a time: it is the COUNT
   * of accumulators reaching the coordinator, which must be the fan's
   * exactly, because it is the same rule over the same partitions.
   */
  test("Flows.run merges only the boundary panes, and the same ones the fan merges") {
    val xs = feed(20000, Late - 1)
    val panes = Flows.run(Flow.slices(xs, 1).tumbling(Size, Late)(_.key)(_.ts)(value),
      paneSum).runWith.value.n
    for p <- Vector(2, 4, 8) do
      val road = Flows.run(Flow.slices(xs, p).tumbling(Size, Late)(_.key)(_.ts)(value),
        paneSum).runWith
      val fan = Flows.fan(Flow.slices(xs, p),
        Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)).runWith
      assertEquals(road.value, fan.value, s"$p partitions")
      assertEquals(road.merged, fan.merged,
        s"$p partitions: the single-stage road merges ${road.merged} where the fan merges ${fan.merged}")
      assert(road.merged * 4 < panes,
        s"$p partitions: ${road.merged} of $panes panes still reach the merge — " +
          "the completeness rule is not firing on this road")
  }

  test("with one partition there is nothing to merge, and nothing is") {
    // the rule's own edge: a single partition finishes every pane it
    // produces, so the coordinator merges NOTHING. It is also the
    // case that would hide a rule that never fires, which is why the
    // count is asserted at zero rather than at "small"
    val xs = feed(20000, Late - 1)
    val one = Flows.run(Flow.slices(xs, 1).tumbling(Size, Late)(_.key)(_.ts)(value),
      paneSum).runWith
    assertEquals(one.merged, 0L, "a lone partition sent panes to a merge it is the only side of")
    assert(one.value.n > 100)
  }

  test("a fan of three sinks answers exactly what three separate runs answer") {
    val xs = feed(20000, Late - 1)
    val route = Flows.fold(Flow.slices(xs, 4).tumbling(Size, Late)(_.key)(_.ts)(value), paneSum).runWith
    val stop = Flows.fold(Flow.slices(xs, 4).sliding(Size, Slide, Late)(_.key)(_.ts)(value), paneSum).runWith
    val bunch = Flows.fold(Flow.slices(xs, 4).keyBy(_.key)(bunching), keySum).runWith

    for p <- Vector(1, 2, 4, 8) do
      val fan = Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
        .and(Sink.sliding(Size, Slide, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum))
        .and(Sink.keyed((e: Ev) => e.key, bunching)(keySum))
      val got = Flows.fan(Flow.slices(xs, p), fan).runWith
      val ((a, b), c) = got.value
      assertEquals(a, route, s"$p partitions")
      assertEquals(b, stop, s"$p partitions")
      assertEquals(c, bunch, s"$p partitions")
      assertEquals(got.dropped, 0L, s"$p partitions")
  }

  test("the fan seeds each sink's watermark from its OWN event time") {
    // two windowed sinks over DIFFERENT times in one pass: the
    // pre-pass computes a prefix maximum per column, so neither
    // borrows the other's watermark
    val xs = feed(20000, Late * 8)                    // late elements, so the seeding shows
    val byTs = Flows.run(Flow.slices(xs, 1).tumbling(Size, Late)(_.key)(_.ts)(value), paneSum).runWith
    val shifted = (e: Ev) => e.ts * 2
    val byShift = Flows.run(
      Flow.slices(xs, 1).tumbling(Size, Late)(_.key)(shifted)(value), paneSum).runWith
    assert(byTs.dropped > 0 && byShift.dropped > 0, "neither column drops anything")

    for p <- Vector(2, 4, 8) do
      val fan = Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
        .and(Sink.tumbling(Size, Late, (e: Ev) => e.key, shifted, value)(paneSum))
      val got = Flows.fan(Flow.slices(xs, p), fan).runWith
      assertEquals(got.value._1, byTs.value, s"$p partitions, column 1")
      assertEquals(got.value._2, byShift.value, s"$p partitions, column 2")
      assertEquals(got.dropped, byTs.dropped + byShift.dropped, s"$p partitions")
  }

  test("a partition finishes what no other partition can touch") {
    // the completeness rule. Its correctness is already covered by
    // every equality test above; what this one asserts is that it
    // FIRES — that the coordinator's merge really does shrink, which
    // is the whole of the 5.5x in MeasureWroclawFlow's table.
    val xs = feed(20000, Late - 1)
    val one = Flows.fan(Flow.slices(xs, 1),
      Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)).runWith

    var last = Long.MaxValue
    for p <- Vector(2, 4, 8) do
      val got = Flows.fan(Flow.slices(xs, p),
        Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)).runWith
      assertEquals(got.value, one.value, s"$p partitions")
      // a partition holds ~all the panes it saw when nothing is
      // finished locally; with the rule it holds only the boundary
      assert(got.merged < one.value.n * p / 4,
        s"$p partitions merged ${got.merged} of ${one.value.n} panes — the rule did not fire")
      last = got.merged
    assert(last > 0, "at eight partitions SOMETHING must still cross a boundary")
  }

  test("an unseeded window finishes nothing locally, and still answers the same") {
    // the completeness bound is the seed; a partition that does not
    // know where the stream stood may not declare anything finished
    val xs = feed(20000, Late - 1)
    val seeded = Flows.fan(Flow.slices(xs, 4),
      Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)).runWith
    val loose = Flows.fan(Flow.slices(xs, 4),
      Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value, seeded = false)(paneSum)).runWith
    assertEquals(loose.value, seeded.value, "nothing is late in this feed, so both roads agree")
    assert(loose.merged > seeded.merged,
      s"unseeded merged ${loose.merged}, seeded ${seeded.merged} — it must finish nothing locally")
  }

  test("a fan over a plan that already has a keyed stage is refused by name") {
    val xs = feed(100, 0)
    val e = intercept[IllegalArgumentException](
      Flows.fan(Flow.slices(xs, 2).keyBy(_.key)(value), Sink.fold(keySum)).runWith)
    assert(e.getMessage.contains("in the sinks"), e.getMessage)
  }

  test("a Sequential terminal is refused where the sink is BUILT, not where it runs") {
    val terminal: Sequential[(Int, Long), Runs, Long] = new Sequential[(Int, Long), Runs, Long]:
      def init: Runs = Runs(0, 0, 0, 0)
      def add(a: Runs, kv: (Int, Long)): Runs = bunching.add(a, Ev(kv._2, kv._1, 0))
      def merge(a: Runs, b: Runs): Runs = bunching.merge(a, b)
      def present(a: Runs): Long = a.bunches
    val e = intercept[IllegalArgumentException](Sink.keyed((e2: Ev) => e2.key, value)(terminal))
    assert(e.getMessage.contains("Sequential"), e.getMessage)
    // and Sink.fold, which really does see the input's order, is fine
    val ok = Flows.fan(Flow.slices(feed(100, 0), 4).map(e2 => (e2.key, e2.ts)),
      Sink.fold(terminal)).runWith
    assert(ok.value >= 0L)
  }

  // -----------------------------------------------------------------
  // stage 4: what crosses a wire
  // -----------------------------------------------------------------

  given okay.codec.Schema[Sum] = okay.codec.Schema.derived
  given okay.codec.Schema[Runs] = okay.codec.Schema.derived

  /** the same three stages as the local fan, wired */
  def wiredJob: Wire[Ev, ((Sum, Sum), Sum)] =
    Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
      .and(Wire.sliding(Size, Slide, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum))
      .and(Wire.keyed((e: Ev) => e.key, bunching)(keySum))

  test("every partial survives its codec, and the answer does not move") {
    // the feed has LATE elements on purpose. With a punctual one the
    // drop count is zero on both sides of the wire, and a codec that
    // silently loses that field passes — which is what a control
    // caught here: the first version of this test asserted nothing
    // about `late` because there was nothing to assert.
    for jitter <- Vector(Late - 1, Late * 8) do
      val xs = feed(20000, jitter)
      val local = Flows.fan(Flow.slices(xs, 1), wiredJob).runWith
      if jitter > Late then assert(local.dropped > 0, "the late feed drops nothing")
      for p <- Vector(1, 2, 4, 8) do
        val wired = Flows.fanWired(Flow.slices(xs, p), wiredJob).runWith
        assertEquals(wired.value, local.value, s"$p partitions, jitter $jitter, through the codec")
        assertEquals(wired.dropped, local.dropped, s"$p partitions, jitter $jitter")
        // and the same plan run locally agrees, so the codec is the
        // only thing under test
        val plain = Flows.fan(Flow.slices(xs, p), wiredJob).runWith
        assertEquals(plain.value, local.value, s"$p partitions, jitter $jitter")
        assertEquals(wired.merged, plain.merged, s"$p partitions, jitter $jitter")
  }

  test("what crosses is a SUMMARY plus the boundary, not the panes") {
    // the completeness rule paying twice: the terminal accumulator
    // stands for every pane the partition finished alone, so what a
    // codec has to carry is that one value and the few panes that
    // span an edge
    val xs = feed(20000, Late - 1)
    val one = Flows.fanWired(Flow.slices(xs, 1), wiredJob).runWith
    val eight = Flows.fanWired(Flow.slices(xs, 8), wiredJob).runWith
    assertEquals(eight.value, one.value)
    assert(eight.merged < one.value._1._1.n,
      s"eight partitions handed over ${eight.merged} accumulators for ${one.value._1._1.n} panes")
  }

  test("a partial that cannot be described has no Wire — it is a build-time refusal") {
    // not a runtime check: `Wire.keyed` demands Schema[K] and
    // Schema[Acc], so a sink whose accumulator nobody can describe
    // does not compile. What IS asserted here is the other half —
    // that a Wire is an ordinary Sink and the local driver takes it
    val xs = feed(1000, 0)
    val both: Sink[Ev, Sum] = Wire.keyed((e: Ev) => e.key, value)(keySum)
    assertEquals(Flows.fan(Flow.slices(xs, 4), both).runWith.value,
      Flows.fan(Flow.slices(xs, 4), Sink.keyed((e: Ev) => e.key, value)(keySum)).runWith.value)
  }

  // -----------------------------------------------------------------
  // the watermark: what a slice cannot see
  // -----------------------------------------------------------------

  test("a seeded window drops exactly what the stream drops; an unseeded one drops fewer") {
    val xs = feed(20000, Late * 8)          // far enough back to be late
    val serial = Flows.run(Flow.slices(xs, 1).tumbling(Size, Late)(_.key)(_.ts)(value), paneSum).runWith
    assert(serial.dropped > 0, "a feed with nothing late cannot test the watermark")

    for p <- Vector(2, 4, 8) do
      val seeded = Flows.run(Flow.slices(xs, p).tumbling(Size, Late)(_.key)(_.ts)(value), paneSum).runWith
      assertEquals(seeded.dropped, serial.dropped, s"seeded, $p partitions")
      assertEquals(seeded.value, serial.value, s"seeded, $p partitions")

      val loose = Flows.run(
        Flow.slices(xs, p).tumbling(Size, Late, seeded = false)(_.key)(_.ts)(value), paneSum).runWith
      assert(loose.dropped < serial.dropped,
        s"unseeded at $p partitions dropped ${loose.dropped}, the stream drops ${serial.dropped}")
      assertNotEquals(loose.value, serial.value, s"unseeded, $p partitions")
  }

  test("two keyed stages are refused by name, not answered wrongly") {
    val xs = feed(100, 0)
    val twice = Flow.slices(xs, 2).keyBy(_.key)(value).map((k, v) => Ev(v, k, 1)).keyBy(_.key)(value)
    val e = intercept[IllegalArgumentException](Flows.fold(twice, keySum).runWith)
    assert(e.getMessage.contains("exchange"), e.getMessage)
    assert(e.getMessage.contains("stage 2"), e.getMessage)
  }
}
