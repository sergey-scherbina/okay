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
