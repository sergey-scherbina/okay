import okay.*
import okay.given
import scala.annotation.nowarn
import scala.collection.mutable

/**
 * Event-time windows (specs/event-time-windows.md), against a brute
 * force that windows the same events by recomputing every pane from
 * scratch. The operator is a fold with a watermark and an eviction
 * sweep; the reference is a `groupBy`, which is slow, obviously right,
 * and the only kind of oracle worth having here.
 *
 * Cross-platform on purpose: the operator reads no clock and needs no
 * `Async`, so JS and Native run this suite too — which is the argument
 * for it being in the core at all.
 */
class TestWindows extends munit.FunSuite {

  final case class Ev(ts: Long, key: String, v: Long)

  val sum: Aggregator[Ev, Long, Long] = Aggregator.sum[Long].contramap[Ev](_.v)

  /** splitmix64: the same stream on every platform, no `Random` */
  def mix(x: Long): Long =
    var z = x + 0x9e3779b97f4a7c15L
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)

  /**
   * `n` events over `span`, in ARRIVAL order, out of order by at most
   * `jitter` — which every caller keeps under the operator's lateness,
   * so that nothing is late and the reference needs no watermark of
   * its own. Lateness has its own tests below.
   */
  def events(n: Int, span: Long, keys: Int, jitter: Long): Vector[Ev] =
    val evs = (0 until n).map { i =>
      val r = mix(i.toLong)
      val ts = Math.floorMod(r, span)
      Ev(ts, "k" + Math.floorMod(r >>> 17, keys.toLong), Math.floorMod(r >>> 41, 100L))
    }
    // arrival = event time + a bounded jitter, which is what "out of
    // order by at most `jitter`" means
    evs.sortBy(e => e.ts + Math.floorMod(mix(e.ts * 31 + e.v), jitter + 1)).toVector

  /** every pane, by recomputing each from the elements that fall in it */
  def reference(evs: Seq[Ev], size: Long, slide: Long): Map[(Long, String), Long] =
    val m = mutable.Map.empty[(Long, String), Seq[Ev]]
    for e <- evs do
      var start = e.ts - Math.floorMod(e.ts, slide) - size + slide
      var i = 0L
      while i < size / slide do
        m.update((start, e.key), m.getOrElse((start, e.key), Seq.empty) :+ e)
        start += slide
        i += 1
      end while
    m.view.mapValues(sum.run).toMap

  /** drive the class form, collecting every pane it emits */
  def run(evs: Seq[Ev], size: Long, slide: Long, lateness: Long): (Vector[Pane[String, Long]], Windows[String, Ev, Long, Long]) =
    val w = Windows.sliding(size, slide, lateness)((e: Ev) => e.key)((e: Ev) => e.ts)(sum)
    val out = Vector.newBuilder[Pane[String, Long]]
    for e <- evs do w.add(e)(p => { out += p; () })
    w.close()(p => { out += p; () })
    (out.result(), w)

  test("tumbling windows equal a recompute of every pane") {
    val evs = events(2000, 10000L, 7, 40L)
    val (panes, w) = run(evs, 100L, 100L, 50L)
    assertEquals(w.dropped, 0L, "nothing was late by construction")
    assertEquals(panes.map(p => (p.start, p.key) -> p.value).toMap, reference(evs, 100L, 100L))
    assertEquals(panes.map(p => (p.start, p.key)).distinct.length, panes.length,
      "a window is emitted exactly once")
    for p <- panes do assertEquals(p.end - p.start, 100L)
  }

  test("sliding windows equal the same recompute, and each element is in size/slide panes") {
    val evs = events(2000, 10000L, 5, 20L)
    val (panes, w) = run(evs, 300L, 100L, 30L)
    assertEquals(w.dropped, 0L)
    assertEquals(panes.map(p => (p.start, p.key) -> p.value).toMap, reference(evs, 300L, 100L))
    // the total of every pane counts each element three times: three
    // panes per element is exactly what a 300/100 window means
    assertEquals(panes.map(_.value).sum, evs.map(_.v).sum * 3)
  }

  test("a window closes when the watermark passes its end, and close() emits the rest") {
    val w = Windows.tumbling(10L, 0L)((e: Ev) => e.key)((e: Ev) => e.ts)(sum)
    val out = Vector.newBuilder[Pane[String, Long]]
    w.add(Ev(3, "a", 1))(p => { out += p; () })
    assertEquals(out.result(), Vector.empty, "the watermark is inside the window")
    w.add(Ev(11, "a", 2))(p => { out += p; () })
    assertEquals(out.result(), Vector(Pane(0L, 10L, "a", 1L)), "0..10 is complete at 11")
    assertEquals(w.live, 1)
    w.close()(p => { out += p; () })
    assertEquals(out.result(), Vector(Pane(0L, 10L, "a", 1L), Pane(10L, 20L, "a", 2L)))
    assertEquals(w.live, 0, "close() leaves nothing open")
  }

  test("an element later than every window it belongs to is dropped and counted") {
    val w = Windows.tumbling(10L, 0L)((e: Ev) => e.key)((e: Ev) => e.ts)(sum)
    val out = Vector.newBuilder[Pane[String, Long]]
    w.add(Ev(5, "a", 1))(p => { out += p; () })
    w.add(Ev(100, "a", 7))(p => { out += p; () }) // closes 0..10 at value 1
    assertEquals(out.result(), Vector(Pane(0L, 10L, "a", 1L)))
    w.add(Ev(5, "a", 999))(p => { out += p; () }) // too late: 0..10 is gone
    assertEquals(w.dropped, 1L)
    w.close()(p => { out += p; () })
    assertEquals(out.result(), Vector(Pane(0L, 10L, "a", 1L), Pane(100L, 110L, "a", 7L)),
      "the late 999 changed no pane's value")
  }

  test("an element inside the lateness bound is not dropped, however out of order") {
    val w = Windows.tumbling(10L, 50L)((e: Ev) => e.key)((e: Ev) => e.ts)(sum)
    val out = Vector.newBuilder[Pane[String, Long]]
    w.add(Ev(100, "a", 1))(p => { out += p; () })
    w.add(Ev(60, "a", 2))(p => { out += p; () }) // watermark is 50; 60..70 is open
    assertEquals(w.dropped, 0L)
    w.close()(p => { out += p; () })
    assertEquals(out.result().sortBy(_.start),
      Vector(Pane(60L, 70L, "a", 2L), Pane(100L, 110L, "a", 1L)))
  }

  /** the elements as a producer: every one told, as TestPhased does it */
  def producer(evs: Seq[Ev]): Ev ! Writer % Ev =
    evs.foldLeft(pure[Writer % Ev, Ev](evs.head)):
      (m, e) => m.flatMap(_ => Writer.tell(e).map(_ => e))

  /** feed events through a stage, collect what it tells */
  // Writer.run's inline body checks the answer at an abstract type —
  // the trusted kernel's warning (Effects.scala), as in TestPhased
  @nowarn("msg=cannot be checked at runtime")
  def runStage[O, R](evs: Seq[Ev])(st: Stage[Ev, O, R]): (Seq[O], R) =
    !.run(Writer.run(through(producer(evs))(st)))

  test("the stage form gives the class's panes, and the same value drives twice") {
    val evs = events(500, 4000L, 4, 20L)
    val (panes, _) = run(evs, 200L, 100L, 30L)
    val st = Windows.stage(200L, 100L, 30L)((e: Ev) => e.key)((e: Ev) => e.ts)(sum)
    val (first, _) = runStage(evs)(st)
    val (second, _) = runStage(evs)(st)
    def asMap(ps: Seq[Pane[String, Long]]) = ps.map(p => (p.start, p.key) -> p.value).toMap
    assertEquals(asMap(first), asMap(panes))
    assertEquals(asMap(second), asMap(first), "a Stage is a VALUE: no state survives a run")
  }

  test("a window is a stage like any other: it composes under `through`") {
    val evs = events(300, 2000L, 3, 10L)
    val (panes, _) = run(evs, 100L, 100L, 20L)
    val composed: Stage[Ev, Long, Long] = through(
      Windows.stage(100L, 100L, 20L)((e: Ev) => e.key)((e: Ev) => e.ts)(sum))(
      Stage.mapAccumulate[Pane[String, Long], Long, Long](0L)((n, p) => (n + p.value, p.value)))
    val (told, total) = runStage(evs)(composed)
    assertEquals(told.sorted, panes.map(_.value).sorted, "every pane reached the downstream stage")
    assertEquals(total, panes.map(_.value).sum, "and the downstream stage's own state is its answer")
  }
}
