package okay.java

import okay.{!, %, Aggregator, Pane, Stage, Writer, pure, through}
import java.util.concurrent.atomic.AtomicInteger
import java.util.stream.{Gatherer, Gatherers, Stream}
import scala.jdk.CollectionConverters.*

/**
 * A `Stage` IS a `Gatherer` (specs/java-gatherers.md), checked as a
 * LAW in both directions: the JDK running an okay stage and okay
 * running it itself must tell the same elements, and okay running a
 * JDK gatherer must answer what `stream.gather` does. Then the parts
 * a law over finite lists cannot show — short-circuit on an INFINITE
 * stream, a downstream that stops asking, the empty stream, a
 * parallel stream, reuse of one value.
 *
 * Runs on the JDK the tests fork on (26 by default); Gatherer is 24+.
 */
class TestGather extends munit.FunSuite {

  // ------------------------------------------------------------ harness

  /** the JDK runs the stage */
  def jdk[I, O, A](xs: Seq[I], s: Stage[I, O, A]): List[O] =
    xs.asJava.stream().gather(Gather.gatherer(s)).toList.asScala.toList

  /** a finite producer */
  def emit[I](xs: Seq[I]): Unit ! Writer % I =
    xs.foldRight(pure[Writer % I, Unit](()))((x, p) => Writer.tell(x).flatMap(_ => p))

  /** okay runs the stage itself */
  def own[I, O, A](src: Unit ! Writer % I, s: Stage[I, O, A]): List[O] =
    !.run(Writer.run(through(src)(s)))._1.toList

  // ------------------------------------------------------------- stages

  val id: Stage[Int, Int, Unit] = Stage.id[Int]

  val chunks: Stage[Int, List[Int], List[Int]] =
    Stage.transduce[Int, List[Int], List[Int]](Nil)(
      (buf, i) =>
        val b = i :: buf
        if b.sizeIs == 3 then Stage.tell[Int, List[Int]](b.reverse).map(_ => Nil)
        else pure(b),
      buf => if buf.isEmpty then pure(buf) else Stage.tell[Int, List[Int]](buf.reverse).map(_ => Nil))

  val runningSum: Stage[Int, Int, Int] = Stage.mapAccumulate[Int, Int, Int](0)((s, i) => (s + i, s + i))

  /** the first three, then the stage ANSWERS — a short-circuit */
  val firstThree: Stage[Int, Int, Unit] =
    Stage.transduceUntil[Int, Int, Int, Unit](0)(
      (n, i) => Stage.tell[Int, Int](i).map(_ => if n + 1 >= 3 then Right(()) else Left(n + 1)),
      _ => ())

  /** tells BEFORE its first await, and flushes a count at the end */
  val framed: Stage[Int, String, Int] =
    Stage.tell[Int, String]("head").flatMap(_ =>
      Stage.transduce[Int, String, Int](0)(
        (n, i) => Stage.tell[Int, String](i.toString).map(_ => n + 1),
        n => Stage.tell[Int, String](s"n=$n").map(_ => n)))

  val inputs: List[List[Int]] = List(Nil, List(7), (1 to 10).toList, (1 to 100).toList)

  // ------------------------------------------------- okay stage -> JDK

  test("law: the JDK running a stage tells what okay's own run tells") {
    for xs <- inputs do
      assertEquals(jdk(xs, id), own(emit(xs), id), s"id over $xs")
      assertEquals(jdk(xs, chunks), own(emit(xs), chunks), s"chunks over $xs")
      assertEquals(jdk(xs, runningSum), own(emit(xs), runningSum), s"runningSum over $xs")
      assertEquals(jdk(xs, firstThree), own(emit(xs), firstThree), s"firstThree over $xs")
      assertEquals(jdk(xs, framed), own(emit(xs), framed), s"framed over $xs")
  }

  /** 0 until n as a JDK stream, counting what the pipeline pulls */
  def counted(n: Int, pulled: AtomicInteger): Stream[Int] =
    Stream.iterate(Integer.valueOf(0), i => i.intValue < n, i => Integer.valueOf(i + 1))
      .peek(_ => pulled.incrementAndGet(): Unit)
      .map(i => i.intValue)

  test("a stage that answers short-circuits: the upstream is pulled no further") {
    // 1000, not infinite: a gatherer that fails to short-circuit must
    // FAIL here (pulled = 1000), not hang the suite
    val pulled = AtomicInteger()
    val out = counted(1000, pulled).gather(Gather.gatherer(firstThree)).toList.asScala.toList
    assertEquals(out, List(0, 1, 2))
    assertEquals(pulled.get, 3, "the integrator must return false with the third element")
  }

  /** each input told `n` times, counting the tells actually made */
  def copies(n: Int, told: AtomicInteger): Stage[Int, Int, Unit] =
    Stage.transduce[Int, Int, Unit](())(
      (_, i) => (1 to n).foldLeft(pure(()): Stage[Int, Int, Unit])((p, _) =>
        p.flatMap(_ => { told.incrementAndGet(): Unit; Stage.tell[Int, Int](i) })),
      pure)

  test("a refused push stops the stage mid-element: the rest of its tells are never made") {
    // one element is worth 1000 tells; `limit(3)` refuses the fourth
    // push, and the stage's continuation past it must not be run
    val told = AtomicInteger()
    val out = counted(10, AtomicInteger()).gather(Gather.gatherer(copies(1000, told))).limit(3)
      .toList.asScala.toList
    assertEquals(out, List(0, 0, 0))
    assert(told.get <= 4, s"${told.get} tells made for a downstream that took 3")
  }

  test("the empty stream: a stage's header and flush come from the finisher") {
    assertEquals(jdk(Nil, framed), List("head", "n=0"))
  }

  test("a parallel stream evaluates the combiner-less gatherer in encounter order") {
    val xs = (1 to 5000).toList
    val par = xs.asJava.parallelStream().gather(Gather.gatherer(chunks)).toList.asScala.toList
    assertEquals(par, own(emit(xs), chunks))
  }

  test("one gatherer value, two streams: no state shared between evaluations") {
    val g = Gather.gatherer(runningSum)
    val a = (1 to 10).toList.asJava.stream().gather(g).toList.asScala.toList
    val b = (1 to 10).toList.asJava.stream().gather(g).toList.asScala.toList
    assertEquals(b, a)
    assertEquals(a.last, 55)
  }

  // ------------------------------------------------- JDK gatherer -> okay

  /** what `stream.gather` answers, as the oracle */
  def gathered[I, S, O](xs: Seq[I], g: Gatherer[I, S, O]): List[O] =
    xs.asJava.stream().gather(g).toList.asScala.toList

  test("law: okay running a JDK gatherer answers what stream.gather does") {
    for xs <- inputs do
      val window = Gatherers.windowFixed[Int](3)
      assertEquals(own(emit(xs), Gather.stage(window)), gathered(xs, window), s"windowFixed over $xs")
      val sliding = Gatherers.windowSliding[Int](2)
      assertEquals(own(emit(xs), Gather.stage(sliding)), gathered(xs, sliding), s"windowSliding over $xs")
      val scan = Gatherers.scan[Int, Int](() => 0, (a, b) => a + b)
      assertEquals(own(emit(xs), Gather.stage(scan)), gathered(xs, scan), s"scan over $xs")
  }

  /** the first two, then the integrator answers false */
  val takeTwo: Gatherer[Int, Array[Int], Int] =
    Gatherer.ofSequential[Int, Array[Int], Int](
      () => Array(0),
      (seen: Array[Int], i: Int, ds: Gatherer.Downstream[? >: Int]) => {
        ds.push(i): Unit
        seen(0) += 1
        seen(0) < 2
      })

  /** 0 until n told, counting what the stage actually pulled */
  def nat(n: Int, pulled: AtomicInteger): Unit ! Writer % Int =
    def go(i: Int): Unit ! Writer % Int =
      if i >= n then pure(())
      else Writer.tell(i).flatMap(_ => { pulled.incrementAndGet(): Unit; go(i + 1) })
    go(0)

  test("an integrator's false stops the stage AWAITING: the producer is pulled no further") {
    val pulled = AtomicInteger()
    assertEquals(own(nat(1000, pulled), Gather.stage(takeTwo)), List(0, 1))
    assert(pulled.get <= 2, s"the producer was pulled ${pulled.get} times past a false")
  }

  test("the finisher runs after a short-circuit, as it does in a JDK stream") {
    // windowFixed(3) over a stream cut to two by takeTwo upstream:
    // the partial window is the FINISHER's push
    val cut = own(nat(1000, AtomicInteger()), Gather.stage(takeTwo))
    assertEquals(own(emit(cut), Gather.stage(Gatherers.windowFixed[Int](3))),
      List(List(0, 1).asJava))
  }

  test("one stage value, run twice: the gatherer's state is made per run") {
    val s = Gather.stage(Gatherers.scan[Int, Int](() => 0, (a, b) => a + b))
    val xs = (1 to 10).toList
    assertEquals(own(emit(xs), s), own(emit(xs), s))
    assertEquals(own(emit(xs), s).last, 55)
  }

  test("a built pipeline over a JDK gatherer is a VALUE: run twice, the same windows") {
    // `through` starts its drive when the program RUNS, not when it is
    // built (windows-stage-rerun-loses-pane, 2026-09-23), so the
    // gatherer's state is made per run of the built program: the
    // second run is a fresh one, not the spent state fed again
    val windows = through(emit(List(1, 2, 3)))(Gather.stage(Gatherers.windowFixed[Int](2)))
    assertEquals(!.run(Writer.run(windows))._1.map(_.asScala.toList), Seq(List(1, 2), List(3)))
    assertEquals(!.run(Writer.run(windows))._1.map(_.asScala.toList), Seq(List(1, 2), List(3)))
  }

  test("a continuation from INSIDE a run, resumed after that run finished, is REFUSED by name") {
    // what cannot be replayed is the rest of a run: the continuation
    // after the first window holds that run's gatherer, whose finisher
    // nulls its array (an NPE inside the JDK, java-gatherers). A Free
    // continuation is multi-shot, so resuming it again is possible —
    // and answered with our message, before the JDK's state is touched.
    // Five elements, not three: `rest` is driven to its NEXT output
    // when it is made, so the refusal needs an await left after that
    // output — [3,4] told, 5 still to come — for the second resume
    // to reach
    val windows = through(emit(List(1, 2, 3, 4, 5)))(Gather.stage(Gatherers.windowFixed[Int](2)))
    Writer.uncons(windows) match
      case Right((first, rest)) =>
        assertEquals(first.asScala.toList, List(1, 2))
        assertEquals(!.run(Writer.run(rest))._1.map(_.asScala.toList), Seq(List(3, 4), List(5)))
        val again = intercept[IllegalStateException](!.run(Writer.run(rest)))
        assert(again.getMessage.contains("already ran"), again.getMessage)
      case Left(_) => fail("the pipeline ended without a window")
  }

  test("Windowed.gatherer: one gatherer value, two evaluations, the same panes") {
    // the JDK path starts the stage PROGRAM per evaluation (Pos.Fresh),
    // so okay-stream's Windows.stage re-run defect (backlog
    // windows-stage-rerun-loses-pane) does not reach it
    val g = Windowed.gatherer[Ev, String, Long, Long](10L, 10L, 0L)(_.key)(_.ts)(sum)
    val evs = (0 until 40).map(i => Ev(i.toLong, "k", 1L)).asJava
    val a = evs.stream().gather(g).toList.asScala.toSet
    val b = evs.stream().gather(g).toList.asScala.toSet
    assertEquals(b, a)
    assertEquals(a.size, 4)
  }

  test("round trip: a JDK gatherer through okay and back through the JDK") {
    val xs = (1 to 50).toList
    val g = Gatherers.windowSliding[Int](4)
    assertEquals(jdk(xs, Gather.stage(g)), gathered(xs, g))
  }

  // ------------------------------------------------------------ Windowed

  final case class Ev(ts: Long, key: String, v: Long)
  val sum: Aggregator[Ev, Long, Long] = Aggregator.sum[Long].contramap[Ev](_.v)

  test("Windowed.gatherer emits each pane when the watermark closes it, not at the end") {
    val evs = (0 until 100).map(i => Ev(i.toLong, "k", 1L))
    val consumed = AtomicInteger()
    val firstPaneAt = AtomicInteger(-1)
    val panes = evs.asJava.stream()
      .peek(_ => consumed.incrementAndGet(): Unit)
      .gather(Windowed.gatherer[Ev, String, Long, Long](10L, 10L, 0L)(_.key)(_.ts)(sum))
      .peek(_ => firstPaneAt.compareAndSet(-1, consumed.get): Unit)
      .toList.asScala.toList
    assertEquals(panes.map(p => (p.start, p.value)).sorted, (0 until 100 by 10).map(s => (s.toLong, 10L)).toList)
    assert(firstPaneAt.get <= 12, s"first pane pushed after ${firstPaneAt.get} of 100 elements")
  }

  test("Windowed.gatherer on a parallel stream: the right panes, where the collector throws") {
    val evs = (0 until 2000).map(i => Ev(i.toLong, "k" + (i % 3), (i % 7).toLong))
    def run(s: Stream[Ev]): Set[Pane[String, Long]] =
      s.gather(Windowed.gatherer[Ev, String, Long, Long](50L, 50L, 0L)(_.key)(_.ts)(sum))
        .toList.asScala.toSet
    assertEquals(run(evs.asJava.parallelStream()), run(evs.asJava.stream()))
  }
}
