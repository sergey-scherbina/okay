package okay.java

import okay.{!, %, Aggregator, Pane, Stage, Writer, pure, through}
import java.util.stream.{Gatherers, Stream}
import scala.jdk.CollectionConverters.*

/**
 * The examples docs/guide.md §5, docs/theory/07-logic-streams.md and
 * docs/modules/okay-java.md print for `Gather` and `Windowed.gatherer`,
 * VERBATIM (the-record-outlives-the-truth): what the page shows, with
 * the answer the page claims. JDK 24+ at run time, like Gather itself.
 */
class TestDocExamplesGather extends munit.FunSuite:

  def lines(xs: String*): Unit ! Writer % String =
    xs.foldRight(pure[Writer % String, Unit](()))((x, p) => Writer.tell(x).flatMap(_ => p))

  test("guide §5: an okay stage as a JDK intermediate operation, on an infinite stream") {
    // a stage that answers after three: the JDK's integrator returns false
    val firstThree: Stage[Int, Int, Unit] =
      Stage.transduceUntil[Int, Int, Int, Unit](0)(
        (n, i) => Stage.tell[Int, Int](i * 10).map(_ => if n + 1 >= 3 then Right(()) else Left(n + 1)),
        _ => ())

    val out = Stream.iterate(1, _ + 1).gather(Gather.gatherer(firstThree)).toList   // [10, 20, 30]
    assertEquals(out.asScala.toList, List(10, 20, 30))
  }

  /** what a program told, for the assertion (generic, so the row infers) */
  def told[O](p: Unit ! Writer % O): Seq[O] = !.run(Writer.run(p))._1

  test("guide §5: a JDK gatherer as an okay stage") {
    val windows = through(lines("a", "b", "c"))(Gather.stage(Gatherers.windowFixed[String](2)))
    // Writer.run(windows) — (Seq([a, b], [c]), ()); run it again: the same
    assertEquals(told(windows).map(_.asScala.toList), Seq(List("a", "b"), List("c")))
    assertEquals(told(windows).map(_.asScala.toList), Seq(List("a", "b"), List("c")))
  }

  test("okay-java.md: Windowed.gatherer hands each pane on while the input still arrives") {
    final case class Ev(ts: Long, key: String, v: Long)
    val sum = Aggregator.sum[Long].contramap[Ev](_.v)
    val events = Stream.of(Ev(1, "a", 5), Ev(4, "a", 1), Ev(12, "a", 2), Ev(25, "a", 7))

    val panes = events
      .gather(Windowed.gatherer[Ev, String, Long, Long](10, 10, 0)(_.key)(_.ts)(sum))
      .toList                                   // [Pane(0,10,a,6), Pane(10,20,a,2), Pane(20,30,a,7)]
    assertEquals(panes.asScala.toList,
      List(Pane(0L, 10L, "a", 6L), Pane(10L, 20L, "a", 2L), Pane(20L, 30L, "a", 7L)))
  }
