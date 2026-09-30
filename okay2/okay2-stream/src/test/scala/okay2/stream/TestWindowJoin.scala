package okay2.stream

import scala.util.Random
import okay2._
import okay2.stream.Chunks.ChunksOps
import okay2.stream.Pipe.into

/**
 * The event-time windowed join (specs/stream-join.md, stage 2), the
 * Scala 2 twin of the core's TestWindowJoin: the machine over a LIST of
 * arrivals — no clock — then the stage form over the same list.
 */
class TestWindowJoin extends munit.FunSuite {

  private type Row = (Long, String)
  private type Ev = WindowJoin.Event[String, Row, Row]
  private type Out = (String, (Row, Row))

  private def L(k: String, t: Long): Ev = Left(Some((k, (t, s"L$t"))))
  private def R(k: String, t: Long): Ev = Right(Some((k, (t, s"R$t"))))
  private val LEnd: Ev = Left(None)
  private val REnd: Ev = Right(None)

  private def fresh(within: Long, lateness: Long) = new WindowJoin[String, Row, Row](within, lateness, _._1, _._1)

  private def drive(j: WindowJoin[String, Row, Row], evs: Seq[Ev]): Vector[(String, String, String)] = {
    val out = Vector.newBuilder[(String, String, String)]
    val emit: Out => Unit = { case (k, (a, b)) => out += ((k, a._2, b._2)); () }
    evs.foreach {
      case Left(Some((k, a))) => j.left(k, a)(emit)
      case Right(Some((k, b))) => j.right(k, b)(emit)
      case Left(None) => j.leftEnd()
      case Right(None) => j.rightEnd()
    }
    out.result()
  }

  test("a row matches what the other side's window holds on arrival, within reach, same key only") {
    val j = fresh(10L, 0L)
    assertEquals(drive(j, List(L("a", 0), R("a", 5), R("a", 20), R("b", 15), L("a", 15))),
      Vector(("a", "L0", "R5"), ("a", "L15", "R5"), ("a", "L15", "R20")))
    assertEquals(j.dropped, 0L)
  }

  test("a held row is evicted as the watermark moves past at + within; a key that never returns is swept") {
    val j = fresh(10L, 0L)
    assertEquals(drive(j, List(L("a", 0), R("a", 0), L("b", 0))), Vector(("a", "L0", "R0")))
    assertEquals(j.held, 3)
    assertEquals(drive(j, List(R("a", 11))), Vector.empty[(String, String, String)])
    assertEquals(j.watermark, 0L)
    assertEquals(j.held, 4)
    assertEquals(drive(j, List(L("a", 11))), Vector(("a", "L11", "R11")))
    assertEquals(j.watermark, 11L)
    assertEquals(j.held, 2)
  }

  test("a row behind the watermark is dropped and counted, never joined") {
    val j = fresh(10L, 5L)
    assertEquals(drive(j, List(L("a", 100), R("a", 100))), Vector(("a", "L100", "R100")))
    assertEquals(j.watermark, 95L)
    assertEquals(drive(j, List(R("a", 90))), Vector.empty[(String, String, String)])
    assertEquals(j.dropped, 1L)
    assertEquals(drive(j, List(L("a", 95))), Vector(("a", "L95", "R100")))
  }

  test("the watermark is the smaller side's; an end frees the other store; exhausted") {
    val j = fresh(10L, 0L)
    assertEquals(drive(j, List(L("a", 1000), R("a", 0))), Vector.empty[(String, String, String)])
    assertEquals(j.watermark, 0L)
    assertEquals(j.dropped, 0L)
    assertEquals(drive(j, List(R("a", 995))), Vector(("a", "L1000", "R995")))
    assertEquals(j.held, 2)
    j.leftEnd()
    assertEquals(j.watermark, 995L)
    assertEquals(j.held, 1)
    j.rightEnd()
    assertEquals(j.held, 0)
    assert(j.exhausted)
  }

  test("agreement with joinSorted on a bounded input whose timestamps all fall within reach") {
    for (seed <- 1 to 20) {
      val rnd = new Random(seed)
      val within = 100L
      def rows(tag: String, n: Int) = List.fill(n)((rnd.nextInt(6).toString, rnd.between(0L, within + 1))).zipWithIndex
        .map { case ((k, t), i) => (k, (t, s"$tag$i")) }
      val l = rows("l", rnd.nextInt(40)); val r = rows("r", rnd.nextInt(40))
      val sorted = Chunks.joinSorted(Chunks.fromIterator(l.sortBy(_._1).iterator), Chunks.fromIterator(r.sortBy(_._1).iterator))
        .elements.map { case (k, (a, b)) => (k, a._2, b._2) }.toVector.sorted
      val evs = rnd.shuffle(l.map(x => Left(Some(x)): Ev) ++ r.map(x => Right(Some(x)): Ev))
      assertEquals(drive(fresh(within, within), evs).sorted, sorted, s"seed $seed")
    }
  }

  private def producer(evs: Seq[Ev]): Ev ! Writer[Ev] =
    evs.foldLeft(pure[Writer[Ev], Ev](evs.head))((m, e) => m.flatMap(_ => Writer.tell(e).map(_ => e)))

  test("the stage form gives the machine's pairs, the same value drives twice, and it ends when nothing can be produced") {
    val evs = List(L("a", 0), R("a", 5), R("a", 20), R("b", 15), L("a", 15))
    val st = WindowJoin.stage[String, Row, Row](10L, 0L)(_._1, _._1)
    val (first, _) = Effects.run(Writer.run(into(producer(evs))(st)))
    val (second, _) = Effects.run(Writer.run(into(producer(evs))(st)))
    assertEquals(first.map { case (k, (a, b)) => (k, a._2, b._2) }.toVector, drive(fresh(10L, 0L), evs))
    assertEquals(second, first, "a Stage is a VALUE: no state survives a run")
    val (told, _) = Effects.run(Writer.run(into(producer(List(L("a", 0), LEnd, R("a", 1), REnd, R("a", 2))))(st)))
    assertEquals(told.map { case (k, (a, b)) => (k, a._2, b._2) }, Seq(("a", "L0", "R1")))
  }
}
