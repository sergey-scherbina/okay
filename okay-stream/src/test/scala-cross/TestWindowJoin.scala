package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.std.given
import scala.util.Random
import Chunks.elements

/**
 * The event-time windowed join (specs/stream-join.md, stage 2): the
 * machine over a LIST of arrivals — no clock, so every answer here is
 * exact — then the stage form over the same list.
 */
class TestWindowJoin extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private type Row = (Long, String)
  private type Ev = WindowJoin.Event[String, Row, Row]
  private type Out = (String, (Row, Row))

  private def L(k: String, t: Long, tag: String = ""): Ev = Left(Some((k, (t, s"L$tag$t"))))
  private def R(k: String, t: Long, tag: String = ""): Ev = Right(Some((k, (t, s"R$tag$t"))))
  private val LEnd: Ev = Left(None)
  private val REnd: Ev = Right(None)

  private def fresh(within: Long, lateness: Long) = new WindowJoin[String, Row, Row](within, lateness, _._1, _._1)

  /** drive the machine over the arrivals; the pairs, as (key, left tag, right tag) */
  private def drive(j: WindowJoin[String, Row, Row], evs: Seq[Ev]): Vector[(String, String, String)] =
    val out = Vector.newBuilder[(String, String, String)]
    val emit: Out => Unit = { case (k, (a, b)) => out += ((k, a._2, b._2)); () }
    for ev <- evs do ev match
      case Left(Some((k, a))) => j.left(k, a)(emit)
      case Right(Some((k, b))) => j.right(k, b)(emit)
      case Left(None) => j.leftEnd()
      case Right(None) => j.rightEnd()
    out.result()

  test("a row matches what the other side's window holds on arrival, within reach, same key only") {
    val j = fresh(10L, 0L)
    val out = drive(j, List(L("a", 0), R("a", 5), R("a", 20), R("b", 15), L("a", 15)))
    // L0-R5 on R5's arrival; L15 reaches R5 (10 apart) and R20 (5 apart), never R20 from L0 or b's row
    assertEquals(out, Vector(("a", "L0", "R5"), ("a", "L15", "R5"), ("a", "L15", "R20")))
    assertEquals(j.dropped, 0L)
  }

  test("a held row is evicted as the watermark moves past at + within; a key that never returns is swept") {
    val j = fresh(10L, 0L)
    assertEquals(drive(j, List(L("a", 0), R("a", 0), L("b", 0))), Vector(("a", "L0", "R0")))
    assertEquals(j.held, 3)
    // the right side ahead alone moves nothing: the watermark is the smaller side's
    assertEquals(drive(j, List(R("a", 11))), Vector.empty)
    assertEquals(j.watermark, 0L)
    assertEquals(j.held, 4)
    // the left at 11 takes the watermark to 11: a's rows at 0 are out of every future row's reach
    assertEquals(drive(j, List(L("a", 11))), Vector(("a", "L11", "R11")))
    assertEquals(j.watermark, 11L)
    onFailure(s"held ${j.held}")
    // b's row at 0 is swept too: the watermark advanced by a whole `within` since the last sweep
    assertEquals(j.held, 2, "L11 and R11 alone are within anyone's reach")
  }

  test("a row behind the watermark is dropped and counted, never joined") {
    val j = fresh(10L, 5L)
    assertEquals(drive(j, List(L("a", 100), R("a", 100))), Vector(("a", "L100", "R100")))
    assertEquals(j.watermark, 95L)
    // 90 is within reach of L100 by the interval, but behind the watermark: late
    assertEquals(drive(j, List(R("a", 90))), Vector.empty)
    assertEquals(j.dropped, 1L)
    // 95 is not late — the watermark is inclusive — and reaches R100
    assertEquals(drive(j, List(L("a", 95))), Vector(("a", "L95", "R100")))
    assertEquals(j.dropped, 1L)
  }

  test("the watermark is the smaller side's: a side far ahead cannot make the other side's rows late; an end frees the other store") {
    val j = fresh(10L, 0L)
    assertEquals(drive(j, List(L("a", 1000), R("a", 0))), Vector.empty)
    assertEquals(j.watermark, 0L, "the right side has shown 0, the left 1000: the join is at 0")
    assertEquals(j.dropped, 0L)
    assertEquals(drive(j, List(R("a", 995))), Vector(("a", "L1000", "R995")))
    assertEquals(j.held, 2, "R0 left a's store as R995 arrived: the watermark at 995 is past its reach")
    j.leftEnd()
    assertEquals(j.watermark, 995L, "an ended side's watermark is everything: the join is at the right's")
    assertEquals(j.held, 1, "no left row will come: the right store is freed; L1000 waits for right rows")
    j.rightEnd()
    assertEquals(j.held, 0)
    assert(j.exhausted)
  }

  test("agreement with joinSorted on a bounded input whose timestamps all fall within reach") {
    for seed <- 1 to 20 do
      val rnd = Random(seed)
      val within = 100L
      def rows(tag: String, n: Int) = List.fill(n)((rnd.nextInt(6).toString, rnd.between(0L, within + 1))).zipWithIndex
        .map { case ((k, t), i) => (k, (t, s"$tag$i")) }
      val l = rows("l", rnd.nextInt(40)); val r = rows("r", rnd.nextInt(40))
      // sort-merge needs key order; the windowed join takes the sides in any order, interleaved any way
      val sorted = Chunks.joinSorted(Chunks.fromIterator(l.sortBy(_._1).iterator), Chunks.fromIterator(r.sortBy(_._1).iterator))
        .elements.map { case (k, (a, b)) => (k, a._2, b._2) }.toVector.sorted
      val evs = rnd.shuffle(l.map(x => Left(Some(x)): Ev) ++ r.map(x => Right(Some(x)): Ev))
      note(s"seed $seed: ${l.size} x ${r.size}")
      assertEquals(drive(fresh(within, within), evs).sorted, sorted, s"seed $seed")
  }

  // ------------------------------------------------------------ the stage

  private def producer(evs: Seq[Ev]): Ev ! Writer % Ev =
    evs.foldLeft(pure[Writer % Ev, Ev](evs.head)):
      (m, e) => m.flatMap(_ => Writer.tell(e).map(_ => e))

  test("the stage form gives the machine's pairs, and the same value drives twice; it ends when nothing can be produced") {
    val evs = List(L("a", 0), R("a", 5), R("a", 20), R("b", 15), L("a", 15))
    val st = WindowJoin.stage[String, Row, Row](10L, 0L)(_._1, _._1)
    val (first, _) = !.run(Writer.run(through(producer(evs))(st)))
    val (second, _) = !.run(Writer.run(through(producer(evs))(st)))
    val expected = drive(fresh(10L, 0L), evs)
    assertEquals(first.map { case (k, (a, b)) => (k, a._2, b._2) }.toVector, expected)
    assertEquals(second, first, "a Stage is a VALUE: no state survives a run")
    // both sides ended, then rows nobody can join: the stage has returned and never awaits them
    val (told, _) = !.run(Writer.run(through(producer(List(L("a", 0), LEnd, R("a", 1), REnd, R("a", 2))))(st)))
    assertEquals(told.map { case (k, (a, b)) => (k, a._2, b._2) }, Seq(("a", "L0", "R1")))
  }
}
