package okay.freer

import okay.*
import okay.given

import okay.freer.Row.*

/**
 * handle-frames-loops: every bespoke handler loop, a handler per level, a hundred thousand levels, on the
 * engine's own stack (Scala.js has no other) — each was an eager loop nested in the one that forced it.
 */
class TestHandleFramesLoops extends munit.FunSuite:

  val n = 100000
  type W = Writer % Int + Pure

  def wrun(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Writer.run[Int, Int, Pure](
      !.tailcall(wrun(n - 1)).at[W].flatMap(x => Writer.tell(1).at[W].map(_ => x + 1))).map(_._2)

  test("Writer.run (loopWith)") { assertEquals(!.run(wrun(n)), n) }

  /** a WALKER per level re-tells every tell below it: n levels are n²/2 re-tells, inherently (the fold's too),
   * so these two run 2 000 deep: 5 000 (12.5 M re-tells) timed out at 30 s on Scala.js on a loaded box in a full
   * gate. Their depth is the 100 000 tests' job; these check the walkers' answers on the frames */
  val m = 2000

  def wmap(n: Int): Int ! W =
    if n == 0 then pure(0)
    else Writer.map[Int, Int, Int, Pure](
      !.tailcall(wmap(n - 1)).flatMap(x => Writer.tell(1).at[W].map(_ => x + 1)))(_ * 2)

  test("Writer.map") {
    val (told, v) = !.run(Writer.run[Int, Int, Pure](wmap(m)))
    assertEquals(v, m)
    assertEquals(told.size, m)
  }

  def wlisten(n: Int): Int ! W =
    if n == 0 then pure(0)
    else Writer.listen[Int, Int, Pure](
      !.tailcall(wlisten(n - 1)).flatMap(x => Writer.tell(1).at[W].map(_ => x + 1))).map(_._1)

  test("Writer.listen") {
    assertEquals(!.run(Writer.run[Int, Int, Pure](wlisten(m)))._2, m)
  }

  // ---- the state-threading loops: a handler per level, n levels

  type SL = Supply % Long + Pure
  def supplied(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Supply.run[Long](0L)(_ + 1)[Int, Pure](
      !.tailcall(supplied(n - 1)).at[SL].flatMap(x => Supply.next[Long].at[SL].map(_ => x + 1))).map(_._2)

  test("Supply.run") { assertEquals(!.run(supplied(n)), n) }

  type RF = Refs + Pure
  def reffed(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Refs.handle[Int, Pure](
      !.tailcall(reffed(n - 1)).at[RF].flatMap(x => Refs.ref(1).at[RF].flatMap(r => Refs.read(r).at[RF]).map(_ => x + 1)))

  test("Refs.handle") { assertEquals(!.run(reffed(n)), n) }

  type OF = Once + Pure
  def onced(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Once.run[Int, Pure](
      !.tailcall(onced(n - 1)).at[OF].flatMap(x => Once.once[Int, Pure](pure(1)).map(_ => x + 1)))

  test("Once.run") { assertEquals(!.run(onced(n)), n) }

  type CF = Chronicle % String + Pure
  def chronicled(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Chronicle.run[String, Int, Pure](
      !.tailcall(chronicled(n - 1)).at[CF].flatMap(x => Chronicle.dictate("w").at[CF].map(_ => x + 1))).map {
        case Chronicle.Verdict.Warned(a, _) => a
        case Chronicle.Verdict.Clean(a) => a
        case _ => -1 }

  test("Chronicle.run") { assertEquals(!.run(chronicled(n)), n) }

  type PF = Produce + Pure
  def generated(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Producer.fold[Int, Int, Int, Pure](
      !.widen[Int, Pure, Produce](!.tailcall(generated(n - 1))).flatMap(x => !.widen[Int, Produce, Pure](produce(1)).map(_ => x + 1)))(0)(_ + _).map(_._2)

  test("Producer.fold") { assertEquals(!.run(generated(n)), n) }
