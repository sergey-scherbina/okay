package okay

import !.*

class Boom extends Exception("boom")

/**
 * The lazy-construction contract, demonstrated against eager pure-bind
 * evaluation: in okay a program is a value — building it runs nothing —
 * while a library that evaluates pure binds at construction diverges,
 * throws, or spends its effects before anybody asked to run.
 */
class TestLaziness extends munit.FunSuite {

  test("an infinite program is a value here; eager construction runs it uninvited") {
    var okaySteps = 0
    def ticks: Unit ! Produce = produce(()).flatMap(_ => { okaySteps += 1; ticks })
    val t = ticks
    assertEquals(okaySteps, 0)             // built: nothing ran
    t.next(1000): Unit
    assertEquals(okaySteps, 1000)          // stepped exactly as asked

    import _root_.kyo.*
    var kyoSteps = 0
    def forever: Unit < Any = ((): Unit < Any).flatMap(_ => { kyoSteps += 1; forever })
    val _ = forever                                // merely building it...
    assert(kyoSteps > 0)                   // ...already ran a chunk of the loop
    println(s"kyo ran $kyoSteps iterations at construction")
  }

  test("no exception until run here; eager construction throws at build") {
    val p = pure[Produce, Int](1).flatMap(x => if x > 0 then throw Boom() else produce(x))
    val _ = intercept[Boom](p.runWith)             // built fine, throws only when run

    import _root_.kyo.*
    intercept[Boom]((1: Int < Any).flatMap((_: Int) => (throw Boom()): Int))
  }

  test("effects happen per run here; eagerly they happen once, at build") {
    var n = 0
    val p = pure[Produce, Int](1).flatMap(x => { n += 1; produce(x) })
    assertEquals(n, 0)                     // building ran nothing
    val _ = p.runWith
    val _ = p.runWith
    assertEquals(n, 2)                     // once per run: the value is reusable

    import _root_.kyo.*
    var m = 0
    val k = (1: Int < Any).flatMap((x: Int) => { m += 1; x })
    assertEquals(m, 1)                     // already ran, at construction
    val _ = k.eval
    val _ = k.eval
    assertEquals(m, 1)                     // and never again
  }

}
