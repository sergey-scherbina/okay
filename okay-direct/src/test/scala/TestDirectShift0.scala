package okay


import okay.freer.*


import okay.freer.given
import okay.Direct.*
import scala.language.implicitConversions

/** dollar-doors: `!Shift.shift0[A]` inside a direct block, one type
 * argument, the mirror of the inline `shift` (delim-one-type). */
class TestDirectShift0 extends munit.FunSuite {

  test("shift0 in a direct block: the body consumes the delimiter, k re-installs it") {
    val r = Shift.delimited[Int, Pure]:
      direct:
        val x = !Shift.shift0[Int](k => k(1).flatMap(a => k(2).map(b => a + b)))
        x * 10
    // k(1) = 10, k(2) = 20, the body answers 30 under the empty stack
    assertEquals(!.run(r), 30)
  }

  test("shift0 under a dollar: ret rides with k, once per resumption") {
    val r = !.run(Shift.run[String, Pure](
      Shift.dollar[Int, String, Pure](i => okay.freer.pure(s"n=$i")):
        direct:
          val x = !Shift.shift0[Int](k => k(1).flatMap(a => k(2).map(b => s"$a|$b")))
          x * 10
    ))
    assertEquals(r, "n=10|n=20")
  }
}
