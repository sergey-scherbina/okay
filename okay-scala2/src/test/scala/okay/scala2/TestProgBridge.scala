package okay.scala2

import okay.{!, Async, async}
import okay.given

/** the Scala 3 side of the seam: an `A ! Async` crosses into a Prog
 * and the program comes back out, still a program */
class TestProgBridge extends munit.FunSuite {

  test("lift an Async program, compose it as a Prog, read the program back") {
    var ran = false
    val p: Int ! Async = async { ran = true; 21 }
    val prog = Bridge.lift(p).map(_ * 2)
    assert(!ran)
    assertEquals(prog.run(), 42)
    assert(ran)
    assertEquals(prog.attempt.run(), Right(42))
    assertEquals(okay.runEither(Bridge.program(prog)).runWith, Right(42))
  }
}
