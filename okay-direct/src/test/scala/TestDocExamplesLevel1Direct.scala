package okay

import okay.Direct.*

/** docs/effects-and-continuations.md, the direct-style section, VERBATIM, then asserted */
class TestDocExamplesLevel1Direct extends munit.FunSuite:

  test("direct style: marks") {
    val both: Int ! State % Int = reset[Int, State % Int](direct {
      val x = shift[Int, Int, State % Int](k => direct { k(1).? + k(10).? }).?
      x * 2 + State.get[Int].?
    })

    val i = both.handle(State(5)).run   // (5, 32)
    assertEquals(i, (5, 32))
  }

  test("direct style: no marks") {
    import scala.language.implicitConversions
    val quiet: Int ! State % Int = reset[Int, State % Int](direct {
      val x: Int = shift[Int, Int, State % Int](k => direct { (k(1): Int) + (k(10): Int) })
      x * 2 + (State.get[Int]: Int)
    })

    val j = quiet.handle(State(5)).run   // (5, 32)
    assertEquals(j, (5, 32))
  }
