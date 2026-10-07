package okay


import okay.freer.*
import okay.freer.given
import okay.Direct.*
// the direct-style `shift` and `reset`, by name: a named import outranks the classic's wildcard
import okay.Direct.{shift, reset}

/** docs/effects-and-continuations.md, the direct-style section, VERBATIM, then asserted */
class TestDocExamplesLevel1Direct extends munit.FunSuite:

  test("direct style: marks") {
    val both: Int ! State % Int = reset(direct {
      val x = shift[Int](k => direct { k(1).? + k(10).? }).?
      x * 2 + State.get[Int].?
    })

    val i = both.handle(State(5)).run   // (5, 32)
    assertEquals(i, (5, 32))
  }

  test("direct style: no marks") {
    import scala.language.implicitConversions
    val quiet: Int ! State % Int = reset(direct {
      val x: Int = shift[Int](k => direct { (k(1): Int) + (k(10): Int) })
      x * 2 + (State.get[Int]: Int)
    })

    val j = quiet.handle(State(5)).run   // (5, 32)
    assertEquals(j, (5, 32))
  }
