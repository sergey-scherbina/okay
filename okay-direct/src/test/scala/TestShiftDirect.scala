package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.Direct.*
// the direct-style `shift` and `reset`, by name: a named import outranks the classic's wildcard
import okay.Direct.{shift, reset}

/** specs/shift-effect.md: `shift`/`reset` in direct style, with the existing `direct` and no form of their own */
class TestShiftDirect extends munit.FunSuite:

  type P = okay.freer.Pure
  type S = State % Int

  test("reset over a direct block, shift's body a direct block") {
    val q: Int ! S = reset[Int, S](direct {
      val x = shift[Int, Int, S](k => direct { k(1).? + k(10).? }).?
      x * 2 + State.get[Int].?
    })
    // k(1): 2 + 5; k(10): 20 + 5
    assertEquals(State.run(5)(q), (5, 32))
  }

  test("two shifts in sequence and one inside another's body") {
    val p: Int ! P = reset[Int, P](direct {
      val x = shift[Int, Int, P](k => direct { k(1).? + k(10).? }).?
      val y = shift[Int, Int, P](k => direct { k(2).? + k(3).? }).?
      x * y
    })
    assertEquals(!.run(p), 55)
    val q: Int ! P = reset[Int, P](direct {
      val x = shift[Int, Int, P] { k =>
        direct {
          val y = shift[Int, Int, P](k2 => direct { k2(k(5).?).? * 2 }).?
          y + 1
        }
      }.?
      x * 3
    })
    assertEquals(!.run(q), 32)
  }

  test("no marks at all: auto-colouring makes the captures values") {
    import scala.language.implicitConversions
    val q: Int ! S = reset[Int, S](direct {
      val x: Int = shift[Int, Int, S](k => direct { (k(1): Int) + (k(10): Int) })
      x * 2 + (State.get[Int]: Int)
    })
    assertEquals(State.run(5)(q), (5, 32))
  }

  test("the short form in a direct block: shift[A], reset's types from the expected type") {
    val q: Int ! S = reset(direct {
      val x = shift[Int](k => direct { k(1).? + k(10).? }).?
      x * 2 + State.get[Int].?
    })
    assertEquals(State.run(5)(q), (5, 32))
  }
