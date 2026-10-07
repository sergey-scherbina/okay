package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.std.given
import okay.Direct.*
// the direct-style `shift`, by name: a named import outranks the classic's wildcard
import okay.Direct.shift
import okay.freer.Row.at

/** specs/direct-reset.md: `Direct.reset` / `Direct.shift`, a direct-style or a monadic body, no `direct` */
class TestDirectReset extends munit.FunSuite:

  type P = okay.freer.Pure
  type S = State % Int

  test("a direct-style body, marks only: no `direct` around reset or shift") {
    val q: Int ! S = Direct.reset[Int, S] {
      val x = Direct.shift[Int](k => k(1).? + k(10).?).?
      x * 2 + State.get[Int].?
    }
    // k(1): 2 + 5; k(10): 20 + 5 — TestShiftDirect's answer, with direct written there
    assertEquals(State.run(5)(q), (5, 32))
  }

  test("a monadic body, the same door: a for over the core's shift") {
    val q: Int ! S = Direct.reset[Int, S](
      for
        x <- shift[Int, Int, S](k => k(1).flatMap(a => k(10).map(_ + a)))
        s <- State.get[Int].at[Shift % Int + S]
      yield x * 2 + s)
    assertEquals(State.run(5)(q), (5, 32))
  }

  test("Direct.shift with a monadic lambda body inside a direct-style reset") {
    val p: Int ! P = Direct.reset[Int, P] {
      val x = Direct.shift[Int](k => k(1).flatMap(a => k(10).map(_ + a))).?
      x + 1
    }
    assertEquals(!.run(p), 13)
  }

  test("the block's evidence is there: Shift.exit leaves a Direct.reset body") {
    val p: Int ! P = Direct.reset[Int, P] {
      val xs = List(1, 2, 3).map(n => if n == 2 then Shift.exit(n * 100).? else n)
      xs.sum
    }
    assertEquals(!.run(p), 200)
  }

  test("nested: a shift's direct body shifting again") {
    val q: Int ! P = Direct.reset[Int, P] {
      val x = Direct.shift[Int] { k =>
        val y = Direct.shift[Int](k2 => k2(k(5).?).? * 2).?
        y + 1
      }.?
      x * 3
    }
    // TestShiftDirect's 32, without the two `direct`s
    assertEquals(!.run(q), 32)
  }

  test("no marks at all, with implicitConversions: auto-colouring as in any block") {
    import scala.language.implicitConversions
    val q: Int ! S = Direct.reset[Int, S] {
      val x: Int = Direct.shift[Int](k => (k(1): Int) + (k(10): Int))
      x * 2 + (State.get[Int]: Int)
    }
    assertEquals(State.run(5)(q), (5, 32))
  }
