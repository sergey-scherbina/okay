package okay.freer


import okay.freer.Row.*

/** shift-in-scope: inside a `reset` block a `shift` names only its value type */
class TestShiftIn extends munit.FunSuite:

  type S = State % Int

  test("reset { shift[A](…) }: R and F from the block, the block's from the expected type") {
    val q: Int ! S = reset {
      for
        x <- shift[Int](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
      yield x * 2 + s
    }
    assertEquals(q.handle(State(5)).run, (5, 32))
  }

  test("no effect: the expected type alone") {
    val p: Int ! Pure = reset(shift[Int](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2))
    assertEquals(p.run, 22)
    val early: Int ! Pure = reset(shift0[Int](_ => pure(42)).map(_ + 1))
    assertEquals(early.run, 42)
  }

  test("a value type other than the answer") {
    val p: String ! Pure = reset(shift[Int](k => k(3)).map(n => "x" * n))
    assertEquals(p.run, "xxx")
  }

  test("nested: the innermost block's reset; the full form reaches past it") {
    val p: Int ! Pure = reset {
      reset[String, Shift % Int] {
        for
          a <- shift0[Int](k => k(2).map(_ + "!"))
          b <- shift0[Int, Int, Shift % String](k => k(a * 10).map(_ + 1))
        yield "x" * b
      }.map(_.length)
    }
    assertEquals(p.run, 22)
  }

  test("outside a block the short form is refused, and says why") {
    val errs = compileErrors("val p = shift[Int](k => k(1))")
    assert(errs.contains("no reset around this shift"), errs)
  }

  test("a program built elsewhere still passes to reset as it is") {
    val built: Int ! Shift % Int = shift[Int, Int, Pure](k => k(7))
    assertEquals(reset[Int, Pure](built).run, 7)
  }
