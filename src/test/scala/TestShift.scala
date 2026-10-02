package okay

import okay.Row.*

/** specs/shift-effect.md, specs/shift-merge.md: `Shift % R`, the continuation as an effect, on the one machine */
class TestShift extends munit.FunSuite:

  type P = okay.Pure
  type S = State % Int

  test("laws: reset(pure(v)) is v; reset(shift(k => k(v))) is v; dropping k aborts") {
    assertEquals(!.run(reset[Int, P](pure(7))), 7)
    assertEquals(!.run(reset[Int, P](shift0[Int, Int, P](k => k(7)))), 7)
    assertEquals(!.run(reset[Int, P](shift[Int, Int, P](k => k(7)))), 7)
    var reached = false
    val r = reset[Int, P](shift0[Int, Int, P](_ => pure(42)).map { x => reached = true; x + 1 })
    assertEquals(!.run(r), 42)
    assert(!reached, "the dropped continuation ran")
  }

  test("Danvy-Filinski: reset { shift(k => k(1) + k(10)) * 2 } == 22") {
    val p: Int ! Shift % Int = shift[Int, Int, P](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2)
    assertEquals(!.run(reset[Int, P](p)), 22)
  }

  test("State after the capture: k runs the rest twice, the state threads through both") {
    val q: Int ! Shift % Int + S =
      for
        x <- shift0[Int, Int, S](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
        _ <- State.set(s + 1).plus[Shift % Int]
      yield x * 2 + s
    assertEquals(State.run(5)(reset[Int, S](q)), (7, 33))
  }

  test("multi-shot with Choose handled outside: every branch, in order") {
    val q: Int ! Shift % Int + Choose =
      for
        x <- shift0[Int, Int, Choose](k => for a <- k(1); b <- k(2) yield a + b)
        c <- choose(10, 20).plus[Shift % Int]
      yield x + c
    assertEquals(!.run(runChoice(reset[Int, Choose](q))), Seq(23, 33, 33, 43))
  }

  test("two shifts in sequence: Danvy-Filinski's 55, and 75 with State") {
    val p: Int ! Shift % Int =
      for
        x <- shift[Int, Int, P](k => for a <- k(1); b <- k(10) yield a + b)
        y <- shift[Int, Int, P](k => for c <- k(2); d <- k(3) yield c + d)
      yield x * y
    assertEquals(!.run(reset[Int, P](p)), 55)
    val q: Int ! Shift % Int + S =
      for
        x <- shift[Int, Int, S](k => for a <- k(1); b <- k(10) yield a + b)
        y <- shift[Int, Int, S](k => for c <- k(2); d <- k(3) yield c + d)
        s <- State.get[Int].plus[Shift % Int]
      yield x * y + s
    assertEquals(State.run(5)(reset[Int, S](q)), (5, 75))
  }

  test("shift's body runs under its reset: it captures to the same reset again") {
    val p: Int ! Shift % Int =
      for
        x <- shift[Int, Int, P](k =>
          for y <- shift[Int, Int, P](k2 => for a <- k(5); b <- k2(a) yield b * 2)
          yield y + 1)
      yield x * 3
    assertEquals(!.run(reset[Int, P](p)), 32)
  }

  test("nested resets, the same answer type: the innermost answers") {
    val inner: Int ! P = reset[Int, P](shift0[Int, Int, P](_ => pure(1)).map(_ + 1000))
    val outer: Int ! Shift % Int = inner.plus[Shift % Int].map(_ + 10)
    assertEquals(!.run(reset[Int, P](outer)), 11)
  }

  test("an outer k called inside an inner reset's body") {
    val p: Int ! Shift % Int =
      shift0[Int, Int, P](k => reset[Int, P](k(1).plus[Shift % Int].map(_ + 100))).map(_ * 2)
    assertEquals(!.run(reset[Int, P](p)), 102)
  }

  test("different answer types in one program: each reset takes its own, a capture crosses the other") {
    val inner: String ! Shift % Int =
      reset[String, Shift % Int](
        for
          a <- shift0[String, Int, Shift % Int](k => k(2).map(_ + "!"))
          b <- shift0[Int, Int, Shift % String](k => k(a * 10).map(_ + 1))
        yield "x" * b)
    assertEquals(!.run(reset[Int, P](inner.map(_.length))), 22)
  }

  test("keys: an abstract answer type has none; one type, one key, through an alias and a union's order") {
    assert(compileErrors("def f[R]: Shift.Key[R] = summon[Shift.Key[R]]").nonEmpty)
    type Name = String
    assert(summon[Shift.Key[Name]] eq summon[Shift.Key[String]])
    assert(summon[Shift.Key[Int | String]] eq summon[Shift.Key[String | Int]])
    assert(summon[Shift.Key[Int]] ne summon[Shift.Key[Long]])
  }

  test("a keyed reset inside a dynamic Shift.delimited block runs on the one machine") {
    val r: Int ! P = Shift.delimited[Int, P] {
      for
        n <- reset[Int, Shift % ?](shift0[Int, Int, Shift % ?](k => k(4).map(_ * 10)).map(_ + 1))
        m <- Shift.shift[Int, Int, P](k => k(n).map(_ + 1))
      yield m
    }
    assertEquals(!.run(r), 51)
  }

  test("depth: 100 000 captures in sequence, both shift and shift0") {
    def loop0(n: Int): Int ! Shift % Int =
      if n == 0 then pure(0)
      else shift0[Int, Int, P](k => k(1)).flatMap(x => !.tailcall(loop0(n - 1)).map(_ + x))
    def loop(n: Int): Int ! Shift % Int =
      if n == 0 then pure(0)
      else shift[Int, Int, P](k => k(1)).flatMap(x => !.tailcall(loop(n - 1)).map(_ + x))
    assertEquals(!.run(reset[Int, P](loop0(100000))), 100000)
    assertEquals(!.run(reset[Int, P](loop(100000))), 100000)
  }

  test("depth: 100 000 nested resets of one answer type, past the stack's room") {
    def nest(n: Int): Int ! Pure =
      if n == 0 then pure(0)
      else reset[Int, Pure](!.tailcall(nest(n - 1)).plus[Shift % Int].flatMap(x => shift0[Int, Int, Pure](k => k(x + 1))))
    assertEquals(!.run(nest(100000)), 100000)
  }

  test("level 2: cont and embed round-trip on the diagonal") {
    val q: Int ! Shift % Int + S =
      for
        x <- shift0[Int, Int, S](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
      yield x * 2 + s
    val viaCont: Int ! S = Shift.cont(q) / (pure(_))
    assertEquals(State.run(5)(viaCont), State.run(5)(reset[Int, S](q)))
    val back: Int ! S = reset[Int, S](Shift.embed(Shift.cont(q)))
    assertEquals(State.run(5)(back), State.run(5)(reset[Int, S](q)))
  }

  test("Shift.dynamic: a capture keyed by its answer type and one to a prompt by value, in one program") {
    // the keyed capture reaches the Int key's prompt; the scope's exit leaves the scope only
    val keyed: Int ! Shift % Int = shift[Int, Int, Pure](k => k(1).map(_ + 10))
    val mixed: Int ! Shift % ? = Shift.dynamic(keyed).flatMap(x => Shift.scope[Int, Pure](Shift.exit(x * 2)))
    val prog = Shift.push[Int, Pure](summon[Shift.Key[Int]].prompt)(mixed)
    assertEquals(Shift.run[Int, Pure](prog).run, 12)
  }
