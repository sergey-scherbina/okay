package okay

import okay.Direct.*
import okay.Row.*

/** specs/shift-effect.md: one suite, two implementations of `Shift % R` */
abstract class TestShiftFx(api: ShiftApi) extends munit.FunSuite:
  import api.{reset, shift, shift0}

  type P = okay.Pure
  type S = State % Int

  test("laws: reset(pure(v)) is v; reset(shift(k => k(v))) is v; dropping k aborts") {
    assertEquals(!.run(reset[Int, P](pure(7))), 7)
    assertEquals(!.run(reset[Int, P](shift0[Int, Int, P](k => k(7)))), 7)
    var reached = false
    val r = reset[Int, P](shift0[Int, Int, P](_ => pure(42)).map { x => reached = true; x + 1 })
    assertEquals(!.run(r), 42)
    assert(!reached, "the dropped continuation ran")
  }

  test("Danvy-Filinski, no other effect: reset { shift(k => k(1) + k(10)) * 2 } == 22") {
    val p: Int ! Shift % Int = shift0[Int, Int, P](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2)
    assertEquals(!.run(reset[Int, P](p)), 22)
  }

  test("the same with State after the capture: k runs the rest twice, the state threads through both") {
    val q: Int ! Shift % Int + S =
      for
        x <- shift0[Int, Int, S](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
        _ <- State.set(s + 1).plus[Shift % Int]
      yield x * 2 + s
    // k(1): 2 + 5, state 6; k(10): 20 + 6, state 7
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

  test("nested resets, different answer types, each body in its own row") {
    val inner: String ! P = reset[String, P](shift0[String, Int, P](k => k(3).map(_ * 2)).map(n => "x" * n))
    val outer: Int ! Shift % Int =
      for
        s <- inner.plus[Shift % Int]
        n <- shift0[Int, Int, P](k => k(s.length).map(_ + 100))
      yield n
    // the inner k doubles "xxx" into "xxxxxx"; the outer adds 100 to its length
    assertEquals(!.run(reset[Int, P](outer)), 106)
  }

  test("an abstract answer type has no key: refused at compile time") {
    assert(compileErrors("def f[R]: Key[R] = summon[Key[R]]").nonEmpty)
  }

  test("one type, one key: through an alias and a union in either order") {
    type Name = String
    assertEquals(summon[Key[Name]].id, summon[Key[String]].id)
    assertEquals(summon[Key[Int | String]].id, summon[Key[String | Int]].id)
    assertNotEquals(summon[Key[Int]].id, summon[Key[Long]].id)
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

  test("direct style: the body of shift and the block under reset are direct blocks") {
    type F[A] = A ! Shift % Int + S
    val q: Int ! Shift % Int + S = direct[F] {
      val x = shift0[Int, Int, S](k => direct[[A] =>> A ! S] { k(1).? + k(10).? }).?
      x * 2 + State.get[Int].plus[Shift % Int].?
    }
    // k(1): 2 + 5; k(10): 20 + 5 — no set, the state is 5 for both
    assertEquals(State.run(5)(reset[Int, S](q)), (5, 32))
  }

  test("depth: 100 000 captures in sequence") {
    def loop(n: Int): Int ! Shift % Int =
      if n == 0 then pure(0)
      else shift0[Int, Int, P](k => k(1)).flatMap(x => !.tailcall(loop(n - 1)).map(_ + x))
    assertEquals(!.run(reset[Int, P](loop(100000))), 100000)
  }

  test("depth: nested resets — how deep before the JVM stack") {
    def nest(n: Int): Int ! P =
      if n == 0 then pure(0)
      else reset[Int, P](!.tailcall(nest(n - 1)).plus[Shift % Int].flatMap(x => shift0[Int, Int, P](k => k(x + 1))))
    // each reset runs its own handler (machine) inside its parent's: JVM depth grows with nesting
    val reached = List(100, 300, 1000, 3000, 10000, 30000, 100000).takeWhile { n =>
      try !.run(nest(n)) == n catch case _: StackOverflowError => false
    }
    println(s"${getClass.getSimpleName}: nested resets reached ${reached.lastOption}")
    assert(reached.nonEmpty)
  }

  test("level 2: cont and embed round-trip on the diagonal") {
    val q: Int ! Shift % Int + S =
      for
        x <- shift0[Int, Int, S](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
      yield x * 2 + s
    val viaCont: Int ! S = ShiftFx.cont(api)(q) / (pure(_))
    assertEquals(State.run(5)(viaCont), State.run(5)(reset[Int, S](q)))
    val back: Int ! S = reset[Int, S](ShiftFx.embed(api)(ShiftFx.cont(api)(q)))
    assertEquals(State.run(5)(back), State.run(5)(reset[Int, S](q)))
  }

  test("level 2: answer-type modification with an effect in the answer (printf with Writer)") {
    type W = Writer % String
    def lift[X, Ans](m: X ! W): Cont[X, Ans ! W, Ans ! W] = okay.Cont.shift(k => m.flatMap(k))
    val int: Cont[String, String ! W, (Int => String ! W) ! W] =
      okay.Cont.shift(k => pure((n: Int) => k(n.toString)))
    val fmt: Cont[String, String ! W, (Int => String ! W) ! W] =
      for
        s <- int
        _ <- lift[Unit, String](Writer.tell(s"formatted $s"))
      yield s + " apples"
    val f: (Int => String ! W) ! W = fmt / (pure(_))
    val (log, out) = !.run(Writer.run(f.flatMap(g => g(3))))
    assertEquals(out, "3 apples")
    assertEquals(log, Seq("formatted 3"))
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
    // k = x => reset(x * 3); the inner capture's k2 = y => reset(y + 1), both under the outer body
    val p: Int ! Shift % Int =
      for
        x <- shift[Int, Int, P](k =>
          for y <- shift[Int, Int, P](k2 => for a <- k(5); b <- k2(a) yield b * 2)
          yield y + 1)
      yield x * 3
    assertEquals(!.run(reset[Int, P](p)), 32)
  }

  test("different answer types in one program: each reset takes its own, a capture crosses the other") {
    // the Int capture's k holds the String reset's remainder, so it crosses it
    val inner: String ! Shift % Int =
      reset[String, Shift % Int](
        for
          a <- shift0[String, Int, Shift % Int](k => k(2).map(_ + "!"))
          b <- shift0[Int, Int, Shift % String](k => k(a * 10).map(_ + 1))
        yield "x" * b)
    assertEquals(!.run(reset[Int, P](inner.map(_.length))), 22)
  }

  test("depth: Danvy-Filinski captures in sequence — how deep before the JVM stack") {
    def loop(n: Int): Int ! Shift % Int =
      if n == 0 then pure(0)
      else shift[Int, Int, P](k => k(1)).flatMap(x => !.tailcall(loop(n - 1)).map(_ + x))
    val reached = List(100, 1000, 10000, 100000).takeWhile { n =>
      try !.run(reset[Int, P](loop(n))) == n catch case _: StackOverflowError => false
    }
    println(s"${getClass.getSimpleName}: D-F captures in sequence reached ${reached.lastOption}")
    assert(reached.nonEmpty)
  }

  test("direct style: reset over a block, shift's body a block, shift answering the value") {
    val q: Int ! S = ShiftDirect.reset[Int, S](api) {
      val x: Int = ShiftDirect.shift[Int, Int, S](api)(k => k(1).? + k(10).?)
      x * 2 + State.get[Int].?
    }
    // k(1): 2 + 5; k(10): 20 + 5
    assertEquals(State.run(5)(q), (5, 32))
  }

  test("direct style: two shifts in sequence and one inside another's body") {
    val p: Int ! P = ShiftDirect.reset[Int, P](api) {
      val x: Int = ShiftDirect.shift[Int, Int, P](api)(k => k(1).? + k(10).?)
      val y: Int = ShiftDirect.shift[Int, Int, P](api)(k => k(2).? + k(3).?)
      x * y
    }
    assertEquals(!.run(p), 55)
    val q: Int ! P = ShiftDirect.reset[Int, P](api) {
      val x: Int = ShiftDirect.shift[Int, Int, P](api) { k =>
        val y: Int = ShiftDirect.shift[Int, Int, P](api)(k2 => k2(k(5).?).? * 2)
        y + 1
      }
      x * 3
    }
    assertEquals(!.run(q), 32)
  }

class TestShiftFxHandled extends TestShiftFx(ShiftFx.Handled)
class TestShiftFxOnDelim extends TestShiftFx(ShiftFx.OnDelim)
