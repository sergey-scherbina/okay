package okay

import okay.Direct.*
import okay.Row.*

/** specs/shift-effect.md: one suite, two implementations of `Shift % R` */
abstract class TestShiftFx(api: ShiftApi) extends munit.FunSuite:
  import api.{reset, shift}

  type P = okay.Pure
  type S = State % Int

  test("laws: reset(pure(v)) is v; reset(shift(k => k(v))) is v; dropping k aborts") {
    assertEquals(!.run(reset[Int, P](pure(7))), 7)
    assertEquals(!.run(reset[Int, P](shift[Int, Int, P](k => k(7)))), 7)
    var reached = false
    val r = reset[Int, P](shift[Int, Int, P](_ => pure(42)).map { x => reached = true; x + 1 })
    assertEquals(!.run(r), 42)
    assert(!reached, "the dropped continuation ran")
  }

  test("Danvy-Filinski, no other effect: reset { shift(k => k(1) + k(10)) * 2 } == 22") {
    val p: Int ! Shift % Int = shift[Int, Int, P](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2)
    assertEquals(!.run(reset[Int, P](p)), 22)
  }

  test("the same with State after the capture: k runs the rest twice, the state threads through both") {
    val q: Int ! Shift % Int + S =
      for
        x <- shift[Int, Int, S](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
        _ <- State.set(s + 1).plus[Shift % Int]
      yield x * 2 + s
    // k(1): 2 + 5, state 6; k(10): 20 + 6, state 7
    assertEquals(State.run(5)(reset[Int, S](q)), (7, 33))
  }

  test("multi-shot with Choose handled outside: every branch, in order") {
    val q: Int ! Shift % Int + Choose =
      for
        x <- shift[Int, Int, Choose](k => for a <- k(1); b <- k(2) yield a + b)
        c <- choose(10, 20).plus[Shift % Int]
      yield x + c
    assertEquals(!.run(runChoice(reset[Int, Choose](q))), Seq(23, 33, 33, 43))
  }

  test("nested resets, different answer types, each body in its own row") {
    val inner: String ! P = reset[String, P](shift[String, Int, P](k => k(3).map(_ * 2)).map(n => "x" * n))
    val outer: Int ! Shift % Int =
      for
        s <- inner.plus[Shift % Int]
        n <- shift[Int, Int, P](k => k(s.length).map(_ + 100))
      yield n
    // the inner k doubles "xxx" into "xxxxxx"; the outer adds 100 to its length
    assertEquals(!.run(reset[Int, P](outer)), 106)
  }

  test("a second answer type in one row is refused at compile time") {
    assert(compileErrors("""
      val api: ShiftApi = ShiftFx.Handled
      val q: Int ! Shift % Int + Shift % String = pure(1)
      api.reset[Int, Shift % String](q)
    """).nonEmpty)
  }

  test("nested resets, the same answer type: the innermost answers") {
    val inner: Int ! P = reset[Int, P](shift[Int, Int, P](_ => pure(1)).map(_ + 1000))
    val outer: Int ! Shift % Int = inner.plus[Shift % Int].map(_ + 10)
    assertEquals(!.run(reset[Int, P](outer)), 11)
  }

  test("an outer k called inside an inner reset's body") {
    val p: Int ! Shift % Int =
      shift[Int, Int, P](k => reset[Int, P](k(1).plus[Shift % Int].map(_ + 100))).map(_ * 2)
    assertEquals(!.run(reset[Int, P](p)), 102)
  }

  test("direct style: the body of shift and the block under reset are direct blocks") {
    type F[A] = A ! Shift % Int + S
    val q: Int ! Shift % Int + S = direct[F] {
      val x = shift[Int, Int, S](k => direct[[A] =>> A ! S] { k(1).? + k(10).? }).?
      x * 2 + State.get[Int].plus[Shift % Int].?
    }
    // k(1): 2 + 5; k(10): 20 + 5 — no set, the state is 5 for both
    assertEquals(State.run(5)(reset[Int, S](q)), (5, 32))
  }

  test("depth: 100 000 captures in sequence") {
    def loop(n: Int): Int ! Shift % Int =
      if n == 0 then pure(0)
      else shift[Int, Int, P](k => k(1)).flatMap(x => !.tailcall(loop(n - 1)).map(_ + x))
    assertEquals(!.run(reset[Int, P](loop(100000))), 100000)
  }

  test("depth: nested resets — how deep before the JVM stack") {
    def nest(n: Int): Int ! P =
      if n == 0 then pure(0)
      else reset[Int, P](!.tailcall(nest(n - 1)).plus[Shift % Int].flatMap(x => shift[Int, Int, P](k => k(x + 1))))
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
        x <- shift[Int, Int, S](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
      yield x * 2 + s
    val viaCont: Int ! S = ShiftFx.cont(api)(q) / (pure(_))
    assertEquals(State.run(5)(viaCont), State.run(5)(reset[Int, S](q)))
    val back: Int ! S = reset[Int, S](ShiftFx.embed(api)(ShiftFx.cont(api)(q)))
    assertEquals(State.run(5)(back), State.run(5)(reset[Int, S](q)))
  }

  test("level 2: answer-type modification with an effect in the answer (printf with Writer)") {
    type W = Writer % String
    def lift[X, Ans](m: X ! W): Cont[X, Ans ! W, Ans ! W] = okay.shift(k => m.flatMap(k))
    val int: Cont[String, String ! W, (Int => String ! W) ! W] =
      okay.shift(k => pure((n: Int) => k(n.toString)))
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

class TestShiftFxHandled extends TestShiftFx(ShiftFx.Handled)
class TestShiftFxOnDelim extends TestShiftFx(ShiftFx.OnDelim)
