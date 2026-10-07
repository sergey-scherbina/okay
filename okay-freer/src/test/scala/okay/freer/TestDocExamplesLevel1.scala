package okay.freer


import okay.freer.Row.*

/**
 * docs/effects-and-continuations.md, VERBATIM: every example line as the page prints it, answer comment
 * included, then asserted. The direct-style section is okay-direct's TestDocExamplesLevel1Direct.
 */
class TestDocExamplesLevel1 extends munit.FunSuite:

  test("effects: perform, handle, run") {
    val counter: Int ! State % Int =
      for
        n <- State.get[Int]
        _ <- State.set(n + 1)
      yield n * 10

    val a = counter.handle(State(5)).run   // (6, 50)

    val checked: Int ! State % Int + Throws % String =
      for
        n <- State.get[Int].plus[Throws % String]
        r <- (if n > 3 then raise[String, Int]("too big") else pure(n)).plus[State % Int]
      yield r

    val b = checked.handle(State(5)).handle(Throws.either).run   // Left("too big")
    val c = checked.handle(Throws.either).handle(State(1)).run   // (1, Right(1))
    assertEquals(a, (6, 50))
    assertEquals(b, Left("too big"))
    assertEquals(c, (1, Right(1)))
  }

  test("tutorial §1, in level 1's words") {
    val prog: Int ! State % Int =
      for
        x <- State.get[Int]
        _ <- State.set(x + 40)
        y <- State.get[Int]
      yield y + 2

    val ran = prog.handle(State(0)).run   // (40, 42) — the final state and the answer

    type F = State % Int + Throws % String
    def risky(n: Int): Int ! F =
      if n < 0 then effect(Throws("negative")) else effect(State.Update[Int, Int](_ => (n, n)))

    val both = risky(5).handle(State(0)).handle(Throws.either).run   // handle State, then Throws: Right((5, 5))
    assertEquals(ran, (40, 42))
    assertEquals(both, Right((5, 5)))
  }

  test("perform: an operation as a program, either spelling") {
    val asked: Int ! State % Int = perform(State.Get[Int, Int]())
    val same: Int ! State % Int = State.Get[Int, Int]().perform
    assertEquals(asked.handle(State(3)).run, (3, 3))
    assertEquals(same.handle(State(3)).run, (3, 3))
  }

  test("continuations: shift and reset") {
    val twice: Int ! Pure = reset {
      shift[Int](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2)
    }

    val d = twice.run   // 22: k(1) is 2, k(10) is 20

    val built: Int ! Shift % Int = shift[Int, Int, Pure](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2)
    val e = built.handle(Reset[Int]).run   // 22, the same: reset is a handler
    assertEquals(d, 22)
    assertEquals(e, 22)
  }

  test("continuations with effects") {
    val both: Int ! State % Int = reset {
      for
        x <- shift[Int](k => for a <- k(1); b <- k(10) yield a + b)
        s <- State.get[Int].plus[Shift % Int]
        _ <- State.set(s + 1).plus[Shift % Int]
      yield x * 2 + s
    }

    val f = both.handle(State(5)).run   // (7, 33): the rest ran twice, the state through both
    assertEquals(f, (7, 33))
  }

  test("an early exit, and two answer types in one program") {
    val early: Int ! Pure = reset(shift0[Int](_ => pure(42)).map(_ + 1))
    val g = early.run   // 42: the continuation was dropped

    val crossing: String ! Shift % Int =
      reset[String, Shift % Int](
        for
          a <- shift0[String, Int, Shift % Int](k => k(2).map(_ + "!"))
          b <- shift0[Int, Int, Shift % String](k => k(a * 10).map(_ + 1))
        yield "x" * b)

    val h = crossing.map(_.length).handle(Reset[Int]).run   // 22: the Int capture crossed the String reset
    assertEquals(g, 42)
    assertEquals(h, 22)
  }

  test("the named patterns: exit, and a generator") {
    import okay.freer.Shift.{collect, emit, exit}
    def firstOver(limit: Int, xs: List[Int]): Int ! Pure = reset {
      !.foldM(xs)(0)((_, x) => if x > limit then exit[Int](x) else pure[Shift % Int, Int](x)).map(_ => -1)
    }

    val found = firstOver(10, List(3, 12, 40)).run   // 12: the rest is never looked at

    def evens(n: Int)(using Shift.Emitting.Aux[Int, Pure]): Unit ! Shift % ? =
      if n == 0 then pure(()) else (if n % 2 == 0 then emit(n) else pure[Shift % ?, Unit](())).flatMap(_ => evens(n - 1))

    val listed = collect(evens(6)).run   // List(6, 4, 2)
    assertEquals(found, 12)
    assertEquals(listed, List(6, 4, 2))
  }

  test("through the typeclass") {
    def program[M[_[+_], _]](using E: Classic[M]): M[State % Int, Int] =
      E.reset[Int, State % Int](
        E.shift[Int, Int, State % Int](k => k(1).flatMap(a => k(10).map(b => a + b))).flatMap(x =>
          E.perform[Shift % Int + State % Int, Int](State.Get[Int, Int]()).map(s => x * 2 + s)))

    val inFree = summon[Classic[Free]].run(summon[Classic[Free]].handle(program[Free], State(5)))      // (5, 32)
    val inEager = summon[Classic[Eager]].run(summon[Classic[Eager]].handle(program[Eager], State(5)))  // (5, 32)
    assertEquals(inFree, (5, 32))
    assertEquals(inEager, (5, 32))
  }

  test("docs/modules/okay-freer.md: using it") {
    val p: Int ! State % Int = State.get[Int].map(_ + 1)
    val j = p.handle(State(41)).run   // (41, 42)
    assertEquals(j, (41, 42))
  }
