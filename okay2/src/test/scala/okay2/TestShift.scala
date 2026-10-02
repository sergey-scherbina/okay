package okay2

/** specs/shift-effect.md: `Shift[R]`, the continuation as an effect, on Delim's machine — the Scala 3 core's
 * TestShift, with the same expected values */
class TestShift extends munit.FunSuite {

  type P = Pure
  type S = State[Int]

  test("laws: reset(pure(v)) is v; reset(shift(k => k(v))) is v; dropping k aborts") {
    assertEquals(!.run(reset[Int, P](pure[Shift[Int], Int](7))), 7)
    assertEquals(!.run(reset[Int, P](shift0[Int, Int, P](k => k(7)))), 7)
    assertEquals(!.run(reset[Int, P](shift[Int, Int, P](k => k(7)))), 7)
    var reached = false
    val r = reset[Int, P](shift0[Int, Int, P](_ => pure[P, Int](42)).map { x => reached = true; x + 1 })
    assertEquals(!.run(r), 42)
    assert(!reached, "the dropped continuation ran")
  }

  test("Danvy-Filinski: reset { shift(k => k(1) + k(10)) * 2 } == 22") {
    val p: Int ! Shift[Int] = shift[Int, Int, P](k => for { a <- k(1); b <- k(10) } yield a + b).map(_ * 2)
    assertEquals(!.run(reset[Int, P](p)), 22)
  }

  test("State after the capture: k runs the rest twice, the state threads through both") {
    val q: Int ! (Shift[Int] + S) =
      for {
        x <- shift0[Int, Int, S](k => for { a <- k(1); b <- k(10) } yield a + b)
        s <- State.get[Int].plus[Shift[Int]]
        _ <- State.set(s + 1).plus[Shift[Int]]
      } yield x * 2 + s
    assertEquals(State.run(5)(reset[Int, S](q)), (7, 33))
  }

  test("multi-shot with Choose handled outside: every branch, in order") {
    val q: Int ! (Shift[Int] + Choose) =
      for {
        x <- shift0[Int, Int, Choose](k => for { a <- k(1); b <- k(2) } yield a + b)
        c <- choose(10, 20).plus[Shift[Int]]
      } yield x + c
    assertEquals(!.run(runChoice[Int, Pure](reset[Int, Choose](q))), Seq(23, 33, 33, 43))
  }

  test("two shifts in sequence: Danvy-Filinski's 55, and 75 with State") {
    val p: Int ! Shift[Int] =
      for {
        x <- shift[Int, Int, P](k => for { a <- k(1); b <- k(10) } yield a + b)
        y <- shift[Int, Int, P](k => for { c <- k(2); d <- k(3) } yield c + d)
      } yield x * y
    assertEquals(!.run(reset[Int, P](p)), 55)
    val q: Int ! (Shift[Int] + S) =
      for {
        x <- shift[Int, Int, S](k => for { a <- k(1); b <- k(10) } yield a + b)
        y <- shift[Int, Int, S](k => for { c <- k(2); d <- k(3) } yield c + d)
        s <- State.get[Int].plus[Shift[Int]]
      } yield x * y + s
    assertEquals(State.run(5)(reset[Int, S](q)), (5, 75))
  }

  test("shift's body runs under its reset: it captures to the same reset again") {
    val p: Int ! Shift[Int] =
      for {
        x <- shift[Int, Int, P](k =>
          for { y <- shift[Int, Int, P](k2 => for { a <- k(5); b <- k2(a) } yield b * 2) } yield y + 1)
      } yield x * 3
    assertEquals(!.run(reset[Int, P](p)), 32)
  }

  test("nested resets, the same answer type: the innermost answers") {
    val inner: Int ! P = reset[Int, P](shift0[Int, Int, P](_ => pure[P, Int](1)).map(_ + 1000))
    val outer: Int ! Shift[Int] = inner.plus[Shift[Int]].map(_ + 10)
    assertEquals(!.run(reset[Int, P](outer)), 11)
  }

  test("an outer k called inside an inner reset's body") {
    val p: Int ! Shift[Int] =
      shift0[Int, Int, P](k => reset[Int, P](k(1).plus[Shift[Int]].map(_ + 100))).map(_ * 2)
    assertEquals(!.run(reset[Int, P](p)), 102)
  }

  test("different answer types in one program: each reset takes its own, a capture crosses the other") {
    val inner: String ! Shift[Int] =
      reset[String, Shift[Int]](
        for {
          a <- shift0[String, Int, Shift[Int]](k => k(2).map(_ + "!"))
          b <- shift0[Int, Int, Shift[String]](k => k(a * 10).map(_ + 1))
        } yield "x" * b)
    assertEquals(!.run(reset[Int, P](inner.map(_.length))), 22)
  }

  test("keys: an abstract answer type has none; one type, one key, through an alias and an intersection's order") {
    assert(compileErrors("def f[R]: Shift.Key[R] = implicitly[Shift.Key[R]]").nonEmpty)
    type Name = String
    assert(implicitly[Shift.Key[Name]] eq implicitly[Shift.Key[String]])
    assert(implicitly[Shift.Key[Serializable with Comparable[String]]] eq implicitly[Shift.Key[Comparable[String] with Serializable]])
    assert(implicitly[Shift.Key[Int]] ne implicitly[Shift.Key[Long]])
    assert(implicitly[Shift.Key[List[Int]]] ne implicitly[Shift.Key[List[Long]]])
  }

  test("nesting: a row holding Shift or Delim is inner; a plain or abstract one is not") {
    assert(implicitly[Shift.Nesting[Shift[Int]]].inner)
    assert(implicitly[Shift.Nesting[Shift[Int] + S]].inner)
    assert(implicitly[Shift.Nesting[Delim + S]].inner)
    assert(implicitly[Shift.Nesting[Delim + Shift[Int]]].inner)
    assert(!implicitly[Shift.Nesting[S]].inner)
    assert(!implicitly[Shift.Nesting[P]].inner)
    def generic[F <: Row]: Boolean = implicitly[Shift.Nesting[F]].inner
    assert(!generic[Shift[Int]])
  }

  test("a reset inside Delim's own reset block runs on Delim's machine") {
    val r: Int ! P = Delim.reset[Int, P] { p =>
      for {
        n <- reset[Int, Delim](shift0[Int, Int, Delim](k => k(4).map(_ * 10)).map(_ + 1))
        m <- Delim.shift[Int, Int, P](p)(k => k(n).map(_ + 1))
      } yield m
    }
    assertEquals(!.run(r), 51)
  }

  test("depth: 100 000 captures in sequence, both shift and shift0") {
    def loop0(n: Int): Int ! Shift[Int] =
      if (n == 0) pure[Shift[Int], Int](0)
      else shift0[Int, Int, P](k => k(1)).flatMap(x => loop0(n - 1).map(_ + x))
    def loop(n: Int): Int ! Shift[Int] =
      if (n == 0) pure[Shift[Int], Int](0)
      else shift[Int, Int, P](k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    assertEquals(!.run(reset[Int, P](loop0(100000))), 100000)
    assertEquals(!.run(reset[Int, P](loop(100000))), 100000)
  }

  test("depth: 100 000 nested resets of one answer type, past the stack's room") {
    def nest(n: Int): Int ! Pure =
      if (n == 0) pure[Pure, Int](0)
      else reset[Int, Pure](pure[Shift[Int], Unit](()).flatMap(_ => nest(n - 1).plus[Shift[Int]])
        .flatMap(x => shift0[Int, Int, Pure](k => k(x + 1))))
    assertEquals(!.run(nest(100000)), 100000)
  }

  test("named patterns: exit leaves its reset, collect answers what emit handed out") {
    val e: Int ! Shift[Int] = Shift.exit[Int, Int, P](7).map(_ + 1000)
    assertEquals(!.run(reset[Int, P](e)), 7)
    val g: Unit ! Shift[List[Int]] =
      for { _ <- Shift.emit[Int, P](1); _ <- Shift.emit[Int, P](2); _ <- Shift.emit[Int, P](3) } yield ()
    assertEquals(!.run(Shift.collect[Int, P](g)), List(1, 2, 3))
  }

  test("level 2: cont and embed round-trip on the diagonal") {
    val q: Int ! (Shift[Int] + S) =
      for {
        x <- shift0[Int, Int, S](k => for { a <- k(1); b <- k(10) } yield a + b)
        s <- State.get[Int].plus[Shift[Int]]
      } yield x * 2 + s
    val viaCont: Int ! S = Shift.cont[Int, Int, S](q) / (pure[S, Int](_))
    assertEquals(State.run(5)(viaCont), State.run(5)(reset[Int, S](q)))
    val back: Int ! S = reset[Int, S](Shift.embed[Int, Int, S](Shift.cont[Int, Int, S](q)))
    assertEquals(State.run(5)(back), State.run(5)(reset[Int, S](q)))
  }
}
