package okay2

/**
 * The monad classes (spec stage 12): laws by running, the generic
 * combinators over programs, `withFilter` by CanFail, and the
 * instances found without an import.
 */
class TestMonad extends munit.FunSuite {

  def runCounter[A](p: A ! State[Int]): (Int, A) = State.run(0)(p)

  val tick: Int ! State[Int] = State.modify[Int](_ + 1)

  test("Monad laws for programs, by running: left and right identity, associativity") {
    val M = Monad[({ type L[A] = Free[State[Int], A] })#L]
    val f: Int => Int ! State[Int] = n => tick.map(_ * 10 + n)
    val g: Int => Int ! State[Int] = n => tick.map(_ + n)
    assertEquals(runCounter(M.flatMap(M.pure(3))(f)), runCounter(f(3)))
    assertEquals(runCounter(M.flatMap(tick)(M.pure)), runCounter(tick))
    assertEquals(
      runCounter(M.flatMap(M.flatMap(tick)(f))(g)),
      runCounter(M.flatMap(tick)(x => M.flatMap(f(x))(g))))
  }

  test("Monad laws for Option, found in Functor's companion with no import") {
    val M = Monad[Option]
    val f: Int => Option[Int] = n => if (n > 0) Some(n * 2) else None
    val g: Int => Option[Int] = n => Some(n + 1)
    assertEquals(M.flatMap(M.pure(3))(f), f(3))
    assertEquals(M.flatMap(Option(5))(M.pure), Option(5))
    assertEquals(M.flatMap(M.flatMap(Option(5))(f))(g), M.flatMap(Option(5))(x => M.flatMap(f(x))(g)))
    assertEquals(implicitly[Applicative[Option]].pure(1), Some(1))
    assertEquals(implicitly[Functor[Option]].fmap(Option(1), (_: Int) + 1), Some(2))
  }

  test("traverse over programs keeps effect order and collects the results") {
    // the generic combinators, at a program whose type is spelled with Free
    val step: Int => Free[State[Int], Int] = n => tick.map(_ + n)
    val t: Free[State[Int], Int] = tick
    assertEquals(runCounter(traverse(Seq(10, 20, 30))(step)), (3, Seq(11, 22, 33)))
    assertEquals(runCounter(sequence(Seq(t, t))), (2, Seq(1, 2)))
    assertEquals(runCounter(replicateA(3)(t)), (3, Seq(1, 2, 3)))
    assertEquals(traverse(Seq(1, 2, 3))(n => Option(n)), Some(Seq(1, 2, 3)))
    assertEquals(traverse(Seq(1, -2, 3))(n => if (n > 0) Some(n) else None), None)
    // the program-shaped ones take `A ! R` as written
    def named(n: Int): Int ! State[Int] = tick.map(_ + n)
    assertEquals(runCounter(!.traverse(Seq(10, 20))(named)), (2, Seq(11, 22)))
    assertEquals(runCounter(!.sequence(Seq(tick, tick))), (2, Seq(1, 2)))
    assertEquals(runCounter(!.replicateA(2)(tick)), (2, Seq(1, 2)))
  }

  test("THE SCALA 2 TRAP: partial unification reads `A ! R` with its parameters reversed") {
    // `tick: Int ! State[Int]` unifies with F[A] as F = [R] Int ! R, and
    // no Applicative exists for that — which is why `!.sequence` exists.
    // If a future scalac dealiases first, this fails and the note can go.
    // The message names the trap and both ways out.
    val errors = compileErrors("okay2.sequence(Seq(tick, tick))")
    assert(errors.contains("no Applicative[[R]Int ! R]"), errors)
    assert(errors.contains("parameters reversed"), errors)
    assert(errors.contains("!.sequence"), errors)
  }

  test("the syntax is there only where the instance is: *>, <*, >>=, >=>, <*>") {
    assertEquals(runCounter(tick *> tick), (2, 2))
    assertEquals(runCounter(tick <* tick), (2, 1))
    assertEquals(runCounter(tick >>= (n => tick.map(_ + n))), (2, 3))
    val k: Int => Option[Int] = n => Some(n + 1)
    assertEquals((k >=> k)(1), Some(3))
    assertEquals(Option((a: Int) => a * 2) <*> Option(21), Some(42))
    assertEquals(Option(1) *> Option(2), Some(2))
    // no Monad for a plain String, so no >>= on it
    assert(compileErrors("\"s\" >>= ((c: Char) => \"t\")").nonEmpty)
  }

  test("Selective.ifS runs ONE branch: the other is not even built") {
    var built = List.empty[String]
    def branch(name: String, v: Int): Int ! State[Int] = { built ::= name; tick.map(_ => v) }
    val cond: Boolean ! State[Int] = pure[State[Int], Boolean](true)
    val p = cond.ifS(branch("then", 1))(branch("else", 2))
    assertEquals(runCounter(p), (1, 1))
    assertEquals(built, List("then"))
    // and the class's own ifS, at Option: branch/select derived from flatMap
    built = Nil
    val S = implicitly[Selective[Option]]
    assertEquals(S.ifS(Option(false))({ built ::= "t"; Option(1) })({ built ::= "e"; Option(2) }), Some(2))
    assertEquals(built, List("e"))
    // whenS: the body runs only when the condition holds
    assertEquals(runCounter(pure[State[Int], Boolean](false).whenS(tick.map(_ => ()))), (0, ()))
    assertEquals(runCounter(pure[State[Int], Boolean](false).unlessS(tick.map(_ => ()))), (1, ()))
  }

  test("withFilter: an if and a refutable pattern PRUNE in a Choose row") {
    val p: Int ! Choose =
      for {
        x <- choose(1, 2, 3, 4)
        if x % 2 == 0
      } yield x
    assertEquals(!.run(runChoice[Int, Pure](p)), Seq(2, 4))
    val q: Int ! Choose =
      for {
        Some(x) <- choose[Option[Int]](Some(1), None, Some(3))
      } yield x
    assertEquals(!.run(runChoice[Int, Pure](q)), Seq(1, 3))
    // with State beside it: the row still searches
    val r: Int ! (Choose + State[Int]) =
      for {
        x <- choose(1, 2, 3)
        // an `if` filters the generator BEFORE it: this one's row must
        // carry Choose, and the step is in both effects
        _ <- State.modify[Int](_ + x).plus[Choose]
        if x != 2
      } yield x
    assertEquals(!.run(State.handle[Int, Seq[Int], Pure](0)(runChoice[Int, State[Int]](r))), (6, Seq(1, 3)))
  }

  test("withFilter: an if STOPS in an Abort row; runOption answers None") {
    def find(id: Int): Option[String] ! Abort = pure[Abort, Option[String]](if (id == 1) Some("ada") else None)
    def name(id: Int): String ! Abort = for { Some(n) <- find(id) } yield n
    assertEquals(!.run(Throws.runOption[String, Pure](name(1))), Some("ada"))
    assertEquals(!.run(Throws.runOption[String, Pure](name(2))), None)
    assertEquals(!.run(Throws.runOption[Unit, Pure](ensure[Abort](false))), None)
  }

  test("withFilter: in a row that can neither prune nor stop, it does not compile — and says why") {
    val errors = compileErrors("for { x <- okay2.State.get[Int] if x > 0 } yield x")
    assert(errors.contains("cannot drop a step"), errors)
  }

  test("MonadPlus for a Choose row: empty prunes, append chooses, guard prunes") {
    type C[A] = Free[Choose, A]
    val MP = MonadPlus[({ type L[A] = Free[Choose with Pure, A] })#L]
    val p: Int ! Choose = MP.append(pure[Choose, Int](1), MP.append(MP.empty[Int], pure[Choose, Int](2)))
    assertEquals(!.run(runChoice[Int, Pure](p)), Seq(1, 2))
    val q: Int ! Choose = choose(1, 2, 3, 4).flatMap(x => guard[({ type L[A] = Free[Choose with Pure, A] })#L](x > 2).map(_ => x))
    assertEquals(!.run(runChoice[Int, Pure](q)), Seq(3, 4))
    val _ = implicitly[Monad[C]]
  }

  test("ParaMonad: Control is one, and its diagonal is a Monad") {
    val P = ParaMonad[Cont.Rep]
    val c: Cont[Int, Int, Int] = P.flatMap[Int, Int, Int, Int, Int](P.pure[Int, Int](20))(x => P.pure[Int, Int](x + 1))
    assertEquals(reset(P.map[Int, Int, Int, Int](c)(_ * 2)), 42)
    val D = ParaMonad.diagonal[Cont.Rep, Int]
    assertEquals(reset(D.flatMap(D.pure(1))(x => D.pure(x + 1))), 2)
  }

  test("Comonad[Id] is found by asking for it, and only then") {
    val C = implicitly[Comonad[Id]]
    assertEquals(C.extract(5), 5)
    assertEquals(C.coflatMap(5)(_ + 1), 6)
  }
}
