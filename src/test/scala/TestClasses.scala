package okay

import !.*

/** The typeclass hierarchy earning its keep: one generic combinator,
 * many carriers — programs, LazyList, Choose searches. */
class TestClasses extends munit.FunSuite {

  type F = Writer % String
  type PF[A] = A ! F

  test("traverse: effects in order, results collected — over programs") {
    val prog: Seq[Int] ! F =
      traverse(Seq(1, 2, 3))(x =>
        effect[F, String](Writer(s"at $x")).map(_ => x * 10))
    val (told, got) = !.run(Writer.run[String, Seq[Int], okay.Pure](prog))
    assertEquals(got, Seq(10, 20, 30))
    assertEquals(told, Seq("at 1", "at 2", "at 3"))
  }

  test("sequence and replicateA over the State effect") {
    def bump: Int ! State % Int =
      State.get[Int].flatMap(x => State.set(x + 1).map(_ => x))
    val (s, xs) = State.run(0)(replicateA[[A] =>> A ! State % Int, Int](4)(bump))
    assertEquals(xs, Seq(0, 1, 2, 3))
    assertEquals(s, 4)
  }

  test("the same traverse runs over LazyList's MonadPlus") {
    assertEquals(
      traverse[LazyList, Int, Int](Seq(1, 2))(x => LazyList(x, x + 10)).toList,
      List(Seq(1, 2), Seq(1, 12), Seq(11, 2), Seq(11, 12)))
  }

  test("*> and <* sequence and pick a side") {
    def say(s: String): PF[String] = effect[F, String](Writer(s))
    val picked: PF[String] = say("a") *> say("b") <* say("c")
    val (told, kept) = !.run(Writer.run[String, String, okay.Pure](picked))
    assertEquals(kept, "b")
    assertEquals(told, Seq("a", "b", "c"))
  }

  test("whenS and unlessS: the branch is declared, run at most once") {
    def tell(s: String): PF[Unit] = effect[F, String](Writer(s)).map(_ => ())
    val prog: PF[Unit] =
      whenS(pure(true): PF[Boolean])(tell("yes")).flatMap(_ =>
        whenS(pure(false): PF[Boolean])(tell("no")).flatMap(_ =>
          unlessS(pure(false): PF[Boolean])(tell("fallback"))))
    val (told, _) = !.run(Writer.run[String, Unit, okay.Pure](prog))
    assertEquals(told, Seq("yes", "fallback"))
  }

  test("guard on LazyList prunes like a comprehension filter") {
    val evens = LazyList.from(1).take(10).flatMap(x =>
      guard[LazyList](x % 2 == 0).map(_ => x))
    assertEquals(evens.toList, List(2, 4, 6, 8, 10))
  }
}
