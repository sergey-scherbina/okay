package okay

import Bisim.{Answers, Verdict}
import okay.Row.at

/**
 * specs/shift0-dollar.md STAGE 3: the correspondence between effect
 * handlers and delimited control (Piróg, Polesiuk & Sieczkowski,
 * "Typed equivalence of effect handlers and delimited control", FSCD
 * 2019), made executable on the State handler.
 *
 *   deep handler     = ret $ body, an operation = shift0 to its prompt
 *                      (k carries the handler back with it)
 *   shallow handler  = an operation = control0 (k is BARE, so the
 *                      clause re-installs the handler around k)
 *
 * Both are checked against `State.handle` with `Bisim.check` on the
 * residual Writer row, which is the only thing a caller can observe.
 */
class TestHandlersAsDollar extends munit.FunSuite:

  type W = Writer % String
  type Row = State % Int + W

  given Answers[W] = Answers.writer[String]

  /** rewrite every State operation with `op`, forwarding the rest */
  def rewrite[A](op: [X] => State[Int, X] => X ! Delim + W)(prog: A ! Row): A ! Delim + W =
    def step[X](e: Row[X], k: X => A ! Row): A ! Delim + W =
      split[State % Int, W](e)(s => op(s).flatMap(x => rewrite(op)(k(x))))(g =>
        effect[Delim + W, X](g).flatMap(x => rewrite(op)(k(x))))
    (prog.resume: @unchecked) match
      case Free.Return(a) => okay.pure(a)
      case Free.Inject(e) => step[A](e, okay.pure)
      case Free.Bind(Free.Inject(e), k) => step(e, k)

  /** DEEP: the answer is a state-passing function; `ret` is the return
   * clause, and each operation's clause gets k with the handler in it */
  def deep[A](s0: Int)(prog: A ! Row): (Int, A) ! W =
    type Ans = Int => (Int, A) ! Delim + W
    val p = Delim.prompt[Ans]
    val op = [X] => (e: State[Int, X]) => (e match
      case State.Get() => Delim.shift0[Ans, Int, W](p)(k => okay.pure((s: Int) => k(s).flatMap(f => f(s))))
      case State.Set(s1) => Delim.shift0[Ans, Int, W](p)(k => okay.pure((_: Int) => k(s1).flatMap(f => f(s1))))
    ): X ! Delim + W
    val ret: A => Ans ! Delim + W = a => okay.pure((s: Int) => okay.pure((s, a)))
    Delim.run[(Int, A), W](Delim.dollar[A, Ans, W](p)(ret)(rewrite(op)(prog)).flatMap(f => f(s0)))

  /** SHALLOW: k is bare, so the clause re-installs the handler around
   * k's result. The return clause rides INSIDE the delimiter as a map,
   * so the bare segment still answers the handler's type */
  def shallow[A](s0: Int)(prog: A ! Row): (Int, A) ! W =
    type Ans = Int => (Int, A) ! Delim + W
    val p = Delim.prompt[Ans]
    val op = [X] => (e: State[Int, X]) => (e match
      case State.Get() =>
        Delim.control0[Ans, Int, W](p)(k => okay.pure((s: Int) => Delim.push(p)(k(s)).flatMap(f => f(s))))
      case State.Set(s1) =>
        Delim.control0[Ans, Int, W](p)(k => okay.pure((_: Int) => Delim.push(p)(k(s1)).flatMap(f => f(s1))))
    ): X ! Delim + W
    val body = rewrite(op)(prog).map(a => (s: Int) => okay.pure[Delim + W, (Int, A)]((s, a)))
    Delim.run[(Int, A), W](Delim.push(p)(body).flatMap(f => f(s0)))

  val counter: Int ! Row = for
    a <- State.get[Int].at[Row]
    _ <- Writer.tell(s"a=$a").at[Row]
    _ <- State.set(a + 10).at[Row]
    b <- State.get[Int].at[Row]
    _ <- Writer.tell(s"b=$b").at[Row]
    _ <- State.set(b * 2).at[Row]
  yield a + b

  def loop(n: Int): Int ! Row =
    if n == 0 then State.get[Int].at[Row]
    else
      for
        s <- State.get[Int].at[Row]
        _ <- Writer.tell(s"$s").at[Row]
        _ <- State.set(s + n).at[Row]
        r <- loop(n - 1)
      yield r

  // ------------------------------------------------ the correspondence, checked

  test("DEEP: ret $ body with shift0 operations IS State.handle, by Bisim on the Writer row") {
    assertEquals(Bisim.check(deep(1)(counter), State.handle[Int](1)(counter)), Verdict.Same(1, 0))
    assertEquals(Bisim.check(deep(0)(loop(20)), State.handle[Int](0)(loop(20)), depth = 64), Verdict.Same(1, 0))
  }

  test("SHALLOW: control0 operations with the handler re-installed around k IS State.handle") {
    assertEquals(Bisim.check(shallow(1)(counter), State.handle[Int](1)(counter)), Verdict.Same(1, 0))
    assertEquals(Bisim.check(shallow(0)(loop(20)), State.handle[Int](0)(loop(20)), depth = 64), Verdict.Same(1, 0))
  }

  test("the values, not only the trees: (22, 12) and the two tells") {
    assertEquals(!.run(Writer.run[String, (Int, Int), Pure](deep(1)(counter))), (List("a=1", "b=11"), (22, 12)))
  }

  test("MUTANT: a deep Set clause that keeps the OLD state is refused, with the first differing tell") {
    type Ans = Int => (Int, Int) ! Delim + W
    val p = Delim.prompt[Ans]
    val op = [X] => (e: State[Int, X]) => (e match
      case State.Get() => Delim.shift0[Ans, Int, W](p)(k => okay.pure((s: Int) => k(s).flatMap(f => f(s))))
      case State.Set(s1) => Delim.shift0[Ans, Int, W](p)(k => okay.pure((s: Int) => k(s1).flatMap(f => f(s))))
    ): X ! Delim + W
    val ret: Int => Ans ! Delim + W = a => okay.pure((s: Int) => okay.pure((s, a)))
    val wrong = Delim.run[(Int, Int), W](Delim.dollar[Int, Ans, W](p)(ret)(rewrite(op)(counter)).flatMap(f => f(1)))
    Bisim.check(wrong, State.handle[Int](1)(counter)) match
      case Verdict.Differ(path, l, r) =>
        assertEquals((l, r), ("performed Say(b=1)", "performed Say(b=11)"))
        assertEquals(path, List("Say(a=1) -> ()"))
      case v => fail(s"expected Differ, got $v")
  }

  test("depth: 10 000 operations through the deep and the shallow encodings, in constant stack") {
    def spin(n: Int): Int ! Row =
      if n == 0 then State.get[Int].at[Row]
      else State.get[Int].at[Row].flatMap(s => State.set(s + 1).at[Row]).flatMap(_ => spin(n - 1))
    assertEquals(!.run(Writer.run[String, (Int, Int), Pure](deep(0)(spin(10_000))))._2, (10_000, 10_000))
    assertEquals(!.run(Writer.run[String, (Int, Int), Pure](shallow(0)(spin(10_000))))._2, (10_000, 10_000))
  }
