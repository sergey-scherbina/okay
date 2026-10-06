package okay.freer

import Freer.*

/** `PState` on the basis: Danvy–Filinski's state-passing answer, the STATE'S TYPE carried by the ANSWER TYPE (Asai–Kameyama).
 * No `Put[S, T]` signature: `put` is a shift that moves the answer from `St[S2]` to `St[S]`, and `Bind` composes the moves */
class TestState extends okay.testkit.Munit.Diagnosed:
  /** the answer of a stateful program: given the state, the rest of the run; `W` the run's value */
  type St[H[+_], S, W] = S => Freer[H, EmptyTuple, W, W, W]

  /** the operations at the run's delimiter, the head of the stack; `W` the run's value. The node `Shift`, not the
   * sugar: `put` moves the answer at the hole, which the sugar pins to the one the body is written at */
  final class State[H[+_], W]:
    /** read the state, its type `S` unchanged: `shift(k => s => k(s)(s))` */
    def get[S]: Freer[H, Lvl[H], St[H, S, W], St[H, S, W], S] =
      Freer.Shift((k: [U, E] => S => Freer[H, E, U, U, St[H, S, W]]) => pure((s: S) => k(s).flatMap(f => f(s))))
    /** write a state of another type: the rest wants `S2`, this point still answers with `S`: `shift(k => _ => k(())(s2))` */
    def put[S]: PutFrom[S] = PutFrom[S]()
    final class PutFrom[S]:
      def apply[S2](s2: S2): Freer[H, Lvl[H], St[H, S2, W], St[H, S, W], Unit] =
        Freer.Shift((k: [U, E] => Unit => Freer[H, E, U, U, St[H, S2, W]]) => pure((_: S) => k(()).flatMap(f => f(s2))))

  /** run from an initial state `S0`, at the top; the state ends at `S1`; the delimiter's answer is the function,
   * applied once outside */
  def runState[H[+_], S0, S1, W](s0: S0)(body: State[H, W] => Freer[H, Lvl[H], St[H, S1, W], St[H, S0, W], W]): Freer[H, EmptyTuple, W, W, W] =
    reset[H, St[H, S1, W]](body(State()).map(a => (_: S1) => pure(a))).flatMap(f => f(s0))

  def value[U, A](p: Freer[Pure, EmptyTuple, U, U, A]): A =
    val head: Freer[Pure, EmptyTuple, U, U, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  test("state: get after put, the type unchanged"):
    val prog: Freer[Pure, EmptyTuple, Int, Int, Int] = runState(1): st =>
      st.put[Int](41).flatMap(_ => st.get[Int]).map(_ + 1)
    assertEquals(value(prog), 42)

  test("TYPE-CHANGING state: Int in, a String put, read back as a String"):
    val prog: Freer[Pure, EmptyTuple, Int, Int, Int] = runState(7): st =>
      st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length)
    assertEquals(value(prog), 7)

  test("the final state returned, as master's PState.run"):
    val prog: Freer[Pure, EmptyTuple, (String, Int), (String, Int), (String, Int)] = runState(3): st =>
      st.put[Int]("abc").flatMap(_ => st.get[String]).map(s => (s, s.length))
    assertEquals(value(prog), ("abc", 3))

  test("a wrong order of puts does not compile: the answer types do not meet"):
    assert(compileErrors("""
      val prog: Freer[Pure, EmptyTuple, Int, Int, Int] = runState(1): st =>
        st.put[String](2).flatMap(_ => st.get[Int])
    """).nonEmpty)
