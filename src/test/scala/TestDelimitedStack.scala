package okay

import okay.Freer.{Return, Inject, Bind}

/** the stack of continuations (specs/cont-atm.md), effect-independent: this suite declares its own effect —
 * Danvy & Filinski's shift/reset with answer-type modification — and the machine knows nothing of it */
class TestDelimitedStack extends munit.FunSuite:

  sealed trait Op[S, R, +A]
  final case class Strict[S, R, A](body: (A => S) => R) extends Op[S, R, A]
  final case class Lazily[S, R, A](body: Delimited.Kont[Op, A, S] => Freer[Op, R, R, R]) extends Op[S, R, A]
  final case class Resume[A, S, T](k: Delimited.Kont[Op, A, S], a: A) extends Op[T, T, S]

  object Steps extends Delimited.Step[Op, Op]:
    def step[A, B, S, T, R, Z](op: Op[T, R, A], k: Frames[Op, A, B, S, T], m: Stack[Op, B, S, R, Z],
                               machine: Delimited[Op]): Delimited.Next[Op, Z] = op match
      case Resume(k1, a) => k1.resume(a, k, m)
      case leaf =>
        val c = machine.closed(k, m)
        if c == null then throw IllegalStateException("a shift with no reset around it")
        leaf match
          case Strict(body) => c.answer(body(x => machine.force(c, x)))
          case Lazily(body) => c.instead(body(c))
          case Resume(_, _) => throw IllegalStateException("unreachable")

  private type Prog[A, S, R] = Freer[Op, S, R, A]
  private def run[A, S, R](c: Prog[A, S, R], k: A => S): R = Delimited(Steps).run(c, k)
  private def pure[A, R](a: A): Prog[A, R, R] = Return(a)
  private def strict[A, S, R](body: (A => S) => R): Prog[A, S, R] = Inject(Strict(body))
  private def lazily[A, S, R](body: Delimited.Kont[Op, A, S] => Freer[Op, R, R, R]): Prog[A, S, R] = Inject(Lazily(body))
  private def call[A, S, T](k: Delimited.Kont[Op, A, S], a: A): Freer[Op, T, T, S] = Inject(Resume[A, S, T](k, a))
  extension [A, S, R](c: Prog[A, S, R])
    private def andThen[B, S2](f: A => Prog[B, S2, S]): Prog[B, S2, R] = Bind(c, f)

  test("answer-type modification: Int → String and Boolean → Int in one reset, strict and lazy") {
    val s: Prog[Boolean, Boolean, String] =
      strict[Int, Int, String](k => k(1).toString).andThen(a =>
        strict[Boolean, Boolean, Int](k => if k(true) then 1 else 0).andThen(b => pure(a > 0 && b)))
    val l: Prog[Boolean, Boolean, String] =
      lazily[Int, Int, String](k => call[Int, Int, String](k, 1).flatMap(n => Return(n.toString))).andThen(a =>
        lazily[Boolean, Boolean, Int](k => call[Boolean, Boolean, Int](k, true).flatMap(h => Return(if h then 1 else 0)))
          .andThen(b => pure(a > 0 && b)))
    // k2(true) = 1 > 0 && true; the second body answers 1; k1(1) = 1; the first answers "1"
    assertEquals((run(s, identity), run(l, identity)), ("1", "1"))
  }

  test("k(x + 1) + k(x + 1) chained d deep, strict and lazy, against closures (cont-shift-op's refuting case)") {
    def s(d: Int): Prog[Int, Int, Int] =
      (1 to d).foldLeft(pure[Int, Int](0))((m, _) => m.andThen(x => strict[Int, Int, Int](k => k(x + 1) + k(x + 1))))
    def l(d: Int): Prog[Int, Int, Int] =
      (1 to d).foldLeft(pure[Int, Int](0))((m, _) => m.andThen(x => lazily[Int, Int, Int](k =>
        call[Int, Int, Int](k, x + 1).flatMap(a => call[Int, Int, Int](k, x + 1).flatMap(b => Return(a + b))))))
    def closures(d: Int): Func[Int, Int, Int] =
      (1 to d).foldLeft[Func[Int, Int, Int]](k => k(0))((m, _) => k => m(x => k(x + 1) + k(x + 1)))
    for d <- 0 to 6 do
      val want = closures(d)(identity)
      assertEquals((run(s(d), identity), run(l(d), identity)), (want, want), s"d = $d")
  }

  test("multi-shot: a lazy k resumed twice collects both worlds") {
    val c: Prog[List[Int], List[Int], List[Int]] =
      lazily[Int, List[Int], List[Int]](k => call[Int, List[Int], List[Int]](k, 1).flatMap(a =>
        call[Int, List[Int], List[Int]](k, 2).flatMap(b => Return(a ++ b)))).andThen(x =>
        lazily[Int, List[Int], List[Int]](k => call[Int, List[Int], List[Int]](k, 10).flatMap(a =>
          call[Int, List[Int], List[Int]](k, 20).flatMap(b => Return(a ++ b)))).andThen(y => pure(List(x + y))))
    assertEquals(run(c, identity), List(11, 21, 12, 22))
  }

  private def onSmallStack[A](body: => A): A =
    var out: Option[A] = None
    var err: Throwable | Null = null
    val t = Thread(null, () => try out = Some(body) catch case e: Throwable => err = e, "small", 256 * 1024)
    t.start()
    t.join()
    if err != null then throw err.nn
    out.get

  test("stack safety on 256 KB: 1M binds, 1M lazy shifts, 100 000 nested strict k") {
    val n = 1000000
    val binds = (1 to n).foldLeft(pure[Int, Int](0))((m, _) => m.andThen(x => pure(x + 1)))
    val shifts = (1 to n).foldLeft(pure[Int, Int](0))((m, _) =>
      m.andThen(x => lazily[Int, Int, Int](k => call[Int, Int, Int](k, x + 1).flatMap(s => Return(s + 1)))))
    val depth = 100000
    val strictly = (1 to depth).foldLeft(pure[Int, Int](0))((m, _) => m.andThen(x => strict[Int, Int, Int](k => k(x + 1) + 1)))
    assertEquals(onSmallStack((run(binds, identity), run(shifts, identity), run(strictly, identity))),
      (n, 2 * n, 2 * depth))
  }
