package okay.freer


import okay.freer.Freer.{Return, Inject, Bind}

/**
 * ATM THROUGH AN EFFECT (specs/cont-atm.md): Reader as continuation MARKS — `local` installs a value boundary
 * carrying the environment, `ask` finds the nearest one. A mark is transparent: it moves no answer type, and a capture
 * carries it. So a shift INSIDE `local` changes the answer type of its own reset, through Reader, and an `ask`
 * inside `k` sees the environment `k` was captured under. One machine, `Freer` untouched.
 */
class TestDelimitedMarks extends munit.FunSuite:

  /** the environment, hung on the stack */
  final class Env(val value: Int) extends Delimited.Mark

  sealed trait Op[S, R, +A]
  final case class Strict[S, R, A](body: (A => S) => R) extends Op[S, R, A]
  final case class Lazily[S, R, A](body: Delimited.Kont[Op, A, S] => Freer[Op, R, R, R]) extends Op[S, R, A]
  final case class Resume[A, S, T](k: Delimited.Kont[Op, A, S], a: A) extends Op[T, T, S]
  /** `body` with `env` for its environment: TRANSPARENT — the same answer pair inside as outside */
  final case class Local[S, R, A](env: Int, body: Freer[Op, S, R, A]) extends Op[S, R, A]
  final case class Ask[X]() extends Op[X, X, Int]

  object Steps extends Delimited.Step[Op, Op]:
    def step[A, B, S, T, R, Z](op: Op[T, R, A], k: Frames[Op, A, B, S, T], m: Stack[Op, B, S, R, Z],
                               machine: Delimited[Op]): Delimited.Next[Op, Z] = op match
      case Resume(k1, a) => k1.resume(a, k, m)
      case Local(env, body) => machine.next(body, machine.end, machine.delim(Env(env), k, m))
      case Ask() => machine.holds(m, _.isInstanceOf[Env]) match
        case e: Env => machine.next(Return(e.value), k, m)
        case _ => throw IllegalStateException("ask outside any local")
      case leaf =>
        val c = machine.closed(k, m)
        if c == null then throw IllegalStateException("a shift with no reset around it")
        leaf match
          case Strict(body) => c.answer(body(x => machine.force(c, x)))
          case Lazily(body) => c.instead(body(c))
          case _ => throw IllegalStateException("unreachable")

  private def run[A, S, R](c: Freer[Op, S, R, A])(k: A => S): R = Delimited(Steps).run(c, k)
  private def pure[A, R](a: A): Freer[Op, R, R, A] = Return(a)
  private def strict[A, S, R](body: (A => S) => R): Freer[Op, S, R, A] = Inject(Strict(body))
  private def lazily[A, S, R](body: Delimited.Kont[Op, A, S] => Freer[Op, R, R, R]): Freer[Op, S, R, A] = Inject(Lazily(body))
  private def call[A, S, X](k: Delimited.Kont[Op, A, S], a: A): Freer[Op, X, X, S] = Inject(Resume[A, S, X](k, a))
  private def local[S, R, A](env: Int)(body: Freer[Op, S, R, A]): Freer[Op, S, R, A] = Inject(Local(env, body))
  private def ask[X]: Freer[Op, X, X, Int] = Inject(Ask[X]())
  extension [A, S, R](c: Freer[Op, S, R, A])
    private def andThen[B, S2](f: A => Freer[Op, S2, S, B]): Freer[Op, S2, R, B] = Bind(c, f)

  test("a shift inside local changes its reset's answer type, Int → String, through Reader; k asks the environment") {
    val lazyLeaf: Freer[Op, Int, String, Int] =
      local(10)(lazily[Int, Int, String](k => call[Int, Int, String](k, 1).andThen(n => pure((n * 2).toString)))
        .andThen(x => ask[Int].andThen(e => pure(x + e))))
    val strictLeaf: Freer[Op, Int, String, Int] =
      local(10)(strict[Int, Int, String](k => (k(1) * 2).toString).andThen(x => ask[Int].andThen(e => pure(x + e))))
    // k(1) runs on under the mark k carries: 1 + 10; the body doubles it and answers text
    assertEquals((run(lazyLeaf)(identity), run(strictLeaf)(identity)), ("22", "22"))
  }

  test("multi-shot: a k resumed twice carries the mark both times") {
    val c: Freer[Op, List[Int], List[Int], Int] =
      local(100)(lazily[Int, List[Int], List[Int]](k =>
        call[Int, List[Int], List[Int]](k, 1).andThen(a => call[Int, List[Int], List[Int]](k, 2).andThen(b => pure(a ++ b))))
        .andThen(x => ask[List[Int]].andThen(e => pure(x + e))))
    assertEquals(run(c)(List(_)), List(101, 102))
  }

  test("marks nest and leave: the inner local shadows, and after it the outer is seen again") {
    val c: Freer[Op, Int, Int, Int] =
      local(1)(local(2)(ask[Int]).andThen(inner => ask[Int].andThen(outer => pure(inner * 10 + outer))))
    assertEquals(run(c)(identity), 21)
    val e = intercept[IllegalStateException](run(ask[Int])(identity))
    assert(e.getMessage.nn.contains("outside any local"), e.getMessage)
  }

  test("stack safety on 256 KB: 10 000 shifts under a local, each k asking through the mark") {
    val n = 10000
    val c = local(1)((1 to n).foldLeft(pure[Int, Int](0))((m, _) =>
      m.andThen(x => lazily[Int, Int, Int](k => call[Int, Int, Int](k, x).andThen(y => pure(y))).andThen(y => ask[Int].andThen(e => pure(y + e))))))
    var out = 0
    var err: Throwable | Null = null
    val t = Thread(null, () => try out = run(c)(identity) catch case e: Throwable => err = e, "small", 256 * 1024)
    t.start()
    t.join()
    if err != null then throw err.nn
    assertEquals(out, n)
  }
