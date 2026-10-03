package okay

import okay.Freer.{Return, Inject, Bind}
import Delimited.{Row, Sum}

/**
 * NESTED MACHINES (specs/cont-atm.md): Danvy & Filinski's shift/reset with answer-type modification as a machine
 * UNDER Reader. It answers its own operations; Reader's leave in the program it answers (`Free`), which
 * `Reader.run` — Reader's own handler, untouched — then runs. ATM stays typed: the machine's stack holds only its
 * own boundaries, and Reader's operations are diagonal by their constructor (`Sum.Fwd`).
 */
class TestDelimitedNested extends munit.FunSuite:

  private type E = Reader % Int
  private type H = Row[D, E]

  sealed trait D[S, R, +A]
  final case class Strict[S, R, A](body: (A => S) => R) extends D[S, R, A]
  final case class Lazily[S, R, A](body: Delimited.Kont[H, A, S] => Freer[H, R, R, R]) extends D[S, R, A]
  final case class Resume[A, S, T](k: Delimited.Kont[H, A, S], a: A) extends D[T, T, S]

  object Steps extends Step[D, H]:
    def step[A, B, S, T, R, Z](op: D[T, R, A], k: Frames[H, A, B, S, T], m: Stack[H, B, S, R, Z],
                               machine: Delimited[H]): Delimited.Next[H, Z] = op match
      case Resume(k1, a) => machine.next(Return(a), k1.k, machine.bound(null, k, m))
      case leaf =>
        val c = machine.closed(k, m)
        if c == null then throw IllegalStateException("a shift with no reset around it")
        leaf match
          case Strict(body) => c.answer(body(x => machine.force(c.k, x)))
          case Lazily(body) => c.instead(body(c))
          case Resume(_, _) => throw IllegalStateException("unreachable")

  private def own[A, S, R](op: D[S, R, A]): Freer[H, S, R, A] = Inject(Sum.Own[D, E, S, R, A](op))
  private def ask[X]: Freer[H, X, X, Int] = Inject(Sum.Fwd[D, E, X, Int](Reader.Ask[Int, Int]()))
  private def pure[A, R](a: A): Freer[H, R, R, A] = Return(a)
  private def strict[A, S, R](body: (A => S) => R): Freer[H, S, R, A] = own(Strict(body))
  private def lazily[A, S, R](body: Delimited.Kont[H, A, S] => Freer[H, R, R, R]): Freer[H, S, R, A] = own(Lazily(body))
  private def call[A, S, X](k: Delimited.Kont[H, A, S], a: A): Freer[H, X, X, S] = own(Resume[A, S, X](k, a))
  extension [A, S, R](c: Freer[H, S, R, A])
    private def andThen[B, S2](f: A => Freer[H, S2, S, B]): Freer[H, S2, R, B] = Bind(c, f)

  /** the inner machine's answer is a Reader program; Reader's own handler runs it */
  private def run[A, S, R](c: Freer[H, S, R, A], env: Int)(k: A => S): R =
    !.run(Reader.run[Int, R, okay.Pure](env)(Delimited.under[D, E](Steps).run(c, k)))

  test("ATM under Reader: Int → String and Boolean → Int in one reset, asking the environment in a body and in k") {
    val c: Freer[H, Boolean, String, Boolean] =
      lazily[Int, Int, String](k => ask[String].andThen(e => call[Int, Int, String](k, e).andThen(n => pure(n.toString))))
        .andThen(a => lazily[Boolean, Boolean, Int](k => call[Boolean, Boolean, Int](k, true).andThen(h => pure(if h then 1 else 0)))
        .andThen(b => ask[Boolean].andThen(e => pure(a + e > 0 && b))))
    // env 7: the first body asks 7, k1(7) runs on: the second body's k2(true) asks again, 7 + 7 > 0 && true;
    // the second body answers 1, so k1(7) is 1 and the first answers "1"
    assertEquals(run(c, 7)(identity), "1")
  }

  test("a strict k that asks the environment is refused by name; one that does not, runs") {
    val asks: Freer[H, Int, Int, Int] = strict[Int, Int, Int](k => k(1) + 1).andThen(x => ask[Int].andThen(e => pure(x + e)))
    val pureOnly: Freer[H, Int, Int, Int] = ask[Int].andThen(e => strict[Int, Int, Int](k => k(e) + 1).andThen(x => pure(x * 2)))
    val e = intercept[IllegalStateException](run(asks, 10)(identity))
    assert(e.getMessage.nn.contains("outer effect"), e.getMessage)
    assertEquals(run(pureOnly, 10)(identity), 21)
  }

  test("stack safety on 256 KB: 100 000 lazy shifts, each asking the environment through the machine outside") {
    val n = 100000
    val c = (1 to n).foldLeft(pure[Int, Int](0))((m, _) =>
      m.andThen(x => lazily[Int, Int, Int](k => ask[Int].andThen(e => call[Int, Int, Int](k, x + e)))))
    var out = 0
    var err: Throwable | Null = null
    val t = Thread(null, () => try out = run(c, 1)(identity) catch case e: Throwable => err = e, "small", 256 * 1024)
    t.start()
    t.join()
    if err != null then throw err.nn
    assertEquals(out, n)
  }
