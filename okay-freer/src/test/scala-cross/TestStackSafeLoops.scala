package okay.freer

import okay.*
import okay.given

/**
 * The carriers' loops and `foldMap` on EVERY platform
 * (specs/eager-carrier-depth.md): Scala.js has no fresh stack to move to,
 * and the derivation these replaced overflowed there at 300-1 000
 * iterations. A million here, on the engine's own stack.
 */
class TestStackSafeLoops extends munit.FunSuite:

  val n = 1000000

  enum Op[+A]:
    case Lookup(key: String) extends Op[Int]

  val toOption: Op ==> Option = [X] => (e: Op[X]) => e match
    case Op.Lookup(_) => Some(1)

  test("tailRecM: Option, Either, LazyList, the context monad, a program — a million each") {
    assertEquals(summon[TailRecM[Option]].tailRecM(0)(i => Some(if i < n then Left(i + 1) else Right(i))), Some(n))
    assertEquals(summon[TailRecM[[X] =>> Either[String, X]]].tailRecM(0)(i => Right(if i < n then Left(i + 1) else Right(i))), Right(n))
    assertEquals(summon[TailRecM[LazyList]].tailRecM(0)(i => if i < n then LazyList(Left(i + 1)) else LazyList(Right(i))).toList, List(n))
    val ctx: Int ?=> Int = summon[TailRecM[[X] =>> Int ?=> X]].tailRecM(0)(i => if i < n then Left(i + summon[Int]) else Right(i))
    assertEquals(ctx(using 1), n)
    val p = summon[Monad[[X] =>> X ! State % Int]].tailRecM(0)(i => State.modify[Int](_ + 1).map(_ => if i < n then Left(i + 1) else Right(i)))
    assertEquals(State.run(0)(p), (n + 1, n))
  }

  test("foldMap into an EAGER Option: a million operations, left-nested and non-tail") {
    val left: Int ! Op = (1 to n).foldLeft(pure[Op, Int](0))((p, _) =>
      p.flatMap(s => effect(Op.Lookup("x")).map(_ + s)))
    def nonTail(i: Int): Int ! Op =
      if i == 0 then pure(0) else effect(Op.Lookup("x")).flatMap(x => nonTail(i - 1).map(_ + x))
    assertEquals(left.foldMap(toOption), Some(n))
    assertEquals(nonTail(n).foldMap(toOption), Some(n))
  }
