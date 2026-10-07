package okay.freer

import okay.{Monad, TailRecM}
import okay.given

/**
 * `TailRecM`, the carrier's own loop (specs/eager-carrier-depth.md), on a
 * 128 KB thread: a loop that held a host frame per iteration overflows
 * there at a thousand, so a million proves the loop holds none. The
 * first cut — a derivation through `Cps` — passed a million only on
 * sbt's 8 MB stack and overflowed this test at 1 000.
 */
class TestTailRecM extends munit.FunSuite:

  val n = 1000000

  test("Option: a million iterations on a 128 KB thread, an EAGER flatMap") {
    val M = summon[Monad[Option]]
    val out = SmallStack.run(128)(M.tailRecM(0)(i => Some(if i < n then Left(i + 1) else Right(i))))
    assertEquals(out, Some(n))
  }

  test("an Option loop stops at the first None") {
    val M = summon[Monad[Option]]
    var calls = 0
    val out = M.tailRecM(0)(i => { calls += 1; if i == 3 then None else Some(Left(i + 1)) })
    assertEquals(out, None)
    assertEquals(calls, 4)
  }

  test("Either: a million on a 128 KB thread; a Left error stops it") {
    val R = summon[TailRecM[[X] =>> Either[String, X]]]
    assertEquals(SmallStack.run(128)(R.tailRecM(0)(i => Right(if i < n then Left(i + 1) else Right(i)))), Right(n))
    assertEquals(R.tailRecM(0)(i => if i == 5 then Left("stop") else Right(Left(i + 1))), Left("stop"))
  }

  test("LazyList: a million Lefts before each answer on a 128 KB thread, answers on demand") {
    val R = summon[TailRecM[LazyList]]
    // from 0: run to n, then answer n and n+1 (two branches)
    val out = SmallStack.run(128)(R.tailRecM(0)(i =>
      if i < n then LazyList(Left(i + 1)) else LazyList(Right(i), Right(i + 1))).toList)
    assertEquals(out, List(n, n + 1))
    var expanded = 0
    val lazily = R.tailRecM(0)(i => { expanded += 1; LazyList(Right(i), Left(i + 1)) })
    assertEquals(lazily.take(3).toList, List(0, 1, 2))
    assertEquals(expanded, 3)
  }

  test("the context monad: a million on a 128 KB thread, under its context") {
    val R = summon[TailRecM[[X] =>> Int ?=> X]]
    val loop: Int ?=> Int = R.tailRecM(0)(i => if i < n then Left(i + summon[Int]) else Right(i))
    assertEquals(SmallStack.run(128)(loop(using 1)), n)
  }

  test("a program: a million iterations, each one an operation, on a 128 KB thread") {
    val M = summon[Monad[[X] =>> X ! State % Int]]
    val p = M.tailRecM(0)(i => State.modify[Int](_ + 1).map(_ => if i < n then Left(i + 1) else Right(i)))
    assertEquals(SmallStack.run(128)(State.run(0)(p)), (n + 1, n))
  }

  test("a strict monad with no TailRecM has no tailRecM: a compile error naming the class") {
    val errors = compileErrors("""
      final case class Box[A](a: A)
      given Monad[Box] with
        def pure[A](a: A): Box[A] = Box(a)
        extension [A](b: Box[A]) def flatMap[B](f: A => Box[B]): Box[B] = f(b.a)
      summon[Monad[Box]].tailRecM(0)(i => Box(Right(i)))
    """)
    assert(errors.contains("no TailRecM"), errors)
  }
