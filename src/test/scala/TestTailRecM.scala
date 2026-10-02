package okay

/**
 * `tailRecM`, derived for every okay `Monad` (specs/monad-tailrecm.md):
 * deep through an eager carrier and a deferring one, short-circuiting,
 * and the `TailRecM` class every Monad gets.
 */
class TestTailRecM extends munit.FunSuite:

  val n = 1000000

  test("a million iterations through Option, an EAGER flatMap") {
    val M = summon[Monad[Option]]
    val out = M.tailRecM(0)(i => Some(if i < n then Left(i + 1) else Right(i)))
    assertEquals(out, Some(n))
  }

  test("an Option loop stops at the first None") {
    val M = summon[Monad[Option]]
    var calls = 0
    val out = M.tailRecM(0)(i => { calls += 1; if i == 3 then None else Some(Left(i + 1)) })
    assertEquals(out, None)
    assertEquals(calls, 4)
  }

  test("a million iterations through a program, each one an operation") {
    val M = summon[Monad[[X] =>> X ! State % Int]]
    val p = M.tailRecM(0)(i => State.modify[Int](_ + 1).map(_ => if i < n then Left(i + 1) else Right(i)))
    assertEquals(State.run(0)(p), (n + 1, n))
  }

  test("every Monad is a TailRecM: the class answers the same loop") {
    val R = summon[TailRecM[Option]]
    assertEquals(R.tailRecM(0)(i => Some(if i < n then Left(i + 1) else Right(i))), Some(n))
  }
