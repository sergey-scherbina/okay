package okay

import okay.Direct.{*, given}
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 16, COMPILED (docs/continuations/16-continuations-and-monads.md).
 *
 * Filinski 1994: given delimited control, ANY monad can be run in
 * direct style. The chapter shows the theorem from both ends --
 * monads the library does not privilege, run in direct style; and a
 * monad BUILT from `shift` with no monadic primitive underneath.
 */
class TestBookMonads extends munit.FunSuite {

  // ---------------------------------------------------------------
  // I. A monad the library knows nothing about, in direct style.

  given Monad[Option] with
    override def pure[A](a: A): Option[A] = Some(a)
    extension [A](m: Option[A])
      override def flatMap[B](f: A => Option[B]): Option[B] = m.flatMap(f)

  def add(mx: Option[Int], my: Option[Int]): Option[Int] =
    direct[Option] {
      val x: Int = mx
      val y: Int = my
      x + y
    }

  test("Option in direct style: no flatMap written, short-circuiting kept") {
    assertEquals(add(Some(2), Some(3)), Some(5))
    assertEquals(add(Some(2), None), None)
    assertEquals(add(None, Some(3)), None)
  }

  test("the short-circuit really skips the rest of the block") {
    var reached = 0
    def watched(o: Option[Int]): Option[Int] = { reached += 1; o }
    val r = direct[Option] {
      val x: Int = Option.empty[Int]
      val y: Int = watched(Some(3))
      x + y
    }
    assertEquals(r, None)
    assertEquals(reached, 0,
      "the block continued past a None: the monad's flatMap was not what ran")

    // and the counter is not merely broken: on the road that DOES continue
    // past the first bind, the same watch fires.
    val ok = direct[Option] {
      val x: Int = Some(1)
      val y: Int = watched(Some(3))
      x + y
    }
    assertEquals(ok, Some(4))
    assertEquals(reached, 1)
  }

  // A monad invented HERE, so that "any monad" is not a figure of speech.
  final case class Counted[A](value: A, steps: Int)

  given Monad[Counted] with
    override def pure[A](a: A): Counted[A] = Counted(a, 0)
    extension [A](m: Counted[A])
      override def flatMap[B](f: A => Counted[B]): Counted[B] =
        val next = f(m.value)
        Counted(next.value, m.steps + next.steps + 1)

  def step(n: Int): Counted[Int] = Counted(n, 1)

  test("a monad defined in this file runs in direct style, bookkeeping intact") {
    val inDirect = direct[Counted] {
      val a: Int = step(2)
      val b: Int = step(3)
      a * b
    }
    val byHand = step(2).flatMap(a => step(3).map(b => a * b))
    assertEquals(inDirect.value, 6)
    assertEquals(inDirect, byHand,
      "direct style must agree with the flatMap chain it replaces")
  }

  // ---------------------------------------------------------------
  // II. The other end: a state monad BUILT from shift, nothing else.
  //     Two definitions. There is no state primitive underneath --
  //     the store is the continuation's argument.

  def sGet[S, R]: Cont[S, S => R, S => R] = shift(k => s => k(s)(s))
  def sSet[S, R](s2: S): Cont[Unit, S => R, S => R] = shift(k => _ => k(())(s2))
  def sRun[S, A](s: S)(m: Cont[A, S => (S, A), S => (S, A)]): (S, A) =
    (m / (a => (fin: S) => (fin, a)))(s)

  test("a state monad derived from shift alone threads the store") {
    val r = sRun(1) {
      for
        a <- sGet[Int, (Int, Int)]
        _ <- sSet[Int, (Int, Int)](a + 10)
        b <- sGet[Int, (Int, Int)]
      yield a + b
    }
    assertEquals(r, (11, 12), "state 1 -> 11, value 1 + 11")
  }

  test("the derived monad obeys the state laws it was never told about") {
    // get >>= set  ==  pure(())   -- reading and writing back changes nothing
    val idle = sRun(7) { sGet[Int, (Int, Unit)].flatMap(s => sSet[Int, (Int, Unit)](s)) }
    assertEquals(idle, (7, ()))
    // set x >> get  ==  set x >> pure(x)  -- the last write wins
    val lastWrite = sRun(7) { sSet[Int, (Int, Int)](9).flatMap(_ => sGet[Int, (Int, Int)]) }
    assertEquals(lastWrite, (9, 9))
  }

  // ---------------------------------------------------------------
  // III. Where the library's own road meets it.

  test("the State effect answers the same program") {
    val p: Int ! State % Int = for
      a <- State.get[Int]
      _ <- State.set(a + 10)
      b <- State.get[Int]
    yield a + b
    assertEquals(State.run[Int, Int](1)(p), (11, 12),
      "the effect row and the shift-derived monad agree")
  }

  test("PState: the derivation generalises to a state that CHANGES TYPE") {
    val r = PState.run(41):
      for
        n <- PState.get                    // n: Int
        _ <- PState.set((n + 1).toString)  // the state is a String now
        s <- PState.get                    // s: String
      yield s + "!"
    assertEquals(r, ("42", "42!"))
  }
}
