package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 18, COMPILED (docs/continuations/18-what-belongs-in-a-library.md).
 *
 * The criterion: a recipe earns the right to be an effect when MORE
 * THAN ONE INTERPRETER exists for it. One interpreter and it stays a
 * function -- an effect there buys ceremony and nothing else.
 */

/** Reading the clock, as an effect: one operation, answering a Long. */
enum Ticks[+A] derives okay.Effect:
  case Millis extends Ticks[Long]

object Ticks:
  inline def millis: Long ! Ticks = effect(Millis)

  /** interpreter one: the real clock. */
  val live: Handler[Ticks] = new:
    def handle[A](e: Ticks[A]): A = e match
      case Millis => System.currentTimeMillis()

  /** interpreter two: a clock that does not move. This is the one
   *  that decides the question -- it exists, therefore Ticks is an
   *  effect rather than a call to System.currentTimeMillis(). */
  def fixed(at: Long): Handler[Ticks] = new:
    def handle[A](e: Ticks[A]): A = e match
      case Millis => at

class TestBookLibrary extends munit.FunSuite {

  // ---- the effect road: one program, two interpreters

  def stamped(msg: String): String ! Ticks = direct:
    s"$msg@${!Ticks.millis}"

  test("the same program is deterministic under one handler and live under the other") {
    given Handler[Ticks] = Ticks.fixed(7L)
    assertEquals(stamped("hi").runWith, "hi@7")
  }

  test("and the live interpreter is the same program, unmodified") {
    given Handler[Ticks] = Ticks.live
    val out = stamped("hi").runWith
    assert(out.startsWith("hi@"), out)
    val t = out.drop(3).toLong
    assert(t > 1_600_000_000_000L, s"not a plausible wall clock: $t")
    assertNotEquals(t, 7L, "the live handler answered the test handler's constant")
  }

  // ---- the call-site road, for comparison: the SAME recipe as a
  // plain function. It works, and it cannot be pinned.

  def stampedDirectly(msg: String): String =
    s"$msg@${System.currentTimeMillis()}"

  test("the function version cannot be made to answer a known time") {
    val a = stampedDirectly("hi")
    assert(a.startsWith("hi@"))
    // There is no handler to supply. A test can assert the shape, or
    // reach for a global clock mock, and that is the whole cost:
    // the SECOND interpretation has nowhere to live.
    assertNotEquals(a, "hi@7")
  }

  // ---- the other side of the criterion: one interpreter, so it
  // stays a function. Making this an effect would add a row member,
  // a handler, and a constructor, and would buy nothing: there is
  // only one way to run it.

  def attempt[A](times: Int)(f: () => A): Either[Throwable, A] =
    var last: Throwable = null
    var i = 0
    while i < times do
      try return Right(f())
      catch case t: Throwable => last = t; i += 1
    Left(last)

  test("a helper with one interpretation is just a helper") {
    var calls = 0
    val ok = attempt(3): () =>
      calls += 1
      if calls < 2 then throw RuntimeException("not yet") else "done"
    assertEquals(ok, Right("done"))
    assertEquals(calls, 2)
  }
}
