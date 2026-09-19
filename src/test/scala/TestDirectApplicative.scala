package okay

import okay.Direct.{*, given}
// The colourless-val spelling uses `selfColor`, which is an implicit
// CONVERSION — so the file needs the language import, and without it
// the nine uses raise feature warnings that no ordinary compile
// shows. Found 2026-09-18 by hunting with `-feature` for another
// lane; AGENTS.md says to hunt that way for exactly this reason, and
// the memory note `direct-given-import-needed` records that the
// per-file import is the standing answer since the build flag went.
import scala.language.implicitConversions

/**
 * Direct style at a carrier with no monad (specs/applicative-do.md).
 *
 * `Validated` refuses a `Monad` on purpose, so until this lane a
 * `direct` block at it was rejected at the call site with
 * `no Monad[V]`. What the block needs is decided by the block: a run
 * of independent binds needs only `Applicative`.
 */
class TestDirectApplicative extends munit.FunSuite {

  private given Semigroup[Vector[String]] with
    def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y

  private type V[A] = Validated[Vector[String], A]

  private def check(n: Int): V[Int] =
    if n % 2 == 0 then Validated.Valid(n) else Validated.Invalid(Vector(s"$n is odd"))

  test("THE POINT: a direct block at Validated COLLECTS, which flatMap cannot") {
    val both: V[Int] = direct[V]:
      val a = check(1).reflect
      val b = check(3).reflect
      a + b
    assertEquals(both, Validated.Invalid(Vector("1 is odd", "3 is odd")))
  }

  test("all valid: the answers and their order are what the bracket gives") {
    val ok: V[Int] = direct[V]:
      val a = check(2).reflect
      val b = check(4).reflect
      val c = check(6).reflect
      a * 100 + b * 10 + c
    assertEquals(ok, Validated.Valid(246))
  }

  test("one bind, and none at all") {
    assertEquals(direct[V](check(2).reflect + 1), Validated.Valid(3))
    assertEquals(direct[V](7), Validated.Valid(7))
  }

  test("THE SHORTEST SPELLING: no type argument, no marks, no annotations") {
    // the carrier comes from the expected type; the vals are
    // COLOURLESS (their inferred type is a program of the carrier, so
    // the macro binds them); the uses are auto-coloured. Every one of
    // the three is optional, and all three can be left out at once.
    val f: V[Int] = direct:
      val a = check(1)
      val b = check(3)
      a + b
    assertEquals(f, Validated.Invalid(Vector("1 is odd", "3 is odd")))
  }

  test("the spellings mix: a mark here, an annotation there, a bare val next") {
    val f: V[Int] = direct[V]:
      val a = check(1).reflect
      val b: Int = check(3)
      val c = check(5)
      a + b + c
    assertEquals(f, Validated.Invalid(Vector("1 is odd", "3 is odd", "5 is odd")))
  }

  test("a coloured USE of a bound name is not a leaf of its own") {
    // the conversion wraps the NAME, and the name is bound by the
    // curried lambda. Hoisting it as a leaf lifted a reference out of
    // the scope that defines it — "a reference to value a was used
    // outside the scope where it was defined", which is how this case
    // was found.
    val f: V[Int] = direct:
      val a = check(2)
      a * 10 + a
    assertEquals(f, Validated.Valid(22))
  }

  test("a DEPENDENT bind is refused by name, not by a missing-Monad at the call site") {
    val e = compileErrors("""
      val bad: V[Int] = Direct.direct[V] {
        val a = check(2).reflect
        val b = check(a).reflect
        a + b
      }
    """)
    assert(e.nonEmpty, "a dependent bind compiled at an applicative-only carrier")
    assert(e.contains("INDEPENDENT"), e)
    assert(e.contains("`b`"), e)
  }

  test("a statement that is not a marked val is refused too") {
    val e = compileErrors("""
      val bad: V[Int] = Direct.direct[V] {
        val a = check(2).reflect
        println("hello")
        a
      }
    """)
    assert(e.nonEmpty, "a bare statement compiled at an applicative-only carrier")
  }

  test("a carrier that HAS a monad is untouched: the program road still binds") {
    // the same shape at a row, which has a Monad — emission unchanged
    given Handler[Reader % Int] = new:
      def handle[A](e: Reader[Int, A]): A = e match
        case Reader.Ask() => 21
    val prog: Int ! Reader % Int = direct:
      val a = Reader.ask[Int].reflect
      val b = Reader.ask[Int].reflect
      a + b
    assertEquals(prog.runWith, 42)
  }
}
