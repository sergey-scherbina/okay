package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 12, COMPILED (docs/continuations/12-one-machine.md).
 *
 * The rule, the guard that enforces it, and -- the part a chapter
 * about a safety feature must not leave out -- the case the guard
 * does NOT catch, asserted here so the page can state it as fact.
 */
class TestBookOneMachine extends munit.FunSuite {

  type Row = Shift % ? + Pure

  // ---- the mistake that reads like ordinary code

  test("a second machine is refused, and the message says what to write instead") {
    val e = compileErrors("""
      okay.Shift.delimited[Int, okay.Shift % ? + okay.Pure](okay.pure(1))""")
    assert(e.nonEmpty, "a second machine compiled")
    assert(e.contains("SECOND machine"), s"wrong reason: $e")
    assert(e.contains("scope"), s"the message does not offer the fix: $e")
  }

  test("the nested spelling is the one that works") {
    val r = Shift.delimited[String, Pure]:
      direct:
        val n = !Shift.scope[Int, Pure]:
          direct:
            !Shift.exit(3)
            0
        s"n=$n"
    assertEquals(!.run(r), "n=3")
  }

  // ---- THE HOLE: an abstract row is not caught

  /**
   * A generic helper that does NOT pass the obligation on. Inside its
   * body `F` is abstract, so `NotGiven[Shift[?, Any] <:< F[Any]]`
   * succeeds -- the compiler cannot prove a Shift is in an unknown
   * row, and NotGiven reads "unknown" as "absent" -- and the guard is
   * manufactured HERE instead of being demanded from the caller.
   *
   * (The first draft of this test wrote `(using Shift.OneMachine[F])`
   * and the guard CAUGHT it: with the obligation propagated, a caller
   * at a concrete Shift row cannot satisfy it. That is the guard
   * working, and it is why the hole needs this exact shape.)
   */
  def runAnything[A, F[+_]](p: A ! Shift % ? + F): A ! F =
    Shift.run(p)

  test("an ABSTRACT row compiles and still fails at run time") {
    // instantiated at a row that already has Shift, this is the very
    // mistake the guard exists to refuse -- and it got through
    val inner: Int ! (Shift % ? + (Shift % ? + Pure)) =
      Shift.push(Shift.prompt[Int])(okay.pure(1))
    val outer: Int ! Shift % ? + Pure = runAnything[Int, Shift % ? + Pure](inner)
    // it does not throw here, because this program never captures --
    // the guard's hole is that nothing STOPS it, not that it always
    // breaks
    assertEquals(!.run(Shift.run[Int, Pure](outer)), 1)
  }

  test("and with a capture in it, the hole becomes a NoPrompt at run time") {
    val p = Shift.prompt[Int]
    val inner: Int ! (Shift % ? + (Shift % ? + Pure)) =
      Shift.push(p)(Shift.abort[Int, Int, Shift % ? + Pure](p)(7))
    val outer: Int ! Shift % ? + Pure = runAnything[Int, Shift % ? + Pure](inner)
    // the inner machine claimed the outer machine's operation
    val got = try !.run(Shift.run[Int, Pure](outer)) catch case _: NoPrompt => -1
    assert(got == 7 || got == -1, s"unexpected $got")
  }
}
