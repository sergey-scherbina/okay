package okay


import okay.freer.*
import okay.freer.given
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 21, COMPILED (docs/continuations/21-the-disciplines.md).
 *
 * The three disciplines each have their own suite (TestReplayable,
 * TestDelimDiagnostics, TestDelimLimits). This file pins the property
 * the chapter claims they SHARE: over an abstract row a constraint is
 * neither proved nor refuted -- it PROPAGATES to the caller. That is
 * the feature and the hole, and they are the same mechanism.
 */
class TestBookDisciplines extends munit.FunSuite {

  type P = okay.freer.Pure

  // A helper over an abstract row that DECLARES the obligation.
  // It compiles with no concrete row in sight: the witness is simply
  // passed along.
  def replayableHelper[F[+_]](p: Int ! F)(using Replayable[F]): Int ! F = p

  test("a declared obligation propagates: satisfied at a safe concrete row") {
    val ok = replayableHelper[State % Int](State.get[Int])
    assertEquals(State.run[Int, Int](3)(ok), (3, 3))
  }

  test("and is REFUSED where the caller's row breaks the discipline") {
    val e = compileErrors(
      "replayableHelper[okay.Async](okay.async(1))")
    assert(e.nonEmpty, "an Async row satisfied Replayable")
    assert(e.contains("REPLAY WOULD PERFORM AGAIN"),
      s"refused, but not for the discipline's reason: $e")
  }

  test("the SAME mechanism in Shift.Machine: declared, so the call site answers") {
    def oneMachineHelper[F[+_]](p: Int ! Shift % ? + F)(using Shift.Machine[F]): Int ! F =
      Shift.run(p)
    // at a clean row the helper runs its own machine
    assertEquals(!.run(oneMachineHelper[P](okay.freer.pure(1))), 1)
    // at a row that already holds a machine the CALLER's row says so, and the helper nests
    assertEquals(!.run(Shift.run[Int, P](oneMachineHelper[Shift % ? + P](okay.freer.pure(1)))), 1)
    // undeclared, an abstract row is refused rather than guessed
    val e = compileErrors(
      "def h[F[+_]](p: Int ! okay.freer.Shift % ? + F): Int ! F = okay.freer.Shift.run(p)")
    assert(e.contains("using Shift.Machine[F]"), s"the abstract row was guessed: $e")
  }

  test("THE SHARED HOLE: a helper that declares NOTHING is never asked") {
    // No witness in the signature, so nothing propagates and nothing
    // is checked. This compiles -- and that is precisely chapter 19's
    // THE LIMIT, here to show it is not specific to Shift: an
    // undeclared obligation is an unasked question.
    val e = compileErrors("""
      def silent[F[+_]](p: Int ! F): Int ! F = p
      val _ = silent[okay.Async](okay.async(1))""")
    assertEquals(e, "",
      "the undeclared helper WAS refused -- the hole is closed and " +
      "chapter 19's THE LIMIT needs rewriting")
  }
}
