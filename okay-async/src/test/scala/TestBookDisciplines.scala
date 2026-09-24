package okay

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

  type P = okay.Pure

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

  test("the SAME mechanism in OneMachine: declared, so the call site answers") {
    def oneMachineHelper[F[+_]](p: Int ! Delim + F)(using Delim.OneMachine[F]): Int ! F =
      Delim.run(p)
    // at a clean row it resolves
    assertEquals(!.run(oneMachineHelper[P](okay.pure(1))), 1)
    // at a row that already holds a machine the CALLER is refused
    val e = compileErrors(
      "oneMachineHelper[okay.Delim + okay.Pure](okay.pure(1))")
    assert(e.nonEmpty, "a second machine satisfied OneMachine")
  }

  test("THE SHARED HOLE: a helper that declares NOTHING is never asked") {
    // No witness in the signature, so nothing propagates and nothing
    // is checked. This compiles -- and that is precisely chapter 19's
    // THE LIMIT, here to show it is not specific to Delim: an
    // undeclared obligation is an unasked question.
    val e = compileErrors("""
      def silent[F[+_]](p: Int ! F): Int ! F = p
      val _ = silent[okay.Async](okay.async(1))""")
    assertEquals(e, "",
      "the undeclared helper WAS refused -- the hole is closed and " +
      "chapter 19's THE LIMIT needs rewriting")
  }
}
