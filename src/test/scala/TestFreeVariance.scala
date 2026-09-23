package okay

import okay.!.*

/**
 * specs/writer-covariance.md, free-answer-variance: `Free[F, +A]`. The
 * row stays invariant (a measured decision, free-row-variance); the
 * ANSWER is covariant, which is what deleted three identity-`map`
 * doors (SharedOnce.answer, Wf.up) — a program of a subtype IS a
 * program of the supertype, no node added.
 */
class TestFreeVariance extends munit.FunSuite:

  type F = Writer % String

  test("a program's answer widens by subtyping: no map, the same object") {
    val p: Int ! F = Writer.tell("x").map(_ => 1)
    val q: AnyVal ! F = p
    val r: Any ! F = q
    assert(r eq p, "widening the answer built a node")
    assertEquals(!.run(Writer.run[String, Any, okay.Pure](r)), (Seq("x"), 1))
  }

  test("a handler over a covariant GADT answers its program with no upcast") {
    // the SharedOnce shape: `Force[A] extends Once[Option[A]]` on a
    // covariant `Once[+A]` refines only `Option[A'] <: X`; with the
    // answer covariant that is enough for `X ! G`
    def answer[X](o: Once[X]): X ! F = o match
      case Once.Force(_) => pure[F, Option[Nothing]](None)
      case Once.Store(_, v) => pure[F, X](v)
    val p: Option[Int] ! F = answer(Once.Force(new Once.Handle[Int]))
    assertEquals(!.run(Writer.run[String, Option[Int], okay.Pure](p)), (Seq(), None))
  }

  test("the row is still invariant, on purpose") {
    assert(compileErrors("""
      val p: Int ! (okay.Writer % String) = okay.pure(1)
      val q: Int ! (okay.Writer % String + okay.Reader % Int) = p
      q""").nonEmpty, "a row widened by subtyping — free-row-variance's decision was undone")
  }
