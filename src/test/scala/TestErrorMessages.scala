package okay

/**
 * The wording is pinned (specs/error-messages.md): each assertion
 * requires an ACTIONABLE substring, so a rewrite that loses the
 * recipe fails here, not in a user's terminal.
 */
class TestErrorMessages extends munit.FunSuite {

  given Monad[Option] with
    override def pure[A](a: A): Option[A] = Some(a)
    extension [A](m: Option[A])
      override def flatMap[B](f: A => Option[B]): Option[B] = m.flatMap(f)
  given Monad[List] with
    override def pure[A](a: A): List[A] = List(a)
    extension [A](m: List[A])
      override def flatMap[B](f: A => List[B]): List[B] = m.flatMap(f)

  test("missing Monad names the given import and the row overlap") {
    val e = compileErrors("summon[okay.Monad[java.util.Optional]] ")
    assert(e.contains("import okay.given"), e)
    assert(e.contains("Choose"), e)
  }

  test("missing Handler shows the union recipe") {
    val e = compileErrors("summon[okay.Handler[Option]] ")
    assert(e.contains("Handler.union"), e)
  }

  // TypeableK and CanBlock cannot be PINNED from inside package okay:
  // both always resolve here (the derivations / the JVM default
  // CanBlock — verified by probe, specs/error-messages.md Results).
  // Their @implicitNotFound texts serve downstream scopes.

  test("missing CanTry names the lazy-monad trap and the one-line strict recipe") {
    val e = compileErrors("summon[okay.CanTry[[X] =>> okay.Eff[okay.Pure, X]]] ")
    assert(e.contains("CanTry.strict"), e)
    assert(e.contains("LAZY"), e)
  }

  test("missing Direct.Effect shows the one-line registration") {
    val e = compileErrors("summon[okay.Direct.Effect[List]] ")
    assert(e.contains("given Direct.Effect"), e)
  }

  test("DirectCtx outside a block points back at direct { }") {
    val e = compileErrors("summon[okay.Direct.DirectCtx[Option]] ")
    assert(e.contains("direct[F] { ... }"), e)
  }

  test("the direct macro's refusals keep naming the workaround") {
    val lam = compileErrors(
      "import okay.Direct.*; okay.Direct.direct[List] { List(1).filter(i => List(i > 0).reflect) }(using summon[Monad[List]]) ")
    assert(lam.contains("bind the marked value to a val"), lam)
    val neither = compileErrors(
      "import okay.Direct.*; okay.Direct.direct[Option] { List(1).reflect }(using summon[Monad[Option]]) ")
    assert(neither.contains("neither"), neither)
  }

  test("the sanctioned channel spellings compile (the discard lint itself lives in build.sbt)") {
    // `c.send(1)` in statement position, as a Unit def's body, or
    // eta-expanded into a Unit function is a compile ERROR — a -Wconf
    // escalation in build.sbt, verified with a probe file (the three
    // shapes error, Sim's nested-`!` Queue does not). compileErrors
    // cannot see lints (they are reported after typer), so the
    // negative half is not testable here; the positive half is.
    assertEquals(compileErrors("val c = okay.Channel[Int](); def f(): Unit = c.offer(1): Unit"), "")
    assertEquals(compileErrors("val c = okay.Channel[Int](); val p: Unit ! okay.Async = c.send(1).map(_ => ())"), "")
  }
}
