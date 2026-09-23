package okay

/**
 * specs/atm-beyond-state-docs.md: answer-type modification (ATM)
 * beyond `PState` — Asai's typed printf (Asai, "On typing delimited
 * continuations: three new solutions to the printf problem", HOSC
 * 2009), built directly on `Cont`'s `shift`, no state cell anywhere.
 * `hole`'s shift is the whole mechanism: the continuation `k: T=>S`
 * IS "the rest of the format, once given a T", so answering with
 * `(t:T) => k(t)` makes the WHOLE program's answer type `T => S`
 * instead of `S` — exactly what `PState.set` does to thread a NEW
 * state type, except here what changes is "how many arguments this
 * format still needs", not a state.
 */
class TestPrintfAtm extends munit.FunSuite:

  /** literal text: no argument, the answer type is unchanged (S -> S) */
  def lit(s: String): Cont[Unit, String, String] = shift(k => s + k(()))

  /** one hole: the answer type grows an arrow, T => (whatever it was) —
   * `hole` does not render T itself; whoever consumes the bound value
   * (typically the `lit` that follows) decides how */
  def hole[T]: Cont[T, String, T => String] = shift(k => (t: T) => k(t))

  test("one hole: the format's answer type is Int => String, not String") {
    // "Score: " ++ show(hole) — bind's OWN result type keeps hole's R
    // (Int => String, unchanged by what follows) and takes ITS "A"
    // from whatever comes AFTER the hole (here, `lit`'s Unit)
    val fmt: Cont[Unit, String, Int => String] = hole[Int].flatMap(n => lit(s"Score: $n"))
    val asFunction: Int => String = fmt / (_ => "")
    assertEquals(asFunction(7), "Score: 7")
    assertEquals(asFunction(-3), "Score: -3")
  }

  test("a literal alone needs no argument: run with `/`, not `reset` — its A (Unit) is not its S (String)") {
    val fmt: Cont[Unit, String, String] = lit("no holes here")
    assertEquals(fmt / (_ => ""), "no holes here")
  }

  test("REFUTED: a second hole does not compose by ordinary flatMap nesting") {
    // the first attempt at this lane assumed two holes chain the same
    // way two literals do — `hole[Int].flatMap(a => hole[Int].flatMap(
    // b => lit(...)))` — and it does not typecheck, for a structural
    // reason bind's own signature states: `bind(c: Rep[A,S,R])(f: A =>
    // Rep[B,S2,S])` requires f's result to answer EXACTLY c's OWN `S`
    // (String here) — but a SECOND `hole` answers `Int => String`, not
    // `String`, so nothing that contains a further hole can sit inside
    // the first hole's continuation. One `shift` may grow the answer
    // type once; growing it AGAIN inside the same chain needs a
    // DIFFERENT construction (Asai's own paper spends its "three new
    // solutions" on exactly this multi-hole case), which this
    // documentation illustration does not attempt to rebuild.
    val e = compileErrors("""
      val h: okay.Cont[Int, String, Int => String] = okay.shift(k => (t: Int) => k(t))
      h.flatMap((a: Int) => h.flatMap((b: Int) => okay.shift[Unit, String, String](k => "x" + k(()))))
    """)
    assert(e.nonEmpty, "two holes composed by ordinary flatMap nesting, which the spec says should not typecheck")
  }
