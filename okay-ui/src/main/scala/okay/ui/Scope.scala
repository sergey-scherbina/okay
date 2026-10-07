package okay.ui


import okay.freer.*


import okay.freer.Shift

/**
 * Shift in Dialog, as an OPTION (specs/ui-toolkit.md, "Dialog
 * scopes") — nothing in Dialog changes: a scenario may run in the
 * `Shift % ? + Dialog` row, where a PROMPT delimits a cancellable
 * sub-flow. Inside the scope no step threads Options; `cancel`
 * aborts to the named scope's boundary, and because prompts are
 * first-class and typed (Dybvig–Peyton Jones–Sabry — theory
 * textbook ch. 2), an inner scope can abort ACROSS its own boundary
 * to an outer one — the multi-prompt capability nested handlers
 * cannot express.
 *
 * The discipline that makes nesting work: `push` installs scopes,
 * ONE `run` (usually via `scoped`) erases the Shift row at the top.
 * Nested `run`s would be separate machines, and a prompt lives in
 * the machine that pushed it.
 */
object Scope {

  /** the row scoped scenarios live in */
  type Row = Shift % ? + Dialog

  /** an ordinary Dialog step, lifted into the scoped row */
  def lift[A](p: A ! Dialog): A ! Row = !.widen[A, Dialog, Shift % ?](p)

  /** install a cancellable scope: the body answers `A`, and `cancel`
   * against this scope's prompt exits it with the given value */
  def push[A](body: okay.freer.Prompt[A] => A ! Row): A ! Row =
    val p = Shift.prompt[A]
    Shift.push(p)(body(p))

  /** exit the named scope immediately with `value` — no Option
   * threading on the steps in between, however deep */
  def cancel[A, R](p: okay.freer.Prompt[R])(value: R): A ! Row =
    Shift.abort[R, A, Dialog](p)(value)

  /** erase the Shift row: after this it is an ordinary Dialog
   * program, running anywhere Dialog runs */
  def run[A](prog: A ! Row): A ! Dialog = Shift.run(prog)

  /** the common one-scope shape: push + run */
  def scoped[A](body: okay.freer.Prompt[A] => A ! Row): A ! Dialog =
    run(push(body))

  // ── the capability door (specs/context-functions.md, ctx-prompts)
  // ADDITIVE: the explicit forms above stay the floor. The prompt
  // becomes ambient; nesting resolves `exit` to the NEAREST scope
  // (inner using-params shadow outer — E8-verified), and a BOUND
  // prompt still crosses boundaries: multi-prompt kept, opt-in.
  //
  // THE AMBIENT EVIDENCE IS `Shift.Prompted`, NOT `Prompt`
  // (delim-doors-are-prompted, 2026-09-18). A `Prompt[R]` is one line
  // to make — `Shift.prompt[R]` — so asking for one as a GIVEN proves
  // nothing: a caller could summon a prompt that was never pushed and
  // `exit` would compile and then fail at runtime with `NoPrompt`.
  // `Prompted`'s constructor is private to `Shift`, so the only way to
  // hold one is to be inside the scope that installed it, and the same
  // mistake is now a compile error. The explicit forms above are
  // unaffected: there the prompt is the one `push` handed you.

  /** install a scope whose prompt is ambient in the body */
  def mark[A](body: Shift.Prompted[A] ?=> A ! Row): A ! Row =
    Shift.scope[A, Dialog](body)

  /** exit the nearest enclosing scope — or a named one, by binding */
  def exit[A, R](value: R)(using p: Shift.Prompted[R]): A ! Row =
    Shift.abort[R, A, Dialog](p.prompt)(value)

  /** the one-scope capability form: mark + run */
  def bounded[A](body: Shift.Prompted[A] ?=> A ! Row): A ! Dialog =
    run(mark(body))
}
