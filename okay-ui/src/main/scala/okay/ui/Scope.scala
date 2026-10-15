package okay.ui


import okay.*
import okay.std.Escape

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
 * On the root's words (freer-on-core) the row is `Scope.Exit +
 * Dialog`, `Exit` being okay-std's one multi-prompt abort,
 * `okay.std.Escape`: each `push` is an `Escape.prompt` — an exit to
 * ITS prompt answers the scope, an exit to another is raised again
 * past the scope's boundary, where an enclosing `push` meets it —
 * and ONE `run` at the top erases the row: an exit no scope answered
 * is a `NoPrompt` there.
 */
object Scope {

  /** a scope's name: made by `push` (or `mark`), one per installed scope */
  type Prompt[A] = Escape.Prompt[A]

  /** THE ONE OPERATION: okay-std's multi-prompt abort, `Escape` */
  type Exit = Escape

  /** an exit no enclosing scope answered: its prompt was not pushed around it */
  type NoPrompt = Escape.NoPrompt

  /** the row scoped scenarios live in */
  type Row = Exit + Dialog

  /** an ordinary Dialog step, lifted into the scoped row */
  def lift[A](p: A ! Dialog): A ! Row = p.at[Row]

  /** install a cancellable scope: the body answers `A`, and `cancel`
   * against this scope's prompt exits it with the given value */
  def push[A](body: Prompt[A] => A ! Row): A ! Row = Escape.prompt[A, Row](body)

  /** exit the named scope immediately with `value` — no Option
   * threading on the steps in between, however deep */
  def cancel[A, R](p: Prompt[R])(value: R): A ! Row = Escape.escape[A, R](p)(value).at[Row]

  /** erase the Exit row: after this it is an ordinary Dialog
   * program, running anywhere Dialog runs */
  def run[A](prog: A ! Row): A ! Dialog = Escape.run[A, Row](prog)

  /** the common one-scope shape: push + run */
  def scoped[A](body: Prompt[A] => A ! Row): A ! Dialog =
    run(push(body))

  // ── the capability door (specs/context-functions.md, ctx-prompts)
  // ADDITIVE: the explicit forms above stay the floor. The prompt
  // becomes ambient; nesting resolves `exit` to the NEAREST scope
  // (inner using-params shadow outer — E8-verified), and a BOUND
  // prompt still crosses boundaries: multi-prompt kept, opt-in.
  //
  // THE AMBIENT EVIDENCE IS `Prompted`, NOT `Prompt`
  // (delim-doors-are-prompted, 2026-09-18). Asking for a bare prompt
  // as a GIVEN would prove nothing were one easy to make; `Prompted`'s
  // constructor is private to `Escape`, so the only way to hold one is
  // to be inside the scope that installed it, and an exit with a
  // forged one is a compile error.

  /** the evidence of being inside a scope: its prompt */
  type Prompted[A] = Escape.Prompted[A]

  /** install a scope whose prompt is ambient in the body */
  def mark[A](body: Prompted[A] ?=> A ! Row): A ! Row = Escape.prompted[A, Row](body)

  /** exit the nearest enclosing scope — or a named one, by binding */
  def exit[A, R](value: R)(using p: Prompted[R]): A ! Row = Escape.leave[A, R](value).at[Row]

  /** the one-scope capability form: mark + run */
  def bounded[A](body: Prompted[A] ?=> A ! Row): A ! Dialog =
    run(mark(body))
}
