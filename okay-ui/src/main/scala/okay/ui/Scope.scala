package okay.ui

import okay.*
import okay.Delim

/**
 * Delim in Dialog, as an OPTION (specs/ui-toolkit.md, "Dialog
 * scopes") — nothing in Dialog changes: a scenario may run in the
 * `Delim + Dialog` row, where a PROMPT delimits a cancellable
 * sub-flow. Inside the scope no step threads Options; `cancel`
 * aborts to the named scope's boundary, and because prompts are
 * first-class and typed (Dybvig–Peyton Jones–Sabry — theory
 * textbook ch. 2), an inner scope can abort ACROSS its own boundary
 * to an outer one — the multi-prompt capability nested handlers
 * cannot express.
 *
 * The discipline that makes nesting work: `push` installs scopes,
 * ONE `run` (usually via `scoped`) erases the Delim row at the top.
 * Nested `run`s would be separate machines, and a prompt lives in
 * the machine that pushed it.
 */
object Scope {

  /** the row scoped scenarios live in */
  type Row = Delim + Dialog

  /** an ordinary Dialog step, lifted into the scoped row */
  def lift[A](p: A ! Dialog): A ! Row = !.widen[A, Dialog, Delim](p)

  /** install a cancellable scope: the body answers `A`, and `cancel`
   * against this scope's prompt exits it with the given value */
  def push[A](body: okay.Prompt[A] => A ! Row): A ! Row =
    val p = Delim.prompt[A]
    Delim.push(p)(body(p))

  /** exit the named scope immediately with `value` — no Option
   * threading on the steps in between, however deep */
  def cancel[A, R](p: okay.Prompt[R])(value: R): A ! Row =
    Delim.abort[R, A, Dialog](p)(value)

  /** erase the Delim row: after this it is an ordinary Dialog
   * program, running anywhere Dialog runs */
  def run[A](prog: A ! Row): A ! Dialog = Delim.run(prog)

  /** the common one-scope shape: push + run */
  def scoped[A](body: okay.Prompt[A] => A ! Row): A ! Dialog =
    run(push(body))
}
