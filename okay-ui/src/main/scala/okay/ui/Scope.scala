package okay.ui


import okay.*

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
 * Dialog`: an exit is ONE operation of `Exit`, naming its prompt.
 * Each `push` handles `Exit` with an aborting handler (the rest of
 * the scope is dropped, which is what an exit is): an exit to ITS
 * prompt answers the scope; an exit to another is raised again past
 * the scope's boundary, where an enclosing `push` meets it. So an
 * inner scope aborts across its boundary to an outer one, and ONE
 * `run` at the top erases the row — an exit no scope answered is a
 * `NoPrompt` there.
 */
object Scope {

  /** a scope's name: made by `push` (or `mark`), one per installed scope */
  final class Prompt[A] private[Scope] ()

  /** THE ONE OPERATION: leave the scope of `prompt` with `value` */
  enum Exit[X]:
    case Leave[R](prompt: Prompt[R], value: R) extends Exit[Nothing]

  /** an exit no enclosing scope answered: its prompt was not pushed around it */
  final class NoPrompt extends IllegalStateException("Scope: an exit to a scope that is not around it")

  /** the row scoped scenarios live in */
  type Row = Exit + Dialog

  /** an ordinary Dialog step, lifted into the scoped row */
  def lift[A](p: A ! Dialog): A ! Row = p.at[Row]

  /** what a scope's body ended with: its answer, or an exit (to it or past it) */
  private type Ended[A] = Either[Exit.Leave[?], A]

  /** the body run to its answer or its first exit — the rest of it dropped */
  private val ending: Handler[Exit, Ended] = Handler.aborting[Exit, Ended]([A] => (a: A) => Right(a))(
    [A, X] => (op: Exit[X]) => op match
      case l: Exit.Leave[r] => Left(l))

  /** install a cancellable scope: the body answers `A`, and `cancel`
   * against this scope's prompt exits it with the given value */
  def push[A](body: Prompt[A] => A ! Row): A ! Row =
    val p = Prompt[A]()
    body(p).handle(ending).at[Row].flatMap {
      case Right(a) => pure[A, Row](a)
      case Left(l) => answer(p, l) match
        case Some(a) => pure[A, Row](a)
        // past this scope: raised again outside it, to an enclosing one
        case None => effect[Exit, Nothing](l).at[Row]
    }

  /** the exit's value when it is to `p` */
  private def answer[A](p: Prompt[A], l: Exit.Leave[?]): Option[A] =
    // THE CAST, and why it is sound: a `Leave` names its prompt with its
    // value's type (`Leave[R](Prompt[R], R)`), so one whose prompt is `p`
    // carries an `A`
    if l.prompt eq p then Some(l.value.asInstanceOf[A]) else None

  /** exit the named scope immediately with `value` — no Option
   * threading on the steps in between, however deep */
  def cancel[A, R](p: Prompt[R])(value: R): A ! Row =
    effect[Exit, Nothing](Exit.Leave(p, value)).at[Row]

  /** erase the Exit row: after this it is an ordinary Dialog
   * program, running anywhere Dialog runs */
  def run[A](prog: A ! Row): A ! Dialog = prog.handle(ending).map {
    case Right(a) => a
    case Left(_) => throw NoPrompt()
  }

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
  // constructor is private to `Scope`, so the only way to hold one is
  // to be inside the scope that installed it, and an exit with a
  // forged one is a compile error.

  /** the evidence of being inside a scope: its prompt */
  final class Prompted[A] private[Scope] (val prompt: Prompt[A])

  /** install a scope whose prompt is ambient in the body */
  def mark[A](body: Prompted[A] ?=> A ! Row): A ! Row =
    push[A](p => body(using Prompted(p)))

  /** exit the nearest enclosing scope — or a named one, by binding */
  def exit[A, R](value: R)(using p: Prompted[R]): A ! Row =
    cancel[A, R](p.prompt)(value)

  /** the one-scope capability form: mark + run */
  def bounded[A](body: Prompted[A] ?=> A ! Row): A ! Dialog =
    run(mark(body))
}
