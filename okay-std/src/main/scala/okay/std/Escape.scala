package okay.std

import okay.{!, Handler, Within, effect, pure}
import okay.core.Split

/**
 * A MULTI-PROMPT ABORT, ONE WORD (okay-freer's Shift escape on the kernel's words): a `prompt` names a scope, and
 * `escape(p)(a)` leaves the scope of `p` at once with `a` — the rest of it dropped, no Option threaded through the
 * steps between, however deep. Prompts are first-class and typed, so an inner scope can be left ACROSS its own
 * boundary to an outer one: each scope answers a leave to ITS prompt and raises any other again past its boundary,
 * where an enclosing scope meets it. A leave no scope answered fails by name (`NoPrompt`) at `run`.
 *
 * One effect, `Escape`, with one operation, `Leave`; each scope is the kernel's `Aborting` handler of it. Its users:
 * okay-ui's `Scope` (cancellable dialog sub-flows) and okay-llm's `Cut` (a validator cutting a token stream).
 */
enum Escape[X]:
  /** leave the scope of `prompt` with `value` */
  case Leave[R](prompt: Escape.Prompt[R], value: R) extends Escape[Nothing]

object Escape:

  /** a scope's name: made by `prompt` (or `prompted`) alone, one per installed scope */
  final class Prompt[A] private[Escape] ()

  /** the evidence of being inside a scope: its prompt. The constructor is private, so the only way to hold one is to
   * be inside the scope that installed it — a `leave` with a forged one does not compile */
  final class Prompted[A] private[Escape] (val prompt: Prompt[A])

  /** a leave no enclosing scope answered: its prompt was not installed around it */
  final class NoPrompt extends IllegalStateException("Escape: a leave to a prompt that is not around it")

  /** what a scope's body ended with: its answer, or a leave (to it or past it) */
  private type Ended[A] = Either[Leave[?], A]

  /** the body run to its answer or its first leave — the rest of it dropped */
  private val ending: Handler[Escape, Ended] = Handler.aborting[Escape, Ended]([A] => (a: A) => Right(a))(
    [A, X] => (op: Escape[X]) => op match
      case l: Leave[r] => Left(l))

  /** the leave's value when it is to `p` */
  private def answer[A](p: Prompt[A], l: Leave[?]): Option[A] =
    // THE CAST, and why it is sound: a `Leave` names its prompt with its value's type (`Leave[R](Prompt[R], R)`),
    // so one whose prompt is `p` carries an `A`
    if l.prompt eq p then Some(l.value.asInstanceOf[A]) else None

  /** leave the scope of `p` with `value` */
  def escape[A, R](p: Prompt[R])(value: R): A ! Escape = effect[Escape, Nothing](Leave(p, value))

  /** leave the nearest enclosing scope (or a named one, by binding its evidence) with `value` */
  def leave[A, R](value: R)(using p: Prompted[R]): A ! Escape = escape[A, R](p.prompt)(value)

  /** install a scope: the body answers `A`, and a leave to its prompt answers the scope; a leave to another prompt
   * is raised again past it */
  def prompt[A, Es[_]](body: Prompt[A] => A ! Es)
                      (using s: Split[Es, Escape], in: Within[s.Rest, Es], up: Within[Escape, Es]): A ! Es =
    val p = Prompt[A]()
    body(p).handle(ending)(using s).at[Es](using in).flatMap {
      case Right(a) => pure[A, Es](a)
      case Left(l) => answer(p, l) match
        case Some(a) => pure[A, Es](a)
        // past this scope: raised again outside it, to an enclosing one
        case None => effect[Escape, Nothing](l).at[Es](using up)
    }

  /** install a scope whose prompt is ambient in the body */
  def prompted[A, Es[_]](body: Prompted[A] ?=> A ! Es)
                        (using s: Split[Es, Escape], in: Within[s.Rest, Es], up: Within[Escape, Es]): A ! Es =
    prompt[A, Es](p => body(using Prompted(p)))

  /** erase `Escape`: a leave no scope answered fails by name */
  def run[A, Es[_]](prog: A ! Es)(using s: Split[Es, Escape]): A ! s.Rest = prog.handle(ending)(using s).map {
    case Right(a) => a
    case Left(_) => throw NoPrompt()
  }
