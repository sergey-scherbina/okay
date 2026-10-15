package okay.ui


import okay.*
import okay.core.{+:, Eff}
import okay.one.Cont
import okay.one.handler.Deep
import okay.std.PState


/**
 * The TYPED wizard — PState's style as an alternative to the monadic
 * `Dialog`, not a replacement (specs/ui-toolkit.md, "The typed
 * wizard"). In a Dialog flow the collected values thread through
 * lambdas; here they thread through a STATE WHOSE TYPE GROWS, exactly
 * as in PState (State.scala; Atkey — theory textbook ch. 3): a step
 * is a typestate program, `PState.Indexed[A, S2, S, _]` — it requires
 * state S and leaves state S2 — and its flatMap composes the
 * transitions, so THE COMPILER enforces the step order. A step that needs the name
 * cannot run before the step that collects it; misordering is a type
 * error, not a review comment.
 *
 * The machine the answer type threads is the same defunctionalized
 * suspend/resume Dialog.Running is: Showing(ui, resume) | Done. And
 * `toDialog` bridges a finished wizard into an ordinary Dialog
 * program, so a typed wizard runs anywhere Dialog runs — over any
 * Host, or as a Screen — with nothing in Dialog changed.
 */
object PWizard {

  /** the suspended machine: what the wizard's answer type threads */
  enum Machine[A]:
    case Showing(ui: Ui, resume: Event => Machine[A])
    case Done(a: A)

  /**
   * The wizard's own operation, beside the state's (`PState.Indexed`, the typestate as data on the kernel): a view
   * shown, the event the answer. Until 2026-10-02 a step was a shift body — `k => s => k(())(f(s))` — and every
   * step between two asks ran the rest NESTED inside it, a host frame each; then the indexed tree with a loop of its
   * own; now the kernel's program: the state answered in place by `PState`'s handler, a `Show` taken by a `Deep`
   * handler whose `k` is the machine's `resume`.
   */
  enum Op[+X]:
    /** show a view; the event is the answer */
    case Show(ui: Ui) extends Op[Event]

  /** the row a wizard's program is over, past the state */
  type Row = Op +: okay.core.Pure

  /** a step: value A out, state S required, state S2 left behind; `R`, the
   * wizard's final answer, is kept so every signature written against
   * the shift road still reads the same */
  type Step[A, S, S2, R] = PState.Indexed[A, S2, S, Row]

  /** show a view of the state-so-far; the event is the value, the
   * state passes through unchanged */
  def ask[S, R](view: S => Ui): Step[Event, S, S, R] =
    get[S, R].flatMap(s => PState.Indexed.perform[Op, S, Row, Event](Op.Show(view(s))))

  /** read the state-so-far, PState.get verbatim */
  def get[S, R]: Step[S, S, S, R] = PState.Indexed.get[S, Row]

  /** grow (or reshape) the state; the type records the transition */
  def mod[S, S2, R](f: S => S2): Step[Unit, S, S2, R] =
    get[S, R].flatMap(s => PState.Indexed.put[S, S2, Row](f(s)).map(_ => ()))

  /**
   * The recurring composite: show a view, fold the event into a new
   * state — RETRYING on events the fold refuses (a validation loop in
   * four lines, the typed twin of Form.ask's retry-by-recursion). The
   * retry is the next step of the program, not a recursion of the host.
   */
  def step[S, S2, R](view: S => Ui)(fold: (S, Event) => Option[S2]): Step[Unit, S, S2, R] =
    get[S, R].flatMap(s => ask[S, R](view).flatMap(e => fold(s, e) match
      case Some(s2) => PState.Indexed.put[S, S2, Row](s2).map(_ => ())
      case None => step[S, S2, R](view)(fold)))

  /** run to the machine: the wizard's answer is (final state, value). The
   * state answered in place, its type moving; a `Show` suspends the run as
   * the machine's `resume`, which runs the rest — the state as it was — on a
   * fresh call */
  def run[S, S2, A](s: S)(m: Step[A, S, S2, ?]): Machine[(S2, A)] =
    PState.Indexed.handle(m)(s).handle(Showing[(S2, A)]()).run

  /** a `Show` as the machine's `Showing`, its `k` the `resume`; the end `Done` */
  private final class Showing[B] extends Deep[Op, okay.core.Pure, B, Machine[B]]:
    def ret(b: B): Eff[okay.core.Pure, Machine[B]] = Cont.pure(Machine.Done(b))
    def apply[X](op: Op[X], k: X => Eff[okay.core.Pure, Machine[B]]): Eff[okay.core.Pure, Machine[B]] = op match
      case Op.Show(ui) => Cont.pure(Machine.Showing(ui, e => k(e).run))

  /** the bridge: a machine as an ordinary Dialog program — the typed
   * wizard runs anywhere Dialog runs, Dialog unchanged */
  def toDialog[A](m: Machine[A]): A ! Dialog = m match
    case Machine.Done(a) => pure(a)
    case Machine.Showing(ui, resume) =>
      Dialog.show(ui).flatMap(e => toDialog(resume(e)))
}
