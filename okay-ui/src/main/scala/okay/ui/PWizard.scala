package okay.ui

import okay.*
import okay.Cont

/**
 * The TYPED wizard — PState's style as an alternative to the monadic
 * `Dialog`, not a replacement (specs/ui-toolkit.md, "The typed
 * wizard"). In a Dialog flow the collected values thread through
 * lambdas; here they thread through a STATE WHOSE TYPE GROWS, exactly
 * as in PState (State.scala; Atkey — theory textbook ch. 3): a step
 * is `Cont[A, S2 => Machine, S => Machine]` — it requires state S and
 * leaves state S2 — and Cont's flatMap composes the transitions, so
 * THE COMPILER enforces the step order. A step that needs the name
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

  /** a step: value A out, state S required, state S2 left behind */
  type Step[A, S, S2, R] = Cont[A, S2 => Machine[R], S => Machine[R]]

  /** show a view of the state-so-far; the event is the value, the
   * state passes through unchanged */
  def ask[S, R](view: S => Ui): Step[Event, S, S, R] =
    shift(k => s => Machine.Showing(view(s), e => k(e)(s)))

  /** grow (or reshape) the state — PState.set with the old state in
   * hand; the type records the transition */
  def mod[S, S2, R](f: S => S2): Step[Unit, S, S2, R] =
    shift(k => s => k(())(f(s)))

  /** read the state-so-far, PState.get verbatim */
  def get[S, R]: Step[S, S, S, R] =
    shift(k => s => k(s)(s))

  /**
   * The recurring composite: show a view, fold the event into a new
   * state — RETRYING on events the fold refuses (a validation loop in
   * four lines, the typed twin of Form.ask's retry-by-recursion).
   */
  def step[S, S2, R](view: S => Ui)(fold: (S, Event) => Option[S2]): Step[Unit, S, S2, R] =
    shift { k => s =>
      def loop(s: S): Machine[R] =
        Machine.Showing(view(s), e => fold(s, e) match
          case Some(s2) => k(())(s2)
          case None => loop(s))
      loop(s)
    }

  /** run to the machine: the wizard's answer is (final state, value) */
  def run[S, S2, A](s: S)(m: Cont[A, S2 => Machine[(S2, A)], S => Machine[(S2, A)]])
  : Machine[(S2, A)] =
    (m / (a => s2 => Machine.Done((s2, a))))(s)

  /** the bridge: a machine as an ordinary Dialog program — the typed
   * wizard runs anywhere Dialog runs, Dialog unchanged */
  def toDialog[A](m: Machine[A]): A ! Dialog = m match
    case Machine.Done(a) => pure(a)
    case Machine.Showing(ui, resume) =>
      Dialog.show(ui).flatMap(e => toDialog(resume(e)))
}
