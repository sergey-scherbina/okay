package okay.ui

import okay.*
import okay.freer.*


/**
 * The TYPED wizard — PState's style as an alternative to the monadic
 * `Dialog`, not a replacement (specs/ui-toolkit.md, "The typed
 * wizard"). In a Dialog flow the collected values thread through
 * lambdas; here they thread through a STATE WHOSE TYPE GROWS, exactly
 * as in PState (State.scala; Atkey — theory textbook ch. 3): a step
 * is a program on the indexed tree, `Freer[Op, S2, S, A]` — it requires
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
   * A wizard's operations, as DATA on the indexed tree (specs/cont-js-depth.md
   * stage 3a): the state's type moves from `R` to `S`, the answer is `X`.
   * Until 2026-10-02 a step was a shift body — `k => s => k(())(f(s))` —
   * and every step between two asks ran the rest NESTED inside it, a host
   * frame each (the census's state-passing shape). Now `run` is a loop.
   */
  enum Op[S, R, +X]:
    /** read the state, leaving its type */
    case Get[S]() extends Op[S, S, S]
    /** replace the state, moving its type from `S` to `T` */
    case Put[S, T](t: T) extends Op[T, S, Unit]
    /** show a view of the state; the event is the answer */
    case Show[S](view: S => Ui) extends Op[S, S, Event]

  /** a step: value A out, state S required, state S2 left behind; `R`, the
   * wizard's final answer, is kept so every signature written against
   * the shift road still reads the same */
  type Step[A, S, S2, R] = Freer[Op, S2, S, A]

  /** show a view of the state-so-far; the event is the value, the
   * state passes through unchanged */
  def ask[S, R](view: S => Ui): Step[Event, S, S, R] = Freer.Inject[Op, S, S, Event](Op.Show(view))

  /** read the state-so-far, PState.get verbatim */
  def get[S, R]: Step[S, S, S, R] = Freer.Inject[Op, S, S, S](Op.Get())

  /** grow (or reshape) the state; the type records the transition */
  def mod[S, S2, R](f: S => S2): Step[Unit, S, S2, R] =
    get[S, R].flatMap(s => Freer.Inject[Op, S2, S, Unit](Op.Put(f(s))))

  /**
   * The recurring composite: show a view, fold the event into a new
   * state — RETRYING on events the fold refuses (a validation loop in
   * four lines, the typed twin of Form.ask's retry-by-recursion). The
   * retry is the next step of the program, not a recursion of the host.
   */
  def step[S, S2, R](view: S => Ui)(fold: (S, Event) => Option[S2]): Step[Unit, S, S2, R] =
    get[S, R].flatMap(s => ask[S, R](view).flatMap(e => fold(s, e) match
      case Some(s2) => Freer.Inject[Op, S2, S, Unit](Op.Put(s2))
      case None => step[S, S2, R](view)(fold)))

  /** run to the machine: the wizard's answer is (final state, value). A
   * loop, the state in hand with its type moving; a `Show` suspends it
   * as the machine's `resume`, which continues the loop on a fresh call */
  def run[S, S2, A](s: S)(m: Step[A, S, S2, ?]): Machine[(S2, A)] = go(m, s)

  @scala.annotation.tailrec
  private def go[A, T, R](p: Freer[Op, T, R, A], r: R): Machine[(T, A)] = (p.resume: @unchecked) match
    case Freer.Return(a) => Machine.Done((r, a))
    case Freer.Inject(op) => go(Freer.Bind(Freer.Inject(op), (x: A) => Freer.Return(x)), r)
    case Freer.Bind(Freer.Inject(Op.Get()), k) => go(k(r), r)
    case Freer.Bind(Freer.Inject(Op.Put(t)), k) => go(k(()), t)
    case Freer.Bind(Freer.Inject(Op.Show(view)), k) => Machine.Showing(view(r), e => again(k(e), r))

  /** the loop resumed from the machine's callback (AGENTS.md: the one-line
   * wrapper that keeps `@tailrec` checking `go`) */
  private def again[A, T, R](p: Freer[Op, T, R, A], r: R): Machine[(T, A)] = go(p, r)

  /** the bridge: a machine as an ordinary Dialog program — the typed
   * wizard runs anywhere Dialog runs, Dialog unchanged */
  def toDialog[A](m: Machine[A]): A ! Dialog = m match
    case Machine.Done(a) => pure(a)
    case Machine.Showing(ui, resume) =>
      Dialog.show(ui).flatMap(e => toDialog(resume(e)))
}
