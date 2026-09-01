package okay.ui

import okay.*

/**
 * A screen is CODATA: what it shows, and what it becomes when the
 * user acts. That one sentence settles the heterogeneous-state
 * problem without an existential — each screen closes over its own
 * state, and a parent that wants an answer from a child passes a
 * CONTINUATION into it.
 *
 * Closures are fine HERE and forbidden in the Ui tree, and the line
 * between the two is exactly the wire: screens never cross it, trees
 * do. (specs/ui.md, phase 2.)
 */
trait Screen:
  def view: Ui
  def step(e: Event): Nav

/** what a step decides: stay (stepped), cover, reveal, or replace */
enum Nav:
  case Stay(s: Screen)
  case Push(next: Screen)
  case Pop
  case To(s: Screen)
  /** the effect slot: stay on `s`, LAUNCH `prog` — its Event answer
   * re-enters the fold like any other event (specs/ui.md, "The
   * effect slot"). The program is data; the loop runs it. */
  case Run(prog: okay.![Event, okay.Async], s: Screen)

object Nav {

  /** a screen from a plain (state, view, update) triple — update may
   * answer a Nav, or just the next state (which means Stay) */
  def screen[S](init: S)(v: S => Ui)(u: (S, Event) => Nav | S): Screen = new Screen:
    def view: Ui = v(init)
    def step(e: Event): Nav = u(init, e) match
      case n: Nav => n
      case s => Stay(screen(s.asInstanceOf[S])(v)(u))

  /**
   * The stack as ONE application for `Ui.run`: the state is the
   * stack, events route to the top, the view is the top's. An empty
   * stack is the end — Pop on the last screen closes the app, which
   * is what a back button on the first screen means.
   */
  def state(root: Screen): List[Screen] = List(root)

  def view(stack: List[Screen]): Ui = stack match
    case top :: _ => top.view
    case Nil => Ui.Text("")

  def update(stack: List[Screen], e: Event): List[Screen] =
    updateCmd(stack, e)._1

  /** the same step, commands out: `Run` stays on its screen and
   * hands the loop the program to launch */
  def updateCmd(stack: List[Screen], e: Event): (List[Screen], Vector[okay.![Event, okay.Async]]) =
    stack match
      case Nil => (Nil, Vector.empty)
      case top :: rest => top.step(e) match
        case Stay(s) => (s :: rest, Vector.empty)
        case Push(next) => (next :: top :: rest, Vector.empty)
        case Pop => (rest, Vector.empty)
        case To(s) => (s :: rest, Vector.empty)
        case Run(prog, s) => (s :: rest, Vector(prog))

  /** run a stack on a host: ends when the host closes (an emptied
   * stack shows nothing and ignores everything until then) */
  def run(root: Screen)(host: Host, external: Source[Event] = pure(()))
         (using Scheduler, CanBlock): Unit ! Async =
    Ui.runCmd(state(root))(view)(updateCmd)(host, external).map(_ => ())

  /**
   * A Dialog scenario as a pushable screen — the imperative flow and
   * the navigation stack meet: push the wizard, and its ANSWER
   * arrives through `done`, which decides where to go (usually a
   * `To`/`Pop` the parent closed over).
   */
  def scenario[A](prog: A ! Dialog)(done: A => Nav): Screen =
    def of(r: Dialog.Running[A]): Screen = new Screen:
      def view: Ui = r match
        case Dialog.Running.Showing(ui, _) => ui
        case Dialog.Running.Done(_) => Ui.Text("")
      def step(e: Event): Nav = Dialog.step(r, e) match
        case Dialog.Running.Done(a) => done(a)
        case next => Nav.Stay(of(next))

    Dialog.start(prog) match
      case Dialog.Running.Done(a) =>
        // a scenario that never shows: its answer routes at the first
        // step — a Screen must show SOMETHING until then
        new Screen:
          def view: Ui = Ui.Text("")
          def step(e: Event): Nav = done(a)
      case running => of(running)
}
