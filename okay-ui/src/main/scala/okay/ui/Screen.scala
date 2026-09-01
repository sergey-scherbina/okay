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
  /** exit to the NAMED boundary (nav-pop-to-screen): every frame
   * above it is dropped untouched, and the boundary routes the
   * typed answer. The key's identity carries its type — the Prompt
   * argument, at the stack. */
  case PopTo[A](key: Nav.Key[A], answer: A) extends Nav

object Nav {

  /** a boundary's identity AND its answer type (the Prompt shape) */
  final class Key[A]
  def key[A]: Key[A] = new Key[A]

  /**
   * Wrap a screen as a NAMED boundary: `PopTo(key, a)` from anywhere
   * above drops every intervening frame — none of them stepped, they
   * are data — and `done(a)` routes at the boundary. ADDITIVE per
   * the adoption doctrine: plain Nav programs never meet this.
   *
   * The Scope pattern one level up, with the mechanism the stack
   * itself dictates: screens are REIFIED frames, so the boundary is
   * a stack marker and the exit is a drop — no captured
   * continuation, hence no Delim (recorded in specs/ui.md; a prompt
   * here would pay for capture the data structure already performs).
   */
  def boundary[A](k: Key[A], s: Screen)(done: A => Nav): Screen =
    new Boundary[A](k, s, done)

  private final class Boundary[A](val k: Key[A], inner: Screen,
                                  val done: A => Nav) extends Screen:
    def view: Ui = inner.view
    def step(e: Event): Nav = inner.step(e) match
      // the boundary survives its inner screen's ordinary moves
      case Stay(s) => Stay(new Boundary[A](k, s, done))
      case To(s) => To(new Boundary[A](k, s, done))
      case Run(p, s) => Run(p, new Boundary[A](k, s, done))
      case other => other

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
        case PopTo(k, a) => popTo(stack, k, a)

  /** drop to the named frame and let it route; an ABSENT boundary
   * changes nothing (total, like every fold here) — the stack is
   * data, and a name not on it names nothing */
  private def popTo[A](stack: List[Screen], k: Key[A], a: A)
  : (List[Screen], Vector[okay.![Event, okay.Async]]) =
    val at = stack.indexWhere {
      case b: Boundary[?] => b.k eq k
      case _ => false
    }
    if at < 0 then (stack, Vector.empty)
    else
      val b = stack(at).asInstanceOf[Boundary[A]]   // key identity carries the type
      val remaining = stack.drop(at)
      b.done(a) match
        case Stay(s) => (s :: remaining.tail, Vector.empty)
        case Push(next) => (next :: remaining, Vector.empty)
        case Pop => (remaining.tail, Vector.empty)
        case To(s) => (s :: remaining.tail, Vector.empty)
        case Run(prog, s) => (s :: remaining.tail, Vector(prog))
        case PopTo(k2, a2) => popTo(remaining, k2, a2)   // boundaries chain

  /** run a stack on a host: ends when the host closes (an emptied
   * stack shows nothing and ignores everything until then) */
  /** the door: the host ambient, the external source optional */
  def hosted(root: Screen)(using host: Host, s: Scheduler, cb: CanBlock): Unit ! Async =
    run(root)(host)

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
