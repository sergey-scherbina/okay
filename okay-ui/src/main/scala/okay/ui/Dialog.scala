package okay.ui

import okay.*
import okay.given

/**
 * The imperative half of a UI, as an effect: show, await, validate,
 * branch — a wizard is a PROGRAM, and retry is recursion, not a
 * combinator. One operation, like Writer; the GADT gives back that a
 * Show answers an Event, so nothing casts.
 *
 * Two ways to run one scenario, and that is the point:
 *   - `run(host)` drives it standalone — render on Show, resume with
 *     the next event;
 *   - `screen` turns it into an ordinary (view, update) pair for
 *     `Ui.run`: the CONTINUATION is the screen's state, one event
 *     steps it — the handler holding the rest of the program until
 *     the awaited event arrives, which is what delimited
 *     continuations are for.
 */
enum Dialog[+A]:
  case Show(ui: Ui) extends Dialog[Event]

object Dialog {

  given okay.TypeableK[Dialog] = okay.typeableK(classOf[Dialog[?]])

  /** show the tree; the answer is what the user did next */
  inline def show(ui: Ui): Event ! Dialog = effect(Show(ui))

  import okay.!.{Bind, Effect, Pure, resume}

  /**
   * A scenario, stepped to its next question: either it is done, or
   * it shows a tree and holds the continuation. `Closed` is not fed
   * to the continuation — a scenario that did not reach its ending
   * has no ending, which is why both runners answer Option.
   */
  enum Running[A]:
    case Showing(ui: Ui, k: Event => A ! Dialog)
    case Done(a: A)

  def start[A](prog: A ! Dialog): Running[A] = (prog.resume: @unchecked) match
    case Pure(a) => Running.Done(a)
    // Dialog is covariant: a bare Show's answer type is above Event,
    // so resuming with the event itself is an upcast, not a cast
    case Effect(Show(ui)) => Running.Showing(ui, e => pure((e: A)))
    case Bind(Effect(Show(ui)), k) => Running.Showing(ui, k)

  def step[A](r: Running[A], e: Event): Running[A] = r match
    case Running.Showing(_, k) => start(k(e))
    case done => done

  /** drive a scenario over any Host: render on Show, resume with the
   * next event; the host ending (or Closed) before the scenario does
   * answers None */
  /** the door (ctx-everywhere): the app's ONE host is ambient */
  def hosted[A](prog: A ! Dialog)(using host: Host): Option[A] ! Async =
    run(host)(prog)

  def run[A](host: Host)(prog: A ! Dialog): Option[A] ! Async =
    def loop(r: Running[A], rest: Source[Event]): Option[A] ! Async = r match
      case Running.Done(a) => pure(Some(a))
      case Running.Showing(ui, _) =>
        host.render(ui).flatMap(_ =>
          Writer.uncons[Event, Unit, Async](rest).flatMap {
            case Left(_) => pure(None)
            case Right((Event.Closed, _)) => pure(None)
            case Right((e, more)) => loop(step(r, e), more)
          })

    loop(start(prog), host.events)

  /**
   * The scenario AS a screen for `Ui.run`: state is `Running[A]`,
   * update steps the continuation, `done` is what to show once the
   * program has its answer.
   */
  def view[A](done: A => Ui)(r: Running[A]): Ui = r match
    case Running.Showing(ui, _) => ui
    case Running.Done(a) => done(a)

  def update[A](r: Running[A], e: Event): Running[A] = step(r, e)
}
