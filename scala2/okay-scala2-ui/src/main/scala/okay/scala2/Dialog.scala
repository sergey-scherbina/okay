package okay.scala2

import okay.codec.Schema
import okay.ui.{Event, Nav, Screen, Ui}
import Rows.coerce

/**
 * okay-ui's scenarios and screens for Scala 2.13 (specs/scala2-facade.md,
 * stage 14).
 *
 * `Dialog` is an effect: `show(ui)` draws a screen and answers the next
 * event, so a whole scenario ("ask the name, then the age, then
 * confirm") is one program. Here it is a capability of `Eff`, run on a
 * host with `run`, or with no host at all with `replay`.
 *
 * `Nav` (screens as a stack) needs almost nothing: probed from scalac
 * 2.13, `okay.ui.Screen` can be IMPLEMENTED in Scala 2, and `Nav`'s
 * cases, `Nav.state`, `Nav.update` and `Nav.view` are readable and pure,
 * so a stack runs in `UiApp.run(Nav.state(root))(Nav.view)(Nav.update)`.
 * The one piece that is not readable is `Nav.screen`, whose `update`
 * answers the union `Nav | S`; `Screens.of` is it with an `Either`.
 */
sealed trait Dialog

object Dialog {

  /** draw `ui`, and answer the next event */
  def show(ui: Ui): Eff[Dialog, Event] = Eff.of(coerce(okay.ui.Dialog.show(ui)))

  /** a form for `A` drawn from its `Schema`, asking until it decodes;
   * `None` if the user leaves it */
  def ask[A](message: String)(using s: Schema[A]): Eff[Dialog, Option[A]] =
    Eff.of(coerce(okay.ui.Form.ask[A](message)))

  /** run the scenario on a host; `None` if the host closes first */
  def run[A](host: UiHost)(prog: Eff[Dialog, A]): Eff[Async, Option[A]] =
    Async.lift(okay.ui.Dialog.run(host.host)(coerce(prog.program)))

  /** run the scenario with no host: every screen it drew, and its answer
   * if the events were enough to finish it */
  def replay[A](prog: Eff[Dialog, A], events: Seq[Event]): (Vector[Ui], Option[A]) = {
    import okay.ui.Dialog.Running
    @scala.annotation.tailrec
    def go(r: Running[A], es: List[Event], drawn: Vector[Ui]): (Vector[Ui], Option[A]) = r match {
      case Running.Done(a) => (drawn, Some(a))
      case Running.Showing(ui, _) => es match {
        case e :: rest => go(okay.ui.Dialog.step(r, e), rest, drawn :+ ui)
        case Nil => (drawn :+ ui, None)
      }
    }
    go(okay.ui.Dialog.start(coerce(prog.program)), events.toList, Vector.empty)
  }
}

object Screens {

  /** a screen from a state, a view and an update that either moves on
   * (`Left(nav)`) or stays with a new state (`Right(s)`); `Nav.screen`
   * with an `Either` where it has a union */
  def of[S](init: S)(view: S => Ui)(update: (S, Event) => Either[Nav, S]): Screen = {
    val v = view
    new Screen {
      def view: Ui = v(init)
      def step(e: Event): Nav = update(init, e) match {
        case Left(nav) => nav
        case Right(next) => Nav.Stay(of(next)(v)(update))
      }
    }
  }
}
