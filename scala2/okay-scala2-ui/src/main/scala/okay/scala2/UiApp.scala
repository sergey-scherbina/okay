package okay.scala2

import okay.given
import okay.ui.{Event, Ui}

/**
 * okay-ui for Scala 2.13 (specs/scala2-facade.md, stage 10).
 *
 * Probed first: okay-ui's tree `Ui`, its `Event`s, `Frame` (the pure
 * text renderer), `Form`, `Swing` and `Terminal` are readable from
 * scalac 2.13, so a Scala 2 application builds its view with okay-ui's
 * own constructors (`Ui.Column`, `Ui.Button`, ...) and matches on its
 * events (`Event.Pressed(key)`). What it cannot use is the LOOP,
 * `Ui.run`, because it answers a program, and a `Host`, whose methods
 * answer programs. So this file gives the loop as an `Eff`, and hosts.
 *
 * The object is `UiApp` and not `App`, because a Scala 2 file that
 * imports `okay.scala2._` would otherwise have its `object Main extends
 * App` resolve to this object instead of `scala.App`.
 */
object UiApp {

  /**
   * The Elm loop: render `view(state)`, fold each event through
   * `update`, render again when the view changed, stop at
   * `Event.Closed`; the answer is the final state. It is okay-ui's own
   * `Ui.run`.
   */
  def run[S](init: S)(view: S => Ui)(update: (S, Event) => S)(host: UiHost): Eff[Async, S] =
    Async.lift(Ui.run(init)(view)(update)(host.host))

  /** the same, with events from the world merged in beside the user's:
   * a timer, a socket, a channel */
  def runWith[S](init: S)(view: S => Ui)(update: (S, Event) => S)(host: UiHost, external: Source[Event]): Eff[Async, S] =
    Async.lift(Ui.run(init)(view)(update)(host.host, external.core))

  /** in a Swing window of its own, closed when the loop ends */
  def window[S](title: String)(init: S)(view: S => Ui)(update: (S, Event) => S): Eff[Async, S] =
    Async.lift(okay.ui.Swing.window(title)(h => Ui.run(init)(view)(update)(h)))
}

/** where a view is drawn and events come from, as okay-ui's `Host` */
final class UiHost private[scala2] (private[scala2] val host: okay.ui.Host)

object UiHost {

  /** this process's terminal: raw mode, keys, painting (JVM) */
  def terminal(): UiHost = new UiHost(okay.ui.Terminal.host())

  /** a Swing container the caller owns */
  def swing(root: java.awt.Container): UiHost = new UiHost(okay.ui.Swing.host(root))
}

/**
 * A host for tests: it delivers `events` in order and then `Closed`
 * (or, from `ScriptedHost.open`, no `Closed`, so something else must
 * end the loop), and keeps every frame it was asked to draw. With
 * `Frame.render` a frame is plain text, so what the user would see is
 * a string to assert on.
 */
final class ScriptedHost private (events: Seq[Event], close: Boolean) {
  private val drawn = new java.util.concurrent.ConcurrentLinkedQueue[Ui]

  val host: UiHost = new UiHost(new okay.ui.Host {
    def render(ui: Ui): okay.![Unit, okay.Async] = okay.async { val _ = drawn.add(ui) }
    def events: okay.Source[Event] =
      okay.Source.of((if (close) ScriptedHost.this.events :+ Event.Closed else ScriptedHost.this.events).toList)
  })

  /** every frame drawn so far, in order */
  def frames: Vector[Ui] = { import scala.jdk.CollectionConverters._; drawn.asScala.toVector }
}

object ScriptedHost {
  def apply(events: Event*): ScriptedHost = new ScriptedHost(events, close = true)

  /** the events, and no `Closed` after them */
  def open(events: Event*): ScriptedHost = new ScriptedHost(events, close = false)
}
