package okay.ui

import okay.*
import okay.given
import okay.codec.Json

/**
 * Server-driven UI, transport-agnostic (specs/ui.md phase 3): the
 * server is a PURE stage — event lines in, patch lines out — so it
 * runs over channels, a Link, a WebSocket or stdio with machinery
 * that already exists, and is tested with none of them.
 *
 * The security rule is structural: THE SHOWN TREE IS THE CAPABILITY
 * LIST. An inbound event is untrusted input with a natural validator
 * — the tree the server itself just showed — and an event naming a
 * key that is not on it is dropped before update ever sees it. The
 * server can only be told about what it was prepared to hear.
 */
object Wire {

  /** may this event happen to this tree? Closed always may; keyed
   * events must name a key that is actually shown */
  def permitted(tree: Ui, e: Event): Boolean = e match
    case Event.Closed | Event.Key(_) | Event.Resized(_, _) => true
    case _ =>
      val keys = Ui.focusable(tree).collect {
        case Ui.Button(_, k) => k
        case Ui.Input(_, k, _) => k
        case Ui.Check(_, k, _) => k
        case Ui.Select(_, _, k) => k
      }.toSet
      e match
        case Event.Pressed(k) => keys(k)
        case Event.Edited(k, _) => keys(k)
        case Event.Toggled(k, _) => keys(k)
        case Event.Chosen(k, _) => keys(k)
        case _ => false

  /**
   * The server: the first line out is the FULL TREE, then the narrow
   * patches the diff makes. A damaged line is dropped (totality); a
   * forged key is dropped (the capability rule); Closed ends the
   * session, answering the final state.
   */
  def serve[S](init: S)(view: S => Ui)(update: (S, Event) => S): Stage[String, String, S] =
    def loop(s: S, shown: Ui): Stage[String, String, S] =
      Stage.await[String, String].flatMap {
        case None => pure(s)
        case Some(line) => WireJson.eventOf(Json.parse(line)) match
          case None => loop(s, shown)                       // damage is dropped
          case Some(Event.Closed) => pure(s)
          case Some(e) if !permitted(shown, e) => loop(s, shown)   // forged is dropped
          case Some(e) =>
            val s2 = update(s, e)
            val next = view(s2)
            val patches = Ui.diff(shown, next)
            def tell(ps: Vector[Patch]): Stage[String, String, Unit] = ps match
              case p +: more => Stage.tell[String, String](
                Json.print(WireJson.patchJson(p))).flatMap(_ => tell(more))
              case _ => pure(())
            tell(patches).flatMap(_ => loop(s2, next))
      }

    val first = view(init)
    Stage.tell[String, String](Json.print(WireJson.uiJson(first)))
      .flatMap(_ => loop(init, first))

  /**
   * The client: keep the tree, apply what arrives (a full tree or a
   * patch), render to any Host; the host's own events go back as
   * lines. Ends when the line stream does, or the user closes.
   */
  def client(host: Host)(lines: Source[String], send: String => Unit ! Async)
            (using Scheduler): Unit ! Async =
    var tree: Ui = Ui.Text("")

    def receive(rest: Source[String]): Unit ! Async =
      Writer.uncons[String, Unit, Async](rest).flatMap {
        case Left(_) => pure(())
        case Right((line, more)) =>
          val j = Json.parse(line)
          val next = WireJson.patchOf(j) match
            case Some(p) => Some(Ui.patch(tree, p))
            case None => WireJson.uiOf(j)          // a full tree
          next match
            case None => receive(more)             // damage is dropped
            case Some(t) =>
              tree = t
              host.render(t).flatMap(_ => receive(more))
      }

    def forward(rest: Source[Event]): Unit ! Async =
      Writer.uncons[Event, Unit, Async](rest).flatMap {
        case Left(_) => pure(())
        case Right((e, more)) =>
          send(Json.print(WireJson.eventJson(e))).flatMap(_ =>
            if e == Event.Closed then pure(()) else forward(more))
      }

    // both directions at once: rendering what arrives, sending what
    // the user does — two programs, one pair
    Async.par(receive(lines), forward(host.events)).map(_ => ())
}
