package okay.ui

import okay.*
import Protocol.Msg

/**
 * Server-driven UI, transport-agnostic (specs/ui.md phase 3,
 * specs/frontend.md stage 1): the server is a PURE stage — lines in,
 * lines out — so it runs over channels, a Link, a WebSocket or stdio
 * with machinery that already exists, and is tested with none of
 * them. The lines are `Protocol`'s: one derived definition, JSON here,
 * CBOR for a byte transport.
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
      val keys = Ui.keys(tree)
      e match
        case Event.Pressed(k) => keys(k)
        case Event.Edited(k, _) => keys(k)
        case Event.Toggled(k, _) => keys(k)
        case Event.Chosen(k, _) => keys(k)
        case _ => false

  /**
   * The server. The client's first line is its `Hello`, naming the
   * semantic nodes it draws; everything else is LOWERED before it is
   * sent. Then the FULL TREE, then the narrow patches the diff makes.
   * A damaged line is dropped (totality); a forged key is dropped
   * (the capability rule); Closed (or Close) ends the session,
   * answering the final state. A client that sends an event before
   * any hello is served as level L.
   */
  def serve[S](init: S)(view: S => Ui)(update: (S, Event) => S): Stage[String, String, S] =
    serve(init, Set.empty)(view)(update)

  /** `vocab` is the vocabulary assumed when the client sends no
   * hello; a hello replaces it (unknown names are ignored) */
  def serve[S](init: S, vocab: Set[String])(view: S => Ui)(update: (S, Event) => S): Stage[String, String, S] =
    def shownView(v: Set[String]): S => Ui = s => Ui.lower(view(s), v)

    def loop(v: Set[String], s: S, shown: Ui): Stage[String, String, S] =
      Stage.await[String, String].flatMap {
        case None => pure(s)
        case Some(line) => step(v, s, shown, line)
      }

    def step(v: Set[String], s: S, shown: Ui, line: String): Stage[String, String, S] =
      Protocol.parse(line) match
        case None => loop(v, s, shown)                                // damage is dropped
        case Some(Msg.Close) | Some(Msg.Event(Event.Closed)) => pure(s)
        case Some(Msg.Event(e)) if !permitted(shown, e) => loop(v, s, shown)   // forged is dropped
        case Some(Msg.Event(e)) =>
          val s2 = update(s, e)
          val next = shownView(v)(s2)
          val patches = Ui.diff(shown, next)
          def tell(ps: Vector[Patch]): Stage[String, String, Unit] = ps match
            case p +: more => Stage.tell[String, String](Protocol.line(Msg.Patch(p))).flatMap(_ => tell(more))
            case _ => pure(())
          tell(patches).flatMap(_ => loop(v, s2, next))
        case Some(_) => loop(v, s, shown)                             // a second hello, a stray tree: ignored

    def start(v: Set[String], pending: Option[String]): Stage[String, String, S] =
      val first = shownView(v)(init)
      Stage.tell[String, String](Protocol.line(Msg.Tree(first))).flatMap { _ =>
        pending match
          case Some(line) => step(v, init, first, line)
          case None => loop(v, init, first)
      }

    Stage.await[String, String].flatMap {
      case None => pure(init)
      case Some(line) => Protocol.parse(line) match
        case Some(Msg.Hello(claimed, _)) => start(claimed.toSet.intersect(Ui.Vocab.all), None)
        case _ => start(vocab, Some(line))
    }

  /**
   * The client: say hello (the vocabulary this host draws), keep the
   * tree, apply what arrives (a full tree or a patch), render to the
   * Host; the host's own events go back as lines. Ends when the line
   * stream does, or the user closes.
   */
  def client(host: Host, vocab: Set[String] = Set.empty)
            (lines: Source[String], send: String => Unit ! Async)
            (using Scheduler): Unit ! Async =
    var tree: Ui = Ui.Text("")

    def receive(rest: Source[String]): Unit ! Async =
      Writer.uncons[String, Unit, Async](rest).flatMap {
        case Left(_) => pure(())
        case Right((line, more)) =>
          val next = Protocol.parse(line) match
            case Some(Msg.Tree(u)) => Some(u)
            case Some(Msg.Patch(p)) => Some(Ui.patch(tree, p))
            case _ => None                          // damage, or not for us
          next match
            case None => receive(more)
            case Some(t) =>
              tree = t
              host.render(t).flatMap(_ => receive(more))
      }

    def forward(rest: Source[Event]): Unit ! Async =
      Writer.uncons[Event, Unit, Async](rest).flatMap {
        case Left(_) => pure(())
        case Right((e, more)) =>
          send(Protocol.eventLine(e)).flatMap(_ =>
            if e == Event.Closed then pure(()) else forward(more))
      }

    // hello first, then both directions at once: rendering what
    // arrives, sending what the user does — two programs, one pair
    send(Protocol.line(Protocol.hello(vocab))).flatMap(_ =>
      Async.par(receive(lines), forward(host.events)).map(_ => ()))
}
