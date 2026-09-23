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
        // a Submitted names a shown Form and only its own fields
        case Event.Submitted(k, edits) => Ui.forms(tree).get(k).exists { fields =>
          edits.forall {
            case Event.Edited(fk, _) => fields(fk)
            case Event.Toggled(fk, _) => fields(fk)
            case Event.Chosen(fk, _) => fields(fk)
            case _ => false
          } }
        case _ => false

  /**
   * The server. The client's first line is its `Hello`, naming the
   * semantic nodes it draws; everything else is LOWERED before it is
   * sent. Then the FULL TREE, then the narrow patches the diff makes.
   * A damaged line is dropped (totality); a forged key is dropped
   * (the capability rule); Closed (or Close), from EITHER side, ends
   * the session, answering the final state. A client that sends an
   * event before any hello is served as level L.
   *
   * `serve` is `serveClosing` whose second half is never true — one
   * loop, not two (wire-server-close).
   */
  def serve[S](init: S)(view: S => Ui)(update: (S, Event) => S): Stage[String, String, S] =
    serveClosing(init)(view)((s, e) => (update(s, e), false))

  /** `vocab` is the vocabulary assumed when the client sends no
   * hello; a hello replaces it (unknown names are ignored) */
  def serve[S](init: S, vocab: Set[String])(view: S => Ui)(update: (S, Event) => S): Stage[String, String, S] =
    serveClosing(init, vocab)(view)((s, e) => (update(s, e), false))

  /**
   * `serve`, but `update` may also decide THIS event is the session's
   * last one (wire-server-close): the server side of what only a
   * client's own `Closed`/`Close` could do before. `true` still sends
   * the patches this event made — the last thing the client sees
   * before the door shuts is what actually happened — then ONE
   * `Msg.Close` line, then the loop ends the same way a client-sent
   * Close already did.
   */
  def serveClosing[S](init: S)(view: S => Ui)
                      (update: (S, Event) => (S, Boolean)): Stage[String, String, S] =
    serveClosing(init, Set.empty)(view)(update)

  def serveClosing[S](init: S, vocab: Set[String])(view: S => Ui)
                      (update: (S, Event) => (S, Boolean)): Stage[String, String, S] =
    def shownView(v: Set[String]): S => Ui = s => Ui.lower(view(s), v)

    // The session is ONE `Stage.transduceUntil` (specs/fold-until.md
    // stage 3; wire-serve-transduce-until): the state is what the
    // server remembers between lines — its own state and the tree it
    // showed — a `Left` carries on with the next, a `Right` is the
    // session's answer, and the input ending is `end`, the state's own
    // half. It was a hand-written `loop`/`step` pair before, found by
    // loop-audit counting the doors.
    def step(v: Set[String])(state: (S, Ui), line: String): Stage[String, String, Either[(S, Ui), S]] =
      val (s, shown) = state
      Protocol.parse(line) match
        case None => pure(Left(state))                                       // damage is dropped
        case Some(Msg.Close) | Some(Msg.Event(Event.Closed)) => pure(Right(s))
        case Some(Msg.Event(e)) if !permitted(shown, e) => pure(Left(state)) // forged is dropped
        case Some(Msg.Event(e)) =>
          val (s2, done) = update(s, e)
          val next = shownView(v)(s2)
          val patches = Ui.diff(shown, next)
          def tell(ps: Vector[Patch]): Stage[String, String, Unit] = ps match
            case p +: more => Stage.tell[String, String](Protocol.line(Msg.Patch(p))).flatMap(_ => tell(more))
            case _ => pure(())
          if done then
            tell(patches).flatMap(_ => Stage.tell[String, String](Protocol.line(Msg.Close)))
              .map(_ => Right(s2))
          else tell(patches).map(_ => Left((s2, next)))
        case Some(_) => pure(Left(state))                                    // a second hello, a stray tree: ignored

    def session(v: Set[String], state: (S, Ui)): Stage[String, String, S] =
      Stage.transduceUntil[String, String, (S, Ui), S](state)(step(v), _._1)

    def start(v: Set[String], pending: Option[String]): Stage[String, String, S] =
      val first = shownView(v)(init)
      Stage.tell[String, String](Protocol.line(Msg.Tree(first))).flatMap { _ =>
        pending match
          // the line that arrived before any hello is the first step
          case Some(line) => step(v)((init, first), line).flatMap {
            case Left(state) => session(v, state)
            case Right(s) => pure(s)
          }
          case None => session(v, (init, first))
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

    // the hybrid rule (specs/frontend.md stage 2): a field edit inside
    // a Form stays here — the tree keeps it and the host re-renders —
    // and the Form's button sends the fields ONCE as Submitted; a
    // claimed Tabs/Disclosure switches here too. Everything else
    // crosses the wire as it did.
    def forward(rest: Source[Event]): Unit ! Async =
      Writer.uncons[Event, Unit, Async](rest).flatMap {
        case Left(_) => pure(())
        case Right((e, more)) =>
          Ui.foldLocal(tree, e, vocab) match
            case Some(t) =>
              tree = t
              host.render(t).flatMap(_ => forward(more))
            case None =>
              val out = e match
                case Event.Pressed(k) if Ui.forms(tree).contains(k) => Ui.submit(tree, k).getOrElse(e)
                case other => other
              send(Protocol.eventLine(out)).flatMap(_ =>
                if e == Event.Closed then pure(()) else forward(more))
      }

    // hello first, then both directions at once: rendering what
    // arrives, sending what the user does — two programs, one pair
    send(Protocol.line(Protocol.hello(vocab))).flatMap(_ =>
      Async.par(receive(lines), forward(host.events)).map(_ => ()))
}
