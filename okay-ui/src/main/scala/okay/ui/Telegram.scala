package okay.ui

import okay.*
import okay.given

/**
 * A CHAT AS ONE MORE HOST (specs/ui-telegram.md).
 *
 * A Telegram chat draws one message — text above, a keyboard of buttons
 * below — and the bot may EDIT that message in place; the person answers
 * by pressing a button or by writing the next message. That is the
 * narrowest screen okay-ui draws, and it is drawn the way every other
 * is: a `Host`, so an application runs here with `Ui.run`, or behind
 * `Wire.serve` with `Wire.client(Telegram.host(…))` — the program a
 * browser is served, unchanged.
 *
 * No HTTP: what the chat is asked to do leaves as `Act`s, performed by
 * the consumer's Bot API client, and what the chat said comes in as
 * `Update`s. The mapping and the session are pure values.
 */
object Telegram {

  /** one message: its text (Telegram's HTML subset) and the inline
   * keyboard under it, row by row */
  final case class Message(text: String, keyboard: Vector[Vector[Key]])

  enum Key:
    case Press(label: String, data: String)
    case Open(label: String, url: String)

  /** what the chat said */
  enum Update:
    case Pressed(data: String, callbackId: String)
    case Said(text: String)

  /** what the host asks the chat to do — performed by the consumer */
  enum Act:
    case Send(m: Message)
    case Edit(messageId: Long, m: Message)
    case Answer(callbackId: String, notice: String = "")
    /** a message asking for a reply (the consumer sends it ForceReply) */
    case Ask(prompt: String)

  /** what one button of the frame means */
  enum Meaning:
    case Press(key: String)
    case Toggle(key: String, on: Boolean)
    case Choose(key: String, index: Int)
    case Edit(key: String, label: String)

  /** a chat draws a link natively (a URL button); everything else is
   * lowered to what it means (`Ui.lower`) */
  val vocab: Set[String] = Set(Ui.Vocab.link)

  /** Telegram's own limits: a message's text, buttons in all, a row */
  val maxText = 4096
  val maxButtons = 100
  val perRow = 8

  /** what a press on a frame that is no longer shown answers */
  val outdated = "outdated — the screen has changed"

  // ---- the pure mapping ---------------------------------------------------

  private enum Pending:
    case Callback(label: String, meaning: Meaning)
    case Url(label: String, url: String)

  private final case class Frag(lines: Vector[String], rows: Vector[Vector[Pending]])
  private val nothing = Frag(Vector.empty, Vector.empty)

  private def esc(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  private def styled(s: String, st: Style): String =
    val e = esc(s)
    if st.bold || st.tone == Tone.Emphasis then s"<b>$e</b>"
    else if st.dim || st.tone == Tone.Muted then s"<i>$e</i>"
    else e

  private def labelled(label: String, role: Role): String = role match
    case Role.Active => s"• $label"
    case _ => label

  private def button(p: Pending): Frag = Frag(Vector.empty, Vector(Vector(p)))

  /** children side by side: their lines as ONE line where each has at
   * most one, their buttons as ONE keyboard row */
  private def across(cs: Vector[Ui]): Frag =
    val fs = cs.map(go)
    val lines = if fs.forall(_.lines.length <= 1) then
      Vector(fs.flatMap(_.lines).filter(_.nonEmpty).mkString("  ")).filter(_.nonEmpty)
    else fs.flatMap(_.lines)
    Frag(lines, Vector(fs.flatMap(_.rows.flatten)).filter(_.nonEmpty))

  /** children one under another: lines and rows in order */
  private def down(cs: Vector[Ui]): Frag =
    val fs = cs.map(go)
    Frag(fs.flatMap(_.lines), fs.flatMap(_.rows))

  private def go(u: Ui): Frag = u match
    case Ui.Text(s, st) => if s.isEmpty then nothing else Frag(Vector(styled(s, st)), Vector.empty)
    case Ui.Image(_, alt) => Frag(Vector(s"🖼 ${esc(alt)}"), Vector.empty)
    case Ui.Button(label, key, role) => button(Pending.Callback(labelled(label, role), Meaning.Press(key)))
    case Ui.Check(on, key, label) =>
      button(Pending.Callback(s"${if on then "☑" else "☐"} $label", Meaning.Toggle(key, !on)))
    case Ui.Select(options, selected, key) =>
      Frag(Vector.empty, Vector(options.zipWithIndex.map((o, i) =>
        Pending.Callback(if i == selected then s"● $o" else o, Meaning.Choose(key, i)))).filter(_.nonEmpty))
    case Ui.Input(value, key, label, kind, _) =>
      val name = if label.nonEmpty then label else key
      val shown =
        if value.isEmpty then "—"
        else if kind == InputKind.Secret then "•" * value.length
        else esc(value)
      Frag(Vector(s"${esc(name)}: $shown"), Vector(Vector(Pending.Callback(s"✎ $name", Meaning.Edit(key, name)))))
    case Ui.Form(fields, submit, key) =>
      val f = down(fields)
      f.copy(rows = f.rows :+ Vector(Pending.Callback(submit, Meaning.Press(key))))
    case Ui.Row(cs, _) => across(cs)
    case Ui.Column(cs, _) => down(cs)
    case b: Ui.Box => if b.dir == Dir.Horizontal then across(b.children) else down(b.children)
    case Ui.Scroll(c, _) => go(c)
    case Ui.Link(label, href) => button(Pending.Url(label, href))
    // lowered away before this is reached (`render` lowers first)
    case _ => nothing

  /**
   * A tree as one message, and what each of its buttons means. The data
   * a button carries is `f<frame>.<n>` — small, whatever the key, and
   * meaningless on any other frame.
   */
  def render(ui: Ui, frame: Int): (Message, Map[String, Meaning]) =
    val f = go(Ui.lower(ui, vocab))
    val all = f.rows.flatMap(_.grouped(perRow).toVector)
    val total = all.map(_.length).sum
    // keep whole rows up to the limit, then cut the last one kept
    val kept = all.foldLeft(Vector.empty[Vector[Pending]]) { (acc, row) =>
      val room = maxButtons - acc.map(_.length).sum
      if room <= 0 then acc else acc :+ row.take(room)
    }
    var n = 0
    var meaning = Map.empty[String, Meaning]
    val keyboard = kept.map(_.map {
      case Pending.Callback(label, m) =>
        val data = s"f$frame.$n"; n += 1
        meaning += data -> m
        Key.Press(label, data)
      case Pending.Url(label, url) => Key.Open(label, url)
    })
    val note = if total > maxButtons then Vector(s"(+${total - maxButtons} not shown)") else Vector.empty
    val raw = (f.lines ++ note).mkString("\n")
    val text =
      if raw.isEmpty then "·"
      else if raw.length > maxText then raw.take(maxText - 1) + "…"
      else raw
    (Message(text, keyboard), meaning)

  // ---- the session: what the host is, as a value --------------------------

  final case class Session(frame: Int = 0, shown: Option[Message] = None,
                           meaning: Map[String, Meaning] = Map.empty,
                           messageId: Option[Long] = None,
                           /** the Input whose value the next message is */
                           focus: Option[String] = None)

  object Session {

    /** a tree to show: nothing when it draws the message already shown;
     * otherwise the next frame, sent once and edited ever after */
    def show(s: Session, ui: Ui): (Session, Vector[Act]) =
      if s.shown.contains(render(ui, s.frame)._1) then (s, Vector.empty)
      else
        val frame = s.frame + 1
        val (m, meaning) = render(ui, frame)
        val act = s.messageId.fold(Act.Send(m))(id => Act.Edit(id, m))
        (s.copy(frame = frame, shown = Some(m), meaning = meaning), Vector(act))

    /** what the chat said, read against the frame SHOWN — the capability
     * rule `Wire.permitted` states for the server, on this side too */
    def hear(s: Session, u: Update): (Session, Vector[Event], Vector[Act]) = u match
      case Update.Pressed(data, cb) => s.meaning.get(data) match
        case None => (s, Vector.empty, Vector(Act.Answer(cb, outdated)))
        case Some(Meaning.Press(k)) => (s, Vector(Event.Pressed(k)), Vector(Act.Answer(cb)))
        case Some(Meaning.Toggle(k, on)) => (s, Vector(Event.Toggled(k, on)), Vector(Act.Answer(cb)))
        case Some(Meaning.Choose(k, i)) => (s, Vector(Event.Chosen(k, i)), Vector(Act.Answer(cb)))
        case Some(Meaning.Edit(k, label)) =>
          (s.copy(focus = Some(k)), Vector.empty, Vector(Act.Answer(cb), Act.Ask(s"$label?")))
      case Update.Said(text) => s.focus match
        case Some(k) => (s.copy(focus = None), Vector(Event.Edited(k, text)), Vector.empty)
        case None => (s, Vector.empty, Vector.empty)

    /** the id the chat gave the message a `Send` created */
    def sent(s: Session, messageId: Long): Session = s.copy(messageId = Some(messageId))
  }

  // ---- the host: the session, and a consumer who performs the acts --------

  /**
   * The Host, and the door the consumer feeds the chat's updates through.
   * `perform` is the consumer's Bot API call; for a `Send` it answers the
   * new message's id, so every later frame edits that message.
   */
  def host(perform: Act => Option[Long] ! Async): (Host, Update => Unit ! Async) =
    val feed = Channel[Event]()
    var session = Session()
    val lock = new Object

    def run(acts: Vector[Act]): Unit ! Async =
      acts.foldLeft(pure(()): Unit ! Async) { (p, a) =>
        p.flatMap(_ => perform(a).map { id =>
          a match
            case Act.Send(_) => id.foreach(i => lock.synchronized { session = Session.sent(session, i) })
            case _ => ()
        })
      }

    val h = new Host:
      def render(ui: Ui): Unit ! Async =
        async { lock.synchronized { val (s, acts) = Session.show(session, ui); session = s; acts } }
          .flatMap(run)
      def events: Source[Event] = Writer.of(feed)

    val hear: Update => Unit ! Async = u =>
      async { lock.synchronized { val (s, evs, acts) = Session.hear(session, u); session = s; (evs, acts) } }
        .flatMap { (evs, acts) =>
          evs.foldLeft(pure(()): Unit ! Async)((p, e) => p.flatMap(_ => feed.send(e).map(_ => ())))
            .flatMap(_ => run(acts))
        }
    (h, hear)
}
