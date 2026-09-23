package okay.ui

import okay.*
import okay.given
import Telegram.{Act, Key, Message, Session, Update}

/**
 * specs/ui-telegram.md — a chat as one more host. The mapping is pure
 * and asserted as a value, the way the React mapping is; the gate is the
 * seam's own claim: one application, the same final state, whether a
 * test host or a chat behind the wire draws it.
 */
class TestTelegram extends munit.FunSuite {

  import Ui.*

  def presses(m: Message): Vector[(String, String)] =
    m.keyboard.flatten.collect { case Key.Press(l, d) => l -> d }
  def dataOf(m: Message, label: String): String =
    presses(m).collectFirst { case (`label`, d) => d }.getOrElse(fail(s"no button «$label» in $m"))

  test("THE MAPPING: text above, a keyboard below — a Row shares a keyboard row, a Column gives each its own") {
    val (m, _) = Telegram.render(Column(Vector(
      Text("Title", Style(bold = true)), Text("a < b & c"),
      Row(Vector(Button("-", "dec"), Button("+", "inc"))),
      Button("reset", "reset"))), frame = 1)
    assertEquals(m.text, "<b>Title</b>\na &lt; b &amp; c")
    assertEquals(m.keyboard.map(_.map { case Key.Press(l, _) => l; case Key.Open(l, _) => l }),
      Vector(Vector("-", "+"), Vector("reset")))
  }

  test("a Check is a toggle, a Select a button per option, an Input a line and an edit button, a Link a URL") {
    val (m, meaning) = Telegram.render(Column(Vector(
      Check(true, "even", "Even"), Select(Vector("a", "b"), 1, "pick"),
      Input("", "name", "Name"), Input("s3cret", "pw", "Password", InputKind.Secret),
      Link("site", "https://example.org"))), frame = 7)
    assertEquals(m.text, "Name: —\nPassword: ••••••")
    val labels = m.keyboard.flatten.map { case Key.Press(l, _) => l; case Key.Open(l, u) => s"$l -> $u" }
    assertEquals(labels, Vector("☑ Even", "a", "● b", "✎ Name", "✎ Password", "site -> https://example.org"))
    assertEquals(meaning(dataOf(m, "☑ Even")), Telegram.Meaning.Toggle("even", on = false))
    assertEquals(meaning(dataOf(m, "a")), Telegram.Meaning.Choose("pick", 0))
    assertEquals(meaning(dataOf(m, "✎ Name")), Telegram.Meaning.Edit("name", "Name"))
  }

  test("everything outside the vocabulary is LOWERED first: a Table is rows of text, Tabs a row of buttons") {
    val (m, _) = Telegram.render(Column(Vector(
      Table(Vector("k", "v"), Vector(Vector(Text("a"), Text("1"))), "t"),
      Tabs(Vector("one", "two"), 0, Vector(Text("page one"), Text("page two")), "tabs"))), frame = 1)
    assert(m.text.contains("k") && m.text.contains("a") && m.text.contains("page one") && !m.text.contains("page two"), m.text)
    assert(presses(m).map(_._1).containsSlice(Vector("• one", "two")), m.toString)
  }

  test("CALLBACK DATA is small and bound to the frame: a press on an older frame is refused, not misread") {
    val s0 = Session()
    val (s1, a1) = Session.show(s0, Column(Vector(Text("n=0"), Button("+", "a-key-that-is-much-longer-than-sixty-four-bytes-" * 2))))
    val first = a1.collectFirst { case Act.Send(m) => m }.get
    val d = dataOf(first, "+")
    assert(d.length <= 64 && d.startsWith("f"), d)
    val s1b = Session.sent(s1, 42L)
    val (s2, a2) = Session.show(s1b, Column(Vector(Text("n=1"), Button("+", "inc"))))
    assert(a2.exists { case Act.Edit(42L, _) => true; case _ => false }, a2.toString)
    // the OLD frame's datum: answered, and no event
    val (_, evs, acts) = Session.hear(s2, Update.Pressed(d, "cb1"))
    assertEquals(evs, Vector.empty)
    assert(acts.exists { case Act.Answer("cb1", notice) => notice.nonEmpty; case _ => false }, acts.toString)
  }

  test("a press is an event and is always answered; an edit button asks, and the next message is the value") {
    val (s1, a1) = Session.show(Session(), Column(Vector(Check(false, "even", "Even"), Input("", "name", "Name"),
      Button("go", "go"))))
    val m = a1.collectFirst { case Act.Send(m) => m }.get
    val (_, e1, x1) = Session.hear(s1, Update.Pressed(dataOf(m, "☐ Even"), "c1"))
    assertEquals(e1, Vector(Event.Toggled("even", true))); assert(x1.contains(Act.Answer("c1")))
    val (s2, e2, x2) = Session.hear(s1, Update.Pressed(dataOf(m, "✎ Name"), "c2"))
    assertEquals(e2, Vector.empty)
    assert(x2.contains(Act.Answer("c2")) && x2.exists { case Act.Ask(p) => p.contains("Name"); case _ => false }, x2.toString)
    val (s3, e3, _) = Session.hear(s2, Update.Said("ada"))
    assertEquals(e3, Vector(Event.Edited("name", "ada")))
    // nothing focused: a message is not an event
    assertEquals(Session.hear(s3, Update.Said("again"))._2, Vector.empty)
  }

  test("one message, edited in place: an equal frame sends nothing; the limits are met, not crashed into") {
    val (s1, _) = Session.show(Session(), Text("same"))
    val (_, again) = Session.show(Session.sent(s1, 1L), Text("same"))
    assertEquals(again, Vector.empty)
    val (big, _) = Telegram.render(Column(Vector(Text("x" * 5000)) ++ (1 to 120).map(i => Button(s"b$i", s"k$i"))), 1)
    assert(big.text.length <= 4096 && big.text.contains("…"), big.text.length.toString)
    assertEquals(big.keyboard.flatten.length, 100)
    assertEquals(Telegram.render(Column(Vector.empty), 1)._1.text, "·")
  }

  // ---- THE GATE: one application, a test host and a chat behind the wire

  final case class S(n: Int = 0, name: String = "", even: Boolean = true, pick: Int = 0, saved: Boolean = false)

  def view(s: S): Ui = Column(Vector(
    Text(s"count: ${s.n}${if s.saved then s" saved ${s.name}/${s.even}" else ""}"),
    Row(Vector(Button("-", "dec"), Button("+", "inc"))),
    Select(Vector("red", "green", "blue"), s.pick, "pick"),
    Form(Vector(Input(s.name, "name", "Name"), Check(s.even, "even", "Even")), "Save", "f"),
    Button("quit", "quit")))

  // both paths must reach the same state: the test host sends a Form's
  // fields as they change and its button as Pressed; behind the wire the
  // client folds them and sends ONE Submitted
  def update(s: S, e: Event): S = e match
    case Event.Pressed("inc") => s.copy(n = s.n + 1)
    case Event.Pressed("dec") => s.copy(n = s.n - 1)
    case Event.Chosen("pick", i) => s.copy(pick = i)
    case Event.Edited("name", v) => s.copy(name = v)
    case Event.Toggled("even", on) => s.copy(even = on)
    case Event.Pressed("f") => s.copy(saved = true)
    case Event.Submitted("f", edits) => edits.foldLeft(s)(update).copy(saved = true)
    case _ => s

  test("THE SEAM: the same app answers the same state on the test host and in a chat behind Wire.serve") {
    // (a) the test host, fed the events a person would cause
    final class TestHost extends Host:
      val feed = Channel[Event]()
      def render(ui: Ui): Unit ! Async = async(())
      def events: Source[Event] = Writer.of(feed)
    val th = TestHost()
    val local = Async.spawn(Ui.run(S())(view)(update)(th))
    Seq(Event.Pressed("inc"), Event.Pressed("inc"), Event.Pressed("dec"), Event.Chosen("pick", 2),
      Event.Edited("name", "ada"), Event.Toggled("even", false), Event.Pressed("f"), Event.Closed)
      .foreach(th.feed.offer)
    val expected = local.join()

    // (b) the server: Wire.serveClosing over two channels, its final state kept
    val up = Channel[String](); val down = Channel[String]()
    var served: Option[S] = None
    val server = Async.spawn {
      def drain(p: S ! (Writer % String + Async)): Unit ! Async =
        Writer.uncons[String, S, Async](p).flatMap {
          case Left(s) => async { served = Some(s); down.close() }
          case Right((l, rest)) => down.send(l).map(_ => ()).flatMap(_ => drain(rest))
        }
      drain(through[String, String, Async, Unit, S](Writer.of(up))(
        !.widen[S, okay.Take % String + Writer % String, Async](
          Wire.serveClosing(S())(view)((s, e) => (update(s, e), e == Event.Pressed("quit"))))))
    }

    // …and the chat: every message the host draws, in order
    val drawn = Channel[Message]()
    var nextId = 100L
    val (host, hear) = Telegram.host {
      case Act.Send(m) => async { val _ = drawn.offer(m); nextId += 1; Some(nextId) }
      case Act.Edit(_, m) => async { val _ = drawn.offer(m); None }
      case _ => async(None)
    }
    Async.spawn(Wire.client(host)(Writer.of(down), l => up.send(l).map(_ => ()))): Unit

    // a person in the chat: tap, wait for the frame it causes, tap again
    def next: Message ! Async = drawn.receive.map(_.getOrElse(fail("the chat closed early")))
    def tap(m: Message, label: String): Unit ! Async = hear(Update.Pressed(dataOf(m, label), "cb"))
    val script: Unit ! Async =
      for
        m0 <- next
        _ <- tap(m0, "+"); m1 <- next
        _ <- tap(m1, "+"); m2 <- next
        _ <- tap(m2, "-"); m3 <- next
        _ <- tap(m3, "blue"); m4 <- next
        _ <- tap(m4, "✎ Name"); _ <- hear(Update.Said("ada")); m5 <- next     // folded by the client, redrawn
        _ <- tap(m5, "☑ Even"); m6 <- next
        _ <- tap(m6, "Save"); m7 <- next                                       // ONE Submitted crosses the wire
        _ <- tap(m7, "quit")
      yield ()
    Async.spawn(script).join()
    server.join()
    assertEquals(served, Some(expected))
    assertEquals(expected, S(n = 1, name = "ada", even = false, pick = 2, saved = true))
  }
}
