package okay.ui

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import Protocol.Msg

/**
 * Stage 2 of specs/frontend.md: no round trip per keystroke. The
 * rule is data in the tree — a Form's fields fold on the client, its
 * button sends ONE Submitted — and the client is `Wire.client`, whose
 * lines are counted here.
 */
class TestHybrid extends munit.FunSuite {

  import Ui.*

  final case class Person(name: String, age: Int, ok: Boolean)
  given Schema[Person] = Schema.derived

  def form(n: Int): Ui = Column(Vector(
    Text(s"saved: $n"),
    Form(Vector(Input("", "name", "Name"), Input("", "age", "Age", InputKind.Number), Check(false, "ok", "Ok")), "Save", "f"),
    Input("", "search", "Search", live = true),
    Tabs(Vector("one", "two"), 0, Vector(Text("A"), Text("B")), "tabs"),
    Disclosure("more", false, Text("hidden"), "d")))

  /** a client over a scripted host: what it SENT, what it RENDERED */
  def client(vocab: Set[String], serverLines: Vector[String], userEvents: Vector[Event]): (Vector[String], Vector[Ui]) =
    val sent = scala.collection.mutable.Buffer[String]()
    val frames = scala.collection.mutable.Buffer[Ui]()
    val down = Channel[String]()
    val feed = Channel[Event]()
    val host = new Host:
      def render(ui: Ui): Unit ! Async = async { frames += ui; () }
      def events: Source[Event] = Writer.of(feed)
    val fiber = Async.spawn(Wire.client(host, vocab)(Writer.of(down), l => async { sent += l; () }))
    serverLines.foreach(l => { val _ = down.offer(l) })
    // let the tree land before the user acts
    Thread.sleep(50)
    userEvents.foreach(e => { val _ = feed.offer(e) })
    val _ = feed.offer(Event.Closed)
    down.close()
    fiber.join()
    (sent.toVector, frames.toVector)

  val tree = Protocol.line(Msg.Tree(form(0)))

  test("typing into a Form crosses the wire ZERO times; the button crosses ONCE as Submitted with every field") {
    val (sent, frames) = client(Set.empty, Vector(tree), Vector(
      Event.Edited("name", "a"), Event.Edited("name", "ad"), Event.Edited("name", "ada"),
      Event.Edited("age", "36"), Event.Toggled("ok", true), Event.Pressed("f")))
    val events = sent.flatMap(Protocol.eventOf)
    assertEquals(events, Vector(
      Event.Submitted("f", Vector(Event.Edited("name", "ada"), Event.Edited("age", "36"), Event.Toggled("ok", true))),
      Event.Closed))
    // and the host saw every keystroke: the tree kept the typed value
    val typed = frames.flatMap(Ui.focusable).collect { case Input(v, "name", _, _, _) => v }
    assertEquals(typed, Vector("", "a", "ad", "ada", "ada", "ada"))
  }

  test("a live input sends Edited per change; a field outside any Form too") {
    val (sent, _) = client(Set.empty, Vector(tree), Vector(Event.Edited("search", "x"), Event.Edited("search", "xy")))
    assertEquals(sent.drop(1).flatMap(Protocol.eventOf), Vector(Event.Edited("search", "x"), Event.Edited("search", "xy"), Event.Closed))
  }

  test("a claimed Tabs/Disclosure switches locally with no line; unclaimed, the press crosses") {
    val local = client(Set(Vocab.tabs, Vocab.disclosure), Vector(tree), Vector(Event.Pressed(Ui.tabKey("tabs", 1)), Event.Pressed("d")))
    assertEquals(local._1.drop(1).flatMap(Protocol.eventOf), Vector(Event.Closed))
    val last = local._2.last
    assert(Ui.keys(last)("d") && Ui.keys(last).contains(Ui.tabKey("tabs", 1)))
    val shown = Ui.focusable(last)   // the disclosure opened, the second tab is selected
    assert(last.toString.contains("Tabs(Vector(one, two),1,") && last.toString.contains("Disclosure(more,true,"), last.toString)
    assert(shown.nonEmpty)
    val remote = client(Set.empty, Vector(tree), Vector(Event.Pressed(Ui.tabKey("tabs", 1)), Event.Pressed("d")))
    assertEquals(remote._1.drop(1).flatMap(Protocol.eventOf), Vector(Event.Pressed(Ui.tabKey("tabs", 1)), Event.Pressed("d"), Event.Closed))
  }

  test("the server stays the truth: its SetValue lands on the locally edited field and wins") {
    // the name input is at path [1, 0]: Form at 1, its first field
    val override_ = Protocol.line(Msg.Patch(Patch.SetValue(List(1, 0), "server")))
    val (_, frames) = client(Set.empty, Vector(tree, override_), Vector(Event.Edited("name", "mine")))
    val values = frames.flatMap(Ui.focusable).collect { case Input(v, "name", _, _, _) => v }
    assertEquals(values, Vector("", "server", "mine"))
    // ...and then a later SetValue would win again — the tree is one, the server's line is the newer
    val (_, again) = client(Set.empty, Vector(tree), Vector(Event.Edited("name", "mine")))
    assertEquals(again.flatMap(Ui.focusable).collect { case Input(v, "name", _, _, _) => v }, Vector("", "mine"))
  }

  test("the server: a Submitted names a shown Form and only its fields, else it never reaches update") {
    var seen = Vector.empty[Event]
    def update(n: Int, e: Event): Int = { seen :+= e; n + 1 }
    def talk(lines: String*): Int =
      !.run(Writer.run(through(Writer.of(lines.toList))(Wire.serve(0)(form)(update))))._2
    def sub(k: String, edits: Event*) = Protocol.eventLine(Event.Submitted(k, edits.toVector))
    val n = talk(
      sub("f", Event.Edited("name", "ada"), Event.Toggled("ok", true)),           // honest
      sub("ghost", Event.Edited("name", "x")),                                    // no such form
      sub("f", Event.Edited("search", "x")),                                      // not this form's field
      sub("f", Event.Pressed("inc")),                                             // not an edit
      sub("f", Event.Submitted("f", Vector.empty)))                               // not an edit either
    assertEquals(n, 1)
    assertEquals(seen, Vector(Event.Submitted("f", Vector(Event.Edited("name", "ada"), Event.Toggled("ok", true)))))
  }

  test("Form.submitted folds the edits through the same edit a live form takes, and decodes typed") {
    val empty = Json.JObj(Vector.empty)
    val edits = Vector(Event.Edited("name", "ada"), Event.Edited("age", "36"), Event.Toggled("ok", true))
    val live = edits.foldLeft(empty)(okay.ui.Form.edit[Person])
    val once = okay.ui.Form.submitted[Person](empty, Event.Submitted("f", edits))
    assertEquals(once, live)
    assertEquals(okay.ui.Form.decode[Person](once), Right(Person("ada", 36, true)))
  }

  test("Disclosure: keys and diff laws, and its lowering is a toggle button plus the body when open") {
    val closed = Disclosure("more", false, Button("x", "inner"), "d")
    val open = Disclosure("more", true, Button("x", "inner"), "d")
    assertEquals(Ui.keys(closed), Set("d"))
    assertEquals(Ui.keys(open), Set("d", "inner"))
    for d <- Vector(closed, open) do assertEquals(Ui.keys(d), Ui.keys(Ui.lower(d, Set.empty)))
    assertEquals(Ui.patch(closed, Ui.diff(closed, open).head), open)
    assertEquals(Ui.lower(open, Set.empty), Box(Vector(Button("more", "d", Role.Active), Button("x", "inner")), Dir.Vertical, key = "d"))
    assertEquals(Ui.lower(closed, Set.empty), Box(Vector(Button("more", "d", Role.Plain)), Dir.Vertical, key = "d"))
    assertEquals(Ui.diff(open, Disclosure("more", true, Button("y", "inner"), "d")), Vector(Patch.Replace(List(1), Button("y", "inner"))))
  }
}
