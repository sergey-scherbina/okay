package okay.ui

import okay.*
import okay.given
import okay.codec.Json

/**
 * Server-driven UI over no transport at all — the stage tested pure —
 * and then over a pair of channels, end to end, with the security
 * rule asserted from the hostile side.
 */
class TestWire extends munit.FunSuite {

  def view(n: Int): Ui = Ui.Column(Vector(
    Ui.Text(s"count: $n"),
    Ui.Row(Vector(Ui.Button("-", "dec"), Ui.Button("+", "inc")))))

  def update(n: Int, e: Event): Int = e match
    case Event.Pressed("inc") => n + 1
    case Event.Pressed("dec") => n - 1
    case Event.Pressed("boom") => sys.error("a key that is not on screen was pressed")
    case _ => n

  val shapes: Seq[Ui] = Seq(
    Ui.Text("plain"), Ui.Text("bold", Style(bold = true)),
    Ui.Row(Vector(Ui.Text("a"), Ui.Button("b", "k1")), "r"),
    Ui.Column(Vector(Ui.Input("v", "k2", "l"), Ui.Check(true, "k3", "c")), "col"),
    Ui.Select(Vector("x", "y"), 1, "k4"))

  test("every Ui, Event and Patch shape round-trips through Json") {
    for u <- shapes do
      assertEquals(WireJson.uiOf(Json.parse(Json.print(WireJson.uiJson(u)))), Some(u))
    val events = Seq(Event.Pressed("a"), Event.Edited("b", "v"), Event.Toggled("c", true),
      Event.Chosen("d", 2), Event.Key('x'), Event.Resized(80, 24), Event.Closed)
    for e <- events do
      assertEquals(WireJson.eventOf(Json.parse(Json.print(WireJson.eventJson(e)))), Some(e))
    val patches = Seq(Patch.Replace(List(1, 0), shapes(2)), Patch.SetText(List(0), "s"),
      Patch.SetValue(Nil, "v"), Patch.SetChecked(List(2), false), Patch.SetSelected(List(1), 1))
    for p <- patches do
      assertEquals(WireJson.patchOf(Json.parse(Json.print(WireJson.patchJson(p)))), Some(p))
  }

  /** drive the pure serve stage with lines, collect its lines */
  def talk(lines: String*): (Seq[String], Int) =
    val (out, s) = !.run(Writer.run(through(Writer.of(lines.toList))(
      Wire.serve(0)(view)(update))))
    (out, s)

  def press(k: String): String = Json.print(WireJson.eventJson(Event.Pressed(k)))

  test("the first line is the full tree; later lines are narrow patches") {
    val (out, _) = talk(press("inc"), press("inc"))
    assertEquals(WireJson.uiOf(Json.parse(out.head)), Some(view(0)))
    // each press changed ONE text: one narrow patch each, no repaints
    assertEquals(out.tail.map(l => WireJson.patchOf(Json.parse(l))), Seq(
      Some(Patch.SetText(List(0), "count: 1")),
      Some(Patch.SetText(List(0), "count: 2"))))
  }

  test("a forged key is dropped before update sees it; damage is dropped too") {
    // update would THROW on "boom" — the wire never lets it through
    val (out, s) = talk(press("boom"), "{ not json", press("inc"))
    assertEquals(s, 1)
    assertEquals(out.length, 2)   // the tree, and one honest patch
  }

  test("Closed ends the session with the final state") {
    val (_, s) = talk(press("inc"),
      Json.print(WireJson.eventJson(Event.Closed)), press("inc"))
    assertEquals(s, 1)   // nothing after Closed
  }

  test("a reconnecting client: a fresh serve of the SAME state leads with the full tree") {
    val (out, _) = !.run(Writer.run(through(Writer.of(List.empty[String]))(
      Wire.serve(41)(view)(update)))): @unchecked
    assertEquals(WireJson.uiOf(Json.parse(out.head)), Some(view(41)))
  }

  test("end to end over channels: the client's frames are the server's views") {
    val up = Channel[String]()      // events, client -> server
    val down = Channel[String]()    // tree/patches, server -> client

    // the server: the pure stage over the channels
    Async.spawn {
      val src: Source[String] = Writer.of(up)
      def drain(p: Int ! (Writer % String + Async)): Unit ! Async =
        Writer.uncons[String, Int, Async](p).flatMap {
          case Left(_) => async(down.close())
          case Right((l, rest)) => async(down.send(l)).flatMap(_ => drain(rest))
        }
      drain(through[String, String, Async, Unit, Int](src)(
        !.widen[Int, okay.Take % String + Writer % String, Async](
          Wire.serve(0)(view)(update))))
    }: Unit

    // the client: a scripted user on a value host
    val frames = scala.collection.mutable.Buffer[Ui]()
    val feed = Channel[Event]()
    val host = new Host:
      def render(ui: Ui): Unit ! Async = async { frames += ui; () }
      def events: Source[Event] = Writer.of(feed)

    val fiber = Async.spawn(Wire.client(host)(Writer.of(down), l => async(up.send(l))))
    Seq(Event.Pressed("inc"), Event.Pressed("inc"), Event.Pressed("dec"), Event.Closed)
      .foreach(feed.send)
    fiber.join()

    assertEquals(frames.toList, List(view(0), view(1), view(2), view(1)))
  }
}
