package okay.ui

import okay.*
import okay.given

/** specs/ui.md, "The effect slot" — one test per box */
class TestCmd extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(10, "s")

  final class Scripted(script: Seq[Event]) extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = okay.Source.of(script.toList)

  test("a press launches a command; the answer folds back through the same loop") {
    def view(s: String) = Ui.Column(Vector(Ui.Text(s), Ui.Button("fetch", "fetch")))
    def update(s: String, e: Event): (String, Vector[Event ! Async]) = e match
      case Event.Pressed("fetch") =>
        ("loading", Vector(async(Event.Edited("data", "42"))))
      case Event.Edited("data", v) =>
        (s"got $v", Vector(async(Event.Closed)))    // a command may end the app
      case _ => (s, Vector.empty)
    val host = Scripted(Seq(Event.Pressed("fetch")))
    val end = Async.run[String, Pure](
      Ui.runCmd("idle")(view)(update)(host)).runWith
    assertEquals(end, "got 42")
    // the interim state was SHOWN while the command ran
    assert(host.frames.exists(f => Frame.render(f).exists(_.contains("loading"))))
  }

  test("Nav.Run from a pure step: interim screen shows, the answer routes") {
    def done(msg: String): Screen = new Screen:
      def view: Ui = Ui.Text(s"done: $msg")
      def step(e: Event): Nav = Nav.Stay(this)
    lazy val loading: Screen = new Screen:
      def view: Ui = Ui.Text("loading...")
      def step(e: Event): Nav = e match
        // Run expresses "go there AND launch": the done screen shows,
        // the closer ends the app — no further event needed
        case Event.Edited("answer", v) => Nav.Run(async(Event.Closed), done(v))
        case _ => Nav.Stay(this)
    val start: Screen = new Screen:
      def view: Ui = Ui.Button("go", "go")
      def step(e: Event): Nav = e match
        case Event.Pressed("go") =>
          Nav.Run(async(Event.Edited("answer", "fetched")), loading)
        case _ => Nav.Stay(this)

    val host = Scripted(Seq(Event.Pressed("go")))
    Async.run[Unit, Pure](Nav.run(start)(host)).runWith
    val shown = host.frames.map(f => Frame.render(f).mkString("\n"))
    assert(shown.exists(_.contains("loading")), shown.mkString("|"))
    assert(shown.exists(_.contains("done: fetched")), shown.mkString("|"))
  }

  test("a throwing command forfeits its answer; the loop survives") {
    def update(s: Int, e: Event): (Int, Vector[Event ! Async]) = e match
      case Event.Pressed("boom") =>
        (s + 1, Vector(
          async[Event] { throw new RuntimeException("dies silently") },
          async(Event.Pressed("after"))))
      case Event.Pressed("after") => (s + 10, Vector(async(Event.Closed)))
      case _ => (s, Vector.empty)
    val host = Scripted(Seq(Event.Pressed("boom")))
    val end = Async.run[Int, Pure](
      Ui.runCmd(0)(_ => Ui.Text("x"))(update)(host)).runWith
    assertEquals(end, 11)   // boom counted, after counted, throw forfeited
  }
}
