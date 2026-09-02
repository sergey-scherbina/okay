package okay.ui

import okay.*
import okay.given

/** the seam's claim, directly: one application, two hosts */
class TestPortable extends munit.FunSuite {

  def view(n: Int): Ui = Ui.Column(Vector(
    Ui.Text(s"n=$n"), Ui.Button("+", "inc")))
  def update(n: Int, e: Event): Int = e match
    case Event.Pressed("inc") => n + 1
    case _ => n

  test("one application, the test host and the terminal's renderer agree") {
    val frames = scala.collection.mutable.Buffer[Ui]()
    val feed = Channel[Event]()
    val host = new Host:
      def render(ui: Ui): Unit ! Async = async { frames += ui; () }
      def events: Source[Event] = Writer.of(feed)
    val fiber = Async.spawn(Ui.run(0)(view)(update)(host))
    Seq(Event.Pressed("inc"), Event.Pressed("inc"), Event.Closed).foreach(feed.offer)
    assertEquals(fiber.join(), 2)

    // the terminal draws exactly these frames — same trees, same
    // lines; nothing about the app knew which host it was on
    assertEquals(frames.toList.map(Frame.render(_)), List(
      Vector("n=0", "[ + ]"),
      Vector("n=1", "[ + ]"),
      Vector("n=2", "[ + ]")))
  }
}
