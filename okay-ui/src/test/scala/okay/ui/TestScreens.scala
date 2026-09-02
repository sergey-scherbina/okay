package okay.ui

import okay.*
import okay.given
import okay.codec.Schema

/**
 * Screens are codata, navigation is a stack, and a Dialog scenario is
 * a pushable screen — the two halves of phase 2 meeting.
 */
class TestScreens extends munit.FunSuite {

  import Event.*

  /** the parent: a counter, with a settings child and a wizard child */
  def counter(n: Int, label: String): Screen =
    Nav.screen((n, label))((s) => Ui.Column(Vector(
      Ui.Text(s"${s._2}: ${s._1}"),
      Ui.Row(Vector(Ui.Button("+", "inc"), Ui.Button("settings", "open"),
        Ui.Button("rename", "wiz"), Ui.Button("quit", "quit")))))) { (s, e) =>
      e match
        case Pressed("inc") => (s._1 + 1, s._2)
        case Pressed("open") => Nav.Push(settings(s._1, s._2))
        case Pressed("wiz") => Nav.Push(Nav.scenario(rename)(
          name => Nav.To(counter(s._1, name.getOrElse(s._2)))))
        case Pressed("quit") => Nav.Pop
        case _ => s
    }

  def settings(n: Int, label: String): Screen =
    Nav.screen(())(_ => Ui.Column(Vector(
      Ui.Text(s"settings of $label"),
      Ui.Button("back", "back")))) { (_, e) =>
      e match
        case Pressed("back") => Nav.Pop
        case _ => Nav.Stay(settings(n, label))
    }

  final case class NewName(name: String)
  given Schema[NewName] = Schema.derived

  def rename: Option[String] ! Dialog =
    Form.ask[NewName]("rename to?").map(_.map(_.name))

  def drive(events: Event*): Vector[Ui] =
    var stack = Nav.state(counter(0, "count"))
    val frames = scala.collection.mutable.Buffer[Ui](Nav.view(stack))
    for e <- events do
      stack = Nav.update(stack, e)
      frames += Nav.view(stack)
    frames.toVector

  test("events route to the top; Push covers, Pop reveals the STEPPED parent") {
    val frames = drive(Pressed("inc"), Pressed("open"), Pressed("back"))
    assertEquals(Frame.render(frames(0))(0), "count: 0")
    assertEquals(Frame.render(frames(1))(0), "count: 1")
    assertEquals(Frame.render(frames(2))(0), "settings of count")
    // the parent under the child kept its stepped state
    assertEquals(Frame.render(frames(3))(0), "count: 1")
  }

  test("a popped stack is the end: the empty stack shows nothing and stays empty") {
    var stack = Nav.state(counter(0, "count"))
    stack = Nav.update(stack, Pressed("quit"))
    assertEquals(stack, Nil)
    assertEquals(Nav.update(stack, Pressed("inc")), Nil)
    assertEquals(Nav.view(stack), Ui.Text(""))
  }

  test("a Dialog scenario is a pushable screen; its answer lands via the continuation") {
    val frames = drive(
      Pressed("inc"),
      Pressed("wiz"),                      // push the wizard
      Edited("name", "total"),
      Pressed("$ok"))                      // the scenario answers -> To(renamed counter)
    assertEquals(Frame.render(frames.last)(0), "total: 1")
    // and the wizard actually SHOWED while it ran
    assert(frames.exists(f => Frame.render(f).exists(_.contains("rename to?"))))
  }

  test("cancelling the wizard keeps the parent as it was") {
    val frames = drive(Pressed("wiz"), Pressed("$cancel"))
    assertEquals(Frame.render(frames.last)(0), "count: 0")
  }

  test("the stack runs in the LOOP: Ui.run over Nav") {
    val feed = Channel[Event]()
    val frames = scala.collection.mutable.Buffer[Ui]()
    val host = new Host:
      def render(ui: Ui): Unit ! Async = async { frames += ui; () }
      def events: Source[Event] = Writer.of(feed)
    val fiber = Async.spawn(Nav.run(counter(0, "count"))(host))
    Seq(Pressed("inc"), Pressed("open"), Pressed("back"), Closed).foreach(feed.offer)
    fiber.join()
    assertEquals(Frame.render(frames.last)(0), "count: 1")
  }
}
