package okay.ui

import okay.*
import okay.given

/**
 * The whole loop with no screen: a test HOST keeps frames as values
 * and feeds scripted events — which is the seam's point, and the
 * first application of it.
 */
class TestUi extends munit.FunSuite {

  import Event.*
  import Ui.*

  /** frames as a value, events from a channel */
  final class TestHost extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    val feed = Channel[Event]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = Writer.of(feed)

  // a counter: two buttons, a label
  def view(n: Int): Ui = Column(Vector(
    Text(s"count: $n"),
    Row(Vector(Button("-", "dec"), Button("+", "inc")))))

  def update(n: Int, e: Event): Int = e match
    case Pressed("inc") => n + 1
    case Pressed("dec") => n - 1
    case _ => n

  test("the loop: scripted events, frames in order, Closed answers the state") {
    val host = TestHost()
    val fiber = Async.spawn(Ui.run(0)(view)(update)(host))
    for e <- Seq(Pressed("inc"), Pressed("inc"), Pressed("dec"), Closed) do host.feed.offer(e): Unit
    assertEquals(fiber.join(), 1)
    assertEquals(host.frames.toList, List(view(0), view(1), view(2), view(1)))
  }

  test("an unchanged view is not re-rendered") {
    val host = TestHost()
    val fiber = Async.spawn(Ui.run(0)(view)(update)(host))
    for e <- Seq(Pressed("nope"), Pressed("inc"), Closed) do host.feed.offer(e): Unit
    assertEquals(fiber.join(), 1)
    assertEquals(host.frames.toList, List(view(0), view(1)))   // the no-op pressed nothing
  }

  test("external sources merge in: a timer and the user share one fold") {
    val host = TestHost()
    val ticks = Channel[Event]()
    val fiber = Async.spawn(
      Ui.run(0)(view)(update)(host, external = Writer.of(ticks)))
    ticks.offer(Pressed("inc")): Unit
    host.feed.offer(Pressed("inc")): Unit
    ticks.offer(Pressed("inc")): Unit
    // no Closed race: end both sources; the merged stream ends when
    // both do, and the loop answers the state it reached
    ticks.close(); host.feed.close()
    assertEquals(fiber.join(), 3)
  }

  test("diff: a changed leaf patches narrowly, equal trees not at all") {
    assertEquals(Ui.diff(view(1), view(1)), Vector.empty)
    assertEquals(Ui.diff(view(1), view(2)),
      Vector(Patch.SetText(List(0), "count: 2")))
    // a changed shape replaces at the highest differing node
    val other = Column(Vector(Text("count: 1"), Text("done")))
    assertEquals(Ui.diff(view(1), other),
      Vector(Patch.Replace(List(1), Text("done"))))
  }

  test("diff then patch equals the next tree — on shapes that differ everywhere") {
    val trees = Seq(
      view(0), view(7), Column(Vector(Text("a"))),
      Row(Vector(Input("x", "i"), Check(true, "c"), Select(Vector("a", "b"), 1, "s"))),
      Row(Vector(Input("y", "i"), Check(false, "c"), Select(Vector("a", "b"), 0, "s"))),
      Text("alone"))
    for a <- trees; b <- trees do
      val patched = Ui.diff(a, b).foldLeft(a)(Ui.patch)
      assertEquals(patched, b, s"diff/patch disagreed from $a to $b")
  }

  test("the patch path and the repaint path agree (Host.diffing)") {
    // a backend that applies patches to a VALUE
    val applied = scala.collection.mutable.Buffer[Ui]()
    val backend = new Backend:
      private var tree: Ui = Text("")
      def apply(p: Patch): Unit ! Async = async {
        tree = p match
          case Patch.Replace(Nil, ui) => ui
          case p => Ui.patch(tree, p)
        applied += tree
        ()
      }
      def events: Source[Event] = pure(())

    val host = Ui.diffing(backend)
    val frames = Seq(view(0), view(1), view(1), view(5))
    def send(rest: Seq[Ui]): Unit ! Async = rest match
      case f +: more => host.render(f).flatMap(_ => send(more))
      case _ => pure(())
    send(frames).runWith
    // after every render, the backend's tree IS the frame
    assertEquals(applied.lastOption, Some(view(5)))
    assert(applied.contains(view(1)))
  }

  test("focusable walks the tree in tab order") {
    assertEquals(Ui.focusable(view(3)).collect { case Button(_, k) => k },
      Vector("dec", "inc"))
  }
}
