package okay.ui

import okay.*
import okay.given
import java.awt.{Component, Container}
import javax.swing.*

/**
 * The Swing host, headless (ui-native-toolkits): the DOM backend's
 * law battery, verbatim, against a component tree — patching frame by
 * frame equals building the last frame — and the delegated events
 * come back as ours by key, interpreted against the mirror.
 */
class TestSwing extends munit.FunSuite {

  System.setProperty("java.awt.headless", "true")

  import Ui.*

  def now[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** a component tree as a string: what a frame LOOKS like, structurally */
  def show(c: Component): String = c match
    case sp: JScrollPane => s"Scroll(${show(sp.getViewport.getView)})"
    case l: JLabel => s"Label(${l.getText}${if l.getFont.isBold then ",b" else ""})"
    case b: JButton => s"Button(${b.getText})"
    case t: JPasswordField => s"Secret(${String(t.getPassword)})"
    case t: JTextArea => s"Area(${t.getText})"
    case t: JTextField => s"Field(${t.getText})"
    case cb: JCheckBox => s"Check(${cb.getText},${cb.isSelected})"
    case combo: JComboBox[?] => s"Select(${combo.getSelectedIndex})"
    case p: JPanel => p.getComponents.toVector.map(c => show(c)).mkString("Panel[", ",", "]")
    case other => other.getClass.getSimpleName

  def mount(): (JPanel, Backend) =
    val root = new JPanel()
    (root, Swing.backend(root))

  def fresh(ui: Ui): String =
    val (root, b) = mount()
    now(b.apply(Patch.Replace(Nil, ui)))
    show(root.getComponent(0))

  /** wait for a count rather than a duration; bounded, and loud when
   * the bound is hit, because a test that says what it waited for is
   * debuggable and one that fails an unrelated assertion is not */
  private def awaitAtLeast(counter: java.util.concurrent.atomic.AtomicInteger,
                           n: Int, what: String): Unit =
    val deadline = System.currentTimeMillis() + 10_000
    while counter.get < n && System.currentTimeMillis() < deadline do Thread.`yield`()
    assert(counter.get >= n, s"timed out after 10 s waiting for $what; saw ${counter.get} of $n")

  test("the law at Swing: patching frame by frame equals building the last frame") {
    val frames = Vector(
      Column(Vector(Text("hello"), Button("go", "go"), Input("", "name", "Name")), "app"),
      Column(Vector(Text("hello!"), Button("go", "go"), Input("Ada", "name", "Name")), "app"),
      Column(Vector(Input("Ada", "name", "Name"), Button("go", "go"), Check(true, "ok", "Ok")), "app"),
      Column(Vector(Row(Vector(Text("a"), Text("b")), "r")), "app"),
      Column(Vector(Row(Vector(Text("a"), Text("c")), "r")), "app"),
      Box(Vector(Image("/a.png", "a"), Scroll(Text("s"), "sc"), Input("", "pw", "Pw", InputKind.Secret),
        Button("go", "go", Role.Primary)), Dir.Horizontal, weights = Vector(1, 2, 1, 1), gap = 1, key = "app"),
      Box(Vector(Image("/b.png", "b"), Scroll(Text("t"), "sc"), Input("x", "pw", "Pw", InputKind.Secret),
        Button("go", "go", Role.Primary)), Dir.Horizontal, weights = Vector(1, 2, 1, 1), gap = 1, key = "app"),
      Form(Vector(Input("", "n", "N"), Input("m", "note", "Note", InputKind.Multiline)), "Save", "f"),
      Form(Vector(Input("v", "n", "N"), Input("mm", "note", "Note", InputKind.Multiline)), "Save", "f"),
      // semantic nodes arrive lowered through Ui.diffing
      Items(Vector(Row(Vector(Text("one"), Button("x", "d0")), key = "i0"), Row(Vector(Text("two"), Button("x", "d1")), key = "i1")), "list"),
      Items(Vector(Row(Vector(Text("two"), Button("x", "d1")), key = "i1"), Row(Vector(Text("one!"), Button("x", "d0")), key = "i0"),
        Row(Vector(Text("three"), Button("x", "d2")), key = "i2")), "list"),
      Tabs(Vector("a", "b"), 1, Vector(Text("A"), Select(Vector("x", "y"), 1, "sel")), "t"))
    val (root, b) = mount()
    val host = Ui.diffing(b)
    frames.indices.foreach { i =>
      now(host.render(frames(i)))
      assertEquals(show(root.getComponent(0)), fresh(Ui.lower(frames(i), Set.empty)), s"after frame $i")
    }
  }

  test("a keyed shuffle MOVES components: the same instances, reordered") {
    val (root, b) = mount()
    val host = Ui.diffing(b)
    def item(k: String) = Button(k, k)
    now(host.render(Column(Vector(item("a"), item("b"), item("c")), "l")))
    val before = root.getComponent(0).asInstanceOf[Container].getComponents.toVector
    now(host.render(Column(Vector(item("c"), item("a"), item("b")), "l")))
    val after = root.getComponent(0).asInstanceOf[Container].getComponents.toVector
    assertEquals(after.map(_.asInstanceOf[JButton].getText), Vector("c", "a", "b"))
    assertEquals(after.toSet, before.toSet)   // moved, not rebuilt
  }

  test("delegated events round-trip by key against the mirror; a patch's own change is not a user") {
    val (root, b) = mount()
    val host = Ui.diffing(b)
    val got = scala.collection.mutable.Buffer[Event]()
    // written from the collector's fiber and READ from this thread,
    // so the COUNT is atomic; the buffer is only inspected once the
    // count says the events have arrived
    val arrived = java.util.concurrent.atomic.AtomicInteger(0)
    val drain = Async.spawn(Writer.uncons[Event, Unit, Async](b.events).flatMap {
      def loop(r: Either[Unit, (Event, Source[Event])]): Unit ! Async = r match
        case Left(_) => pure(())
        case Right((e, more)) =>
          got += e; arrived.incrementAndGet()
          Writer.uncons[Event, Unit, Async](more).flatMap(loop)
      loop
    })
    now(host.render(Column(Vector(
      Button("go", "go"), Input("", "name", "Name"), Check(false, "ok", "Ok"),
      Select(Vector("x", "y"), 0, "sel"), Button("nokey", "")), "app")))
    val panel = root.getComponent(0).asInstanceOf[Container]
    panel.getComponent(0).asInstanceOf[JButton].doClick()
    panel.getComponent(1).asInstanceOf[Container].getComponents.collectFirst { case t: JTextField => t }.get.setText("Ada")
    panel.getComponent(2).asInstanceOf[JCheckBox].doClick()
    panel.getComponent(3).asInstanceOf[JComboBox[String]].setSelectedIndex(1)
    panel.getComponent(4).asInstanceOf[JButton].doClick()
    // a server-side SetValue is applied, not spoken
    now(host.render(Column(Vector(
      Button("go", "go"), Input("server", "name", "Name"), Check(false, "ok", "Ok"),
      Select(Vector("x", "y"), 0, "sel"), Button("nokey", "")), "app")))
    // WAIT FOR THE FOUR EVENTS, not for 50 ms. The four gestures above
    // — click, type, tick, choose — each speak once, and the
    // server-side SetValue that follows must speak NOTHING, which is
    // what the last assertion checks. Sleeping asked the clock how
    // long that takes on this machine today; under a loaded gate the
    // answer was "longer than 50 ms" and the suite failed on the
    // count (gate-honesty, 2026-09-09).
    awaitAtLeast(arrived, 4, "the four gestures to be heard")
    assertEquals(got.headOption, Some(Event.Pressed("go")))
    assert(got.contains(Event.Edited("name", "Ada")), got.toString)
    assert(got.contains(Event.Toggled("ok", true)), got.toString)
    assert(got.contains(Event.Chosen("sel", 1)), got.toString)
    assert(!got.exists { case Event.Edited(_, "server") => true; case _ => false }, got.toString)
    assertEquals(got.count(_ == Event.Pressed("go")), 1)   // the keyless button spoke nothing
    val _ = drain
  }
}
