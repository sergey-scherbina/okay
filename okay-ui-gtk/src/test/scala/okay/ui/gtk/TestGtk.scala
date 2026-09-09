package okay.ui.gtk

import okay.*
import okay.given
import okay.ui.*
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/**
 * The GTK host: the DOM/Swing law battery against a real GTK 4 widget
 * tree — patching frame by frame equals building the last frame — a
 * keyed shuffle MOVES the same widgets, and signals come back as our
 * events by key. Needs a display: when `gtk_init_check` fails the
 * suite says so and passes, rather than lying either way.
 */
class TestGtk extends munit.FunSuite {

  import Ui.*

  val display: Boolean = Gtk.init()

  def now[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** a widget tree as a string: type names and values, structurally */
  def show(w: Gtk4.Widget): String =
    val tpe = fromCString(Gtk4.g_type_name_from_instance(w))
    tpe match
      case "GtkLabel" => s"Label(${fromCString(Gtk4.gtk_label_get_text(w))})"
      case "GtkButton" => s"Button(${fromCString(Gtk4.gtk_button_get_label(w))})"
      case "GtkEntry" => s"Entry(${fromCString(Gtk4.gtk_editable_get_text(w))})"
      case "GtkCheckButton" => s"Check(${fromCString(Gtk4.gtk_check_button_get_label(w))},${Gtk4.gtk_check_button_get_active(w)})"
      case "GtkDropDown" => s"Select(${Gtk4.gtk_drop_down_get_selected(w).toInt})"
      case "GtkScrolledWindow" => s"Scroll(${show(Gtk.scrolled(w))})"
      case "GtkBox" =>
        var out = Vector.empty[String]
        var c = Gtk4.gtk_widget_get_first_child(w)
        while c != null do { out :+= show(c); c = Gtk4.gtk_widget_get_next_sibling(c) }
        out.mkString("Box[", ",", "]")
      case other => other

  def mount(): (Gtk4.Widget, Backend) =
    val root = Gtk4.gtk_box_new(GtkConst.VERTICAL, 0)
    (root, Gtk.backend(root))

  def fresh(ui: Ui): String =
    val (root, b) = mount()
    now(b.apply(Patch.Replace(Nil, ui)))
    show(Gtk4.gtk_widget_get_first_child(root))

  test("the law at GTK: patching frame by frame equals building the last frame") {
    assume(display, "no display: gtk_init_check failed — the GTK host is untested here")
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
      Items(Vector(Row(Vector(Text("one"), Button("x", "d0")), key = "i0"), Row(Vector(Text("two"), Button("x", "d1")), key = "i1")), "list"),
      Items(Vector(Row(Vector(Text("two"), Button("x", "d1")), key = "i1"), Row(Vector(Text("one!"), Button("x", "d0")), key = "i0"),
        Row(Vector(Text("three"), Button("x", "d2")), key = "i2")), "list"),
      Tabs(Vector("a", "b"), 1, Vector(Text("A"), Select(Vector("x", "y"), 1, "sel")), "t"))
    val (root, b) = mount()
    val host = Ui.diffing(b)
    frames.indices.foreach { i =>
      now(host.render(frames(i)))
      assertEquals(show(Gtk4.gtk_widget_get_first_child(root)), fresh(Ui.lower(frames(i), Set.empty)), s"after frame $i")
    }
  }

  test("a keyed shuffle MOVES widgets: the same pointers, reordered") {
    assume(display, "no display")
    val (root, b) = mount()
    val host = Ui.diffing(b)
    def item(k: String) = Button(k, k)
    def ptrs(): Vector[Long] =
      var out = Vector.empty[Long]
      var c = Gtk4.gtk_widget_get_first_child(Gtk4.gtk_widget_get_first_child(root))
      while c != null do { out :+= c.toLong; c = Gtk4.gtk_widget_get_next_sibling(c) }
      out
    now(host.render(Column(Vector(item("a"), item("b"), item("c")), "l")))
    val before = ptrs()
    now(host.render(Column(Vector(item("c"), item("a"), item("b")), "l")))
    val after = ptrs()
    assertEquals(after, Vector(before(2), before(0), before(1)))
  }

  test("signals come back as our events by key; a patch's own change is not a user") {
    assume(display, "no display")
    val root = Gtk4.gtk_box_new(GtkConst.VERTICAL, 0)
    val b = new Gtk.GtkBackend(root)
    val host = Ui.diffing(b)
    now(host.render(Column(Vector(
      Button("go", "go"), Input("", "name", "Name"), Check(false, "ok", "Ok"),
      Select(Vector("x", "y"), 0, "sel"), Button("nokey", "")), "app")))
    val panel = Gtk4.gtk_widget_get_first_child(root)
    def child(i: Int): Gtk4.Widget =
      var c = Gtk4.gtk_widget_get_first_child(panel); var n = i
      while n > 0 do { c = Gtk4.gtk_widget_get_next_sibling(c); n -= 1 }
      c
    Zone { Gtk4.g_signal_emit_by_name(child(0), toCString("clicked")) }
    Zone { Gtk4.gtk_editable_set_text(Gtk4.gtk_widget_get_next_sibling(Gtk4.gtk_widget_get_first_child(child(1))), toCString("Ada")) }
    Gtk4.gtk_check_button_set_active(child(2), 1)
    Gtk4.gtk_drop_down_set_selected(child(3), 1.toUInt)
    Zone { Gtk4.g_signal_emit_by_name(child(4), toCString("clicked")) }
    // a server-side SetValue is applied, not spoken
    now(host.render(Column(Vector(
      Button("go", "go"), Input("server", "name", "Name"), Check(false, "ok", "Ok"),
      Select(Vector("x", "y"), 0, "sel"), Button("nokey", "")), "app")))
    b.close()   // everything the widgets said is in the channel; read it all
    val got = scala.collection.mutable.Buffer[Event]()
    def take(src: Source[Event]): Unit ! Async =
      Writer.uncons[Event, Unit, Async](src).flatMap {
        case Left(_) => pure(())
        case Right((e, more)) => got += e; take(more)
      }
    now(take(b.events))
    assertEquals(got.headOption, Some(Event.Pressed("go")))
    assert(got.contains(Event.Edited("name", "Ada")), got.toString)
    assert(got.contains(Event.Toggled("ok", true)), got.toString)
    assert(got.contains(Event.Chosen("sel", 1)), got.toString)
    assert(!got.exists { case Event.Edited(_, "server") => true; case _ => false }, got.toString)
  }
}
