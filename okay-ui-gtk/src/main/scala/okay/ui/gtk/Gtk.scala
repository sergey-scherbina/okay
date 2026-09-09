package okay.ui.gtk

import okay.*
import okay.given
import okay.ui.*
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import Gtk4.Widget

/**
 * GTK 4 on Scala Native as a Backend (ui-gtk): the native toolkit leg
 * of specs/frontend.md, over the SAME seam as the DOM and Swing
 * backends and built the same way — the tree is the plan, `Ui.patch`
 * keeps a MIRROR (a patch is dispatched by what the mirror says is at
 * its path, so no widget is ever type-tested), `React.event` is the
 * one pure interpretation, and a patch path walks first-child /
 * next-sibling index for index. Every Ui child builds exactly ONE
 * widget (an Input's label wrapper is a leaf's root; a Scroll's child
 * is the scrolled window's child).
 *
 * GTK's rule that widgets are touched on its own thread is kept: a
 * patch applied from the GTK thread runs now, one from any other
 * thread is queued and run by `g_idle_add` inside the main loop —
 * which is why `window` pumps the loop until the application ends.
 *
 * Signal handlers are static C function pointers (they capture
 * nothing), so the widget → (key, kind) table they consult is global:
 * ONE live GTK backend per process, stated here.
 */
object Gtk {

  private final case class Bound(key: String, kind: String, backend: GtkBackend)
  private val bound = new java.util.HashMap[Long, Bound]()
  private val queue = new java.util.concurrent.ConcurrentLinkedQueue[() => Unit]()
  @volatile private var gtkThread: Thread = null

  /** GTK, initialised once; false means no display — say so, do not crash */
  def init(): Boolean =
    if gtkThread != null then true
    else if Gtk4.gtk_init_check() != 0 then { gtkThread = Thread.currentThread(); true }
    else false

  /** run what other threads queued — the idle callback and the tests' pump */
  def drain(): Unit =
    var f = queue.poll()
    while f != null do { f(); f = queue.poll() }

  /** run GTK's own loop until `done` says so, draining our queue as we go */
  def pump(done: () => Boolean): Unit =
    while !done() do
      drain()
      val _ = Gtk4.g_main_context_iteration(null, 0)

  private val idle: Gtk4.IdleCallback = CFuncPtr1.fromScalaFunction[Ptr[Byte], CInt] { _ => drain(); 0 }
  private val onClick: Gtk4.Callback = CFuncPtr2.fromScalaFunction[Ptr[Byte], Ptr[Byte], Unit] { (w, _) =>
    val b = bound.get(w.toLong)
    if b != null then b.backend.emit(b.key, "click", "")
  }
  private val onChanged: Gtk4.Callback = CFuncPtr2.fromScalaFunction[Ptr[Byte], Ptr[Byte], Unit] { (w, _) =>
    val b = bound.get(w.toLong)
    if b != null then b.backend.emit(b.key, "input", fromCString(Gtk4.gtk_editable_get_text(w)))
  }
  private val onToggled: Gtk4.Callback = CFuncPtr2.fromScalaFunction[Ptr[Byte], Ptr[Byte], Unit] { (w, _) =>
    val b = bound.get(w.toLong)
    if b != null then b.backend.emit(b.key, "change", "")
  }
  private val onSelected: Gtk4.NotifyCallback = CFuncPtr3.fromScalaFunction[Ptr[Byte], Ptr[Byte], Ptr[Byte], Unit] { (w, _, _) =>
    val b = bound.get(w.toLong)
    if b != null then b.backend.chose(b.key, Gtk4.gtk_drop_down_get_selected(w).toInt)
  }

  private def connect(w: Widget, signal: String, handler: CVoidPtr): Unit =
    Zone { val _ = Gtk4.g_signal_connect_data(w, toCString(signal), handler, null, null, 0) }

  private def cstr[A](s: String)(f: CString => A): A = Zone { f(toCString(s)) }

  /** a scrolled window's child, past the GtkViewport GTK wraps a
   * non-scrollable child in (found by a Gtk-CRITICAL: a SetText that
   * landed on the viewport while the law compared two viewports) */
  def scrolled(sw: Widget): Widget =
    val c = Gtk4.gtk_scrolled_window_get_child(sw)
    if c != null && fromCString(Gtk4.g_type_name_from_instance(c)) == "GtkViewport" then Gtk4.gtk_viewport_get_child(c) else c

  /** the node a path names in a tree — patches dispatch by it */
  private def nodeAt(u: Ui, path: List[Int]): Option[Ui] = path match
    case Nil => Some(u)
    case i :: rest => (u match
      case Ui.Row(c, _) => c.lift(i)
      case Ui.Column(c, _) => c.lift(i)
      case Ui.Box(c, _, _, _, _, _) => c.lift(i)
      case Ui.Form(f, s, k) => (f :+ Ui.Button(s, k, Role.Primary)).lift(i)
      case Ui.Scroll(c, _) => if i == 0 then Some(c) else None
      case _ => None).flatMap(nodeAt(_, rest))

  final class GtkBackend(root: Widget) extends Backend:
    private val feed = Channel[Event]()
    private var mirror: Ui = Ui.Text("")
    private var applying = false

    def events: Source[Event] = Writer.of(feed)
    /** the window closed: no more events */
    def close(): Unit = feed.close()

    def apply(p: Patch): Unit ! Async = async {
      if Thread.currentThread() eq gtkThread then applyNow(p)
      else
        queue.add(() => applyNow(p))
        val _ = Gtk4.g_idle_add(idle, null)
    }

    private[Gtk] def emit(key: String, kind: String, value: String): Unit =
      if !applying then React.event(mirror, key, kind, value).foreach(feed.offer)
    private[Gtk] def chose(key: String, i: Int): Unit =
      if !applying && Ui.keys(mirror)(key) then { val _ = feed.offer(Event.Chosen(key, i)) }

    private def applyNow(p: Patch): Unit =
      applying = true
      try step(p) finally applying = false
      mirror = Ui.patch(mirror, p)

    /** the i-th child, walking first-child / next-sibling */
    private def child(parent: Widget, i: Int): Widget =
      var c = Gtk4.gtk_widget_get_first_child(parent)
      var n = i
      while n > 0 && c != null do { c = Gtk4.gtk_widget_get_next_sibling(c); n -= 1 }
      c
    private def kids(parent: Widget): Vector[Widget] =
      var out = Vector.empty[Widget]
      var c = Gtk4.gtk_widget_get_first_child(parent)
      while c != null do { out :+= c; c = Gtk4.gtk_widget_get_next_sibling(c) }
      out
    private def at(path: List[Int], from: Ui): Widget =
      // a Scroll's child is the scrolled window's child, not a sibling walk
      def go(w: Widget, u: Ui, rest: List[Int]): Widget = rest match
        case Nil => w
        case i :: more => u match
          case Ui.Scroll(c, _) => go(scrolled(w), c, more)
          case _ => go(child(w, i), nodeAt(u, List(i)).getOrElse(Ui.Text("")), more)
      go(Gtk4.gtk_widget_get_first_child(root), from, path)
    /** the editable inside a leaf's root (a labelled Input is a box) */
    private def editable(w: Widget, u: Ui): Widget = u match
      case Ui.Input(_, _, label, _, _) if label.nonEmpty => child(w, 1)
      case _ => w

    private def step(p: Patch): Unit = p match
      case Patch.Replace(Nil, ui) =>
        kids(root).foreach(Gtk4.gtk_box_remove(root, _))
        Gtk4.gtk_box_append(root, build(ui))
      case Patch.Replace(path, ui) =>
        val parentNode = nodeAt(mirror, path.init).getOrElse(Ui.Text(""))
        val parent = at(path.init, mirror)
        parentNode match
          case Ui.Scroll(_, _) => Gtk4.gtk_scrolled_window_set_child(parent, build(ui))
          case _ =>
            val old = child(parent, path.last)
            Gtk4.gtk_box_insert_child_after(parent, build(ui), old)
            Gtk4.gtk_box_remove(parent, old)
      case Patch.SetText(path, s) => cstr(s)(Gtk4.gtk_label_set_text(at(path, mirror), _))
      case Patch.SetValue(path, v) =>
        val node = nodeAt(mirror, path).getOrElse(Ui.Text(""))
        cstr(v)(Gtk4.gtk_editable_set_text(editable(at(path, mirror), node), _))
      case Patch.SetChecked(path, on) => Gtk4.gtk_check_button_set_active(at(path, mirror), if on then 1 else 0)
      case Patch.SetSelected(path, i) => Gtk4.gtk_drop_down_set_selected(at(path, mirror), i.toUInt)
      case Patch.Remove(path, i) =>
        val parent = at(path, mirror)
        Gtk4.gtk_box_remove(parent, child(parent, i))
      case Patch.Reorder(path, order) =>
        val parent = at(path, mirror)
        val snapshot = kids(parent)
        var prev: Widget = null
        order.foreach { i =>
          Gtk4.gtk_box_reorder_child_after(parent, snapshot(i), prev)
          prev = snapshot(i)
        }
      case Patch.Insert(path, i, ui) =>
        val parent = at(path, mirror)
        Gtk4.gtk_box_insert_child_after(parent, build(ui), if i == 0 then null else child(parent, i - 1))

    private def bind(w: Widget, key: String, kind: String): Unit =
      if key.nonEmpty then bound.put(w.toLong, Bound(key, kind, this)): Unit

    private def box(orientation: Int, children: Vector[Widget], gap: Int, pad: Int, weights: Vector[Int]): Widget =
      val b = Gtk4.gtk_box_new(orientation, gap * 8)
      if pad > 0 then
        Gtk4.gtk_widget_set_margin_start(b, pad * 8); Gtk4.gtk_widget_set_margin_end(b, pad * 8)
        Gtk4.gtk_widget_set_margin_top(b, pad * 8); Gtk4.gtk_widget_set_margin_bottom(b, pad * 8)
      children.zipWithIndex.foreach { (c, i) =>
        // GTK has no weights: a weighted child EXPANDS along the axis
        if weights.length == children.length && weights(i) > 0 then
          if orientation == GtkConst.HORIZONTAL then Gtk4.gtk_widget_set_hexpand(c, 1) else Gtk4.gtk_widget_set_vexpand(c, 1)
        Gtk4.gtk_box_append(b, c)
      }
      b

    /** the tree, built — level S arrives lowered (`Ui.diffing` lowers) */
    private def build(ui: Ui): Widget = ui match
      case Ui.Text(s, style) =>
        val l = cstr(s)(Gtk4.gtk_label_new)
        if style.bold || style.tone == Tone.Emphasis then cstr("heading")(Gtk4.gtk_widget_add_css_class(l, _))
        if style.dim || style.tone == Tone.Muted then cstr("dim-label")(Gtk4.gtk_widget_add_css_class(l, _))
        if style.tone == Tone.Danger then cstr("error")(Gtk4.gtk_widget_add_css_class(l, _))
        if style.size == okay.ui.Size.Large then cstr("title-2")(Gtk4.gtk_widget_add_css_class(l, _))
        if style.size == okay.ui.Size.Small then cstr("caption")(Gtk4.gtk_widget_add_css_class(l, _))
        l
      case Ui.Row(children, _) => box(GtkConst.HORIZONTAL, children.map(build), 0, 0, Vector.empty)
      case Ui.Column(children, _) => box(GtkConst.VERTICAL, children.map(build), 0, 0, Vector.empty)
      case Ui.Box(children, dir, weights, gap, pad, _) =>
        box(if dir == Dir.Horizontal then GtkConst.HORIZONTAL else GtkConst.VERTICAL, children.map(build), gap, pad, weights)
      case Ui.Form(fields, submit, key) =>
        box(GtkConst.VERTICAL, fields.map(build) :+ build(Ui.Button(submit, key, Role.Primary)), 0, 0, Vector.empty)
      case Ui.Scroll(c, _) =>
        val sw = Gtk4.gtk_scrolled_window_new()
        Gtk4.gtk_scrolled_window_set_child(sw, build(c))
        sw
      case Ui.Image(_, alt) => cstr(s"[image: $alt]")(Gtk4.gtk_label_new)
      case Ui.Button(label, key, role) =>
        val b = cstr(label)(Gtk4.gtk_button_new_with_label)
        role match
          case Role.Primary => cstr("suggested-action")(Gtk4.gtk_widget_add_css_class(b, _))
          case Role.Danger => cstr("destructive-action")(Gtk4.gtk_widget_add_css_class(b, _))
          case _ => ()
        bind(b, key, "click")
        connect(b, "clicked", CFuncPtr.toPtr(onClick))
        b
      case Ui.Input(value, key, label, kind, _) =>
        val e = Gtk4.gtk_entry_new()
        cstr(value)(Gtk4.gtk_editable_set_text(e, _))
        if kind == InputKind.Secret then Gtk4.gtk_entry_set_visibility(e, 0)
        bind(e, key, "input")
        connect(e, "changed", CFuncPtr.toPtr(onChanged))
        if label.isEmpty then e
        else box(GtkConst.HORIZONTAL, Vector(cstr(label + ": ")(Gtk4.gtk_label_new), e), 0, 0, Vector.empty)
      case Ui.Check(on, key, label) =>
        val c = cstr(label)(Gtk4.gtk_check_button_new_with_label)
        Gtk4.gtk_check_button_set_active(c, if on then 1 else 0)
        bind(c, key, "change")
        connect(c, "toggled", CFuncPtr.toPtr(onToggled))
        c
      case Ui.Select(options, selected, key) =>
        val d = Zone {
          val arr = alloc[CString](options.length + 1)
          options.zipWithIndex.foreach((o, i) => arr(i) = toCString(o))
          arr(options.length) = null
          Gtk4.gtk_drop_down_new_from_strings(arr)
        }
        if options.nonEmpty then Gtk4.gtk_drop_down_set_selected(d, math.min(math.max(selected, 0), options.length - 1).toUInt)
        bind(d, key, "change")
        connect(d, "notify::selected", CFuncPtr.toPtr(onSelected))
        d
      case semantic => build(Ui.lower(semantic, Set.empty))

  /** a Backend over a vertical box that will hold the tree's one root */
  def backend(root: Widget): Backend = new GtkBackend(root)

  /** the Host: the core diff over the GTK backend */
  def host(root: Widget): Host = Ui.diffing(backend(root))

  /**
   * A window running an application: GTK on this thread (its rule),
   * the application on the scheduler's, patches marshalled in
   * between; answers when the application does.
   */
  def window[A](title: String)(app: Host => A ! Async)(using Scheduler): Option[A] =
    if !init() then None
    else
      val win = Gtk4.gtk_window_new()
      cstr(title)(Gtk4.gtk_window_set_title(win, _))
      Gtk4.gtk_window_set_default_size(win, 480, 360)
      val root = Gtk4.gtk_box_new(GtkConst.VERTICAL, 0)
      Gtk4.gtk_window_set_child(win, root)
      Gtk4.gtk_window_present(win)
      @volatile var result: Option[A] = None
      @volatile var done = false
      Async.spawn(app(host(root))).onComplete { r =>
        result = r.toOption; done = true
      }
      pump(() => done)
      Gtk4.gtk_window_destroy(win)
      result
}
