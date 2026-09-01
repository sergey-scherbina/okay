package okay.ui

import okay.*
import okay.given
import scala.scalajs.js

/**
 * The fake document IS the specification of the glue's surface:
 * createElement, appendChild, replaceChild, removeChild,
 * insertBefore, setAttribute, addEventListener, textContent — what
 * Dom.scala uses and nothing more. It also counts creations, which is
 * how the test proves a Reorder MOVES nodes instead of rebuilding.
 */
class FakeNode(val tag: String) extends js.Object {
  val childNodes = js.Array[FakeNode]()
  var parentNode: FakeNode = null
  private val attrs = js.Dictionary[String]()
  private val handlers = js.Dictionary[js.Function1[js.Dynamic, Unit]]()
  var textContent: String = ""
  var value: String = ""
  var checked: Boolean = false
  var selectedIndex: Int = -1
  def tagName: String = tag.toUpperCase

  def appendChild(c: FakeNode): Unit =
    if c.parentNode != null then c.parentNode.removeChild(c)
    c.parentNode = this
    val _ = childNodes.push(c)
  def removeChild(c: FakeNode): Unit =
    val _ = childNodes.splice(childNodes.indexOf(c), 1)
    c.parentNode = null
  def replaceChild(nw: FakeNode, old: FakeNode): Unit =
    childNodes(childNodes.indexOf(old)) = nw
    nw.parentNode = this
    old.parentNode = null
  def insertBefore(nw: FakeNode, ref: FakeNode): Unit =
    if ref == null then appendChild(nw)
    else
      if nw.parentNode != null then nw.parentNode.removeChild(nw)
      val _ = childNodes.splice(childNodes.indexOf(ref), 0, nw)
      nw.parentNode = this
  def setAttribute(k: String, v: String): Unit = attrs(k) = v
  def getAttribute(k: String): String = attrs.get(k).orNull
  def addEventListener(k: String, f: js.Function1[js.Dynamic, Unit]): Unit = handlers(k) = f

  def fire(kind: String, target: FakeNode): Unit =
    val ev = FakeEvent(target).asInstanceOf[js.Dynamic]
    handlers.get(kind).foreach(f => f(ev))
  def attrList: String =
    attrs.toSeq.sortBy(_._1).map((k, v) => s"$k=$v").mkString(",")
}

class FakeEvent(val target: FakeNode) extends js.Object

class FakeDoc extends js.Object {
  var created = 0
  def createElement(tag: String): FakeNode = { created += 1; FakeNode(tag) }
}

class TestDom extends munit.FunSuite {

  import Ui.*

  /** the DOM as a string — selects show selectedIndex (their value
   * property is derived state in a real browser) */
  def show(n: FakeNode): String =
    val core = s"${n.tag}[${n.attrList}]"
    val state =
      if n.tag == "input" then s"v=${n.value},c=${n.checked}"
      else if n.tag == "select" then s"i=${n.selectedIndex}"
      else s"t=${n.textContent}"
    s"<$core $state>${n.childNodes.toSeq.map(show).mkString}</>"

  /** run a program whose Async is only Run — it completes in place */
  def now[A](prog: A ! Async): A =
    Async.runAsync(prog).value.get.get

  def mount(): (FakeDoc, FakeNode, Backend) =
    val doc = FakeDoc()
    val root = FakeNode("root")
    (doc, root, Dom.backend(doc.asInstanceOf[js.Dynamic], root.asInstanceOf[js.Dynamic]))

  /** the fresh build of a tree, for the law's right-hand side */
  def fresh(ui: Ui): FakeNode =
    val (_, root, b) = mount()
    now(b.apply(Patch.Replace(Nil, ui)))
    root.childNodes(0)

  def render(host: Host, frames: Ui*): Unit = frames.foreach(u => now(host.render(u)))

  test("the law at the DOM: patching frame by frame equals building the last frame") {
    val frames = Vector(
      Column(Vector(Text("hello"), Button("go", "go"), Input("", "name", "Name")), "app"),
      Column(Vector(Text("hello!"), Button("go", "go"), Input("Ada", "name", "Name")), "app"),
      // keyed shuffle plus churn
      Column(Vector(Input("Ada", "name", "Name"), Button("go", "go"), Check(true, "ok", "Ok")), "app"),
      // shape change: replace at the highest differing node
      Column(Vector(Row(Vector(Text("a"), Text("b")), "r")), "app"),
      Column(Vector(Row(Vector(Text("a"), Text("c")), "r")), "app"))
    val (_, root, b) = mount()
    val host = Ui.diffing(b)
    frames.indices.foreach { i =>
      render(host, frames(i))
      assertEquals(show(root.childNodes(0)), show(fresh(frames(i))), s"after frame $i")
    }
  }

  test("a keyed shuffle MOVES nodes — the fake counts zero creations") {
    def item(k: String) = Button(k, k)
    val before = Column(Vector("a", "b", "c", "d").map(item), "list")
    val after = Column(Vector("d", "c", "a", "b").map(item), "list")
    val (doc, root, b) = mount()
    val host = Ui.diffing(b)
    render(host, before)
    val builtOnce = doc.created
    render(host, after)
    assertEquals(doc.created, builtOnce, "a shuffle built new nodes")
    assertEquals(show(root.childNodes(0)), show(fresh(after)))
  }

  test("narrow patches narrow: SetText touches textContent, SetValue reaches through the label") {
    val v1 = Column(Vector(Text("count: 0"), Input("", "name", "Name")), "app")
    val v2 = Column(Vector(Text("count: 1"), Input("Ada", "name", "Name")), "app")
    val (_, root, b) = mount()
    val host = Ui.diffing(b)
    render(host, v1)
    val spanBefore = root.childNodes(0).childNodes(0)
    val labelBefore = root.childNodes(0).childNodes(1)
    render(host, v2)
    // the same nodes, mutated — not replaced
    assert(root.childNodes(0).childNodes(0) eq spanBefore)
    assert(root.childNodes(0).childNodes(1) eq labelBefore)
    assertEquals(spanBefore.textContent, "count: 1")
    assertEquals(labelBefore.childNodes(1).value, "Ada")
  }

  test("delegated events round-trip through the mirror; a keyless node is silent") {
    val ui = Column(Vector(
      Button("go", "go"), Input("Ada", "name", "Name"),
      Check(false, "ok", "Ok"), Select(Vector("x", "y"), 0, "pick"),
      Text("plain")), "app")
    val (_, root, b) = mount()
    now(Ui.diffing(b).render(ui))
    val app = root.childNodes(0)
    root.fire("click", app.childNodes(0))                   // the button itself
    val input = app.childNodes(1).childNodes(1)
    input.value = "Adam"
    root.fire("input", input)                               // nested in its label
    root.fire("change", app.childNodes(2).childNodes(0))    // the box inside the check label
    val select = app.childNodes(3)
    select.value = "y"
    root.fire("change", select)
    root.fire("click", app.childNodes(4))                   // keyless Text: silence
    def take(n: Int)(src: Source[Event]): Vector[Event] ! Async =
      if n == 0 then pure(Vector.empty)
      else Writer.uncons[Event, Unit, Async](src).flatMap {
        case Right((e, more)) => take(n - 1)(more).map(e +: _)
        case Left(_) => pure(Vector.empty)
      }
    val got = now(take(4)(b.events))
    assertEquals(got.length, 4)
    assertEquals(got(0), Event.Pressed("go"))
    assertEquals(got(1), Event.Edited("name", "Adam"))
    assertEquals(got(2), Event.Toggled("ok", true))
    assertEquals(got(3), Event.Chosen("pick", 1))
  }
}
