package okay.ui

import okay.*
import okay.given
import scala.scalajs.js

/**
 * The raw-DOM patch Backend — the consumer `Host.diffing` was built
 * for: React-less DOM, zero dependencies, driven by the core diff.
 * Three existing pieces carry it: `React.elem` is the build plan,
 * `Ui.patch` keeps a MIRROR of the tree as a value (events interpret
 * against the mirror, not the DOM), and `React.event` is the same
 * pure interpretation the React host uses — one delegated listener
 * per kind, walking up to the nearest `data-key`.
 *
 * Paths walk `childNodes`: every Ui child builds exactly one root
 * element (Input/Check's label wrapper is a LEAF's root — no path
 * descends through it; a Select's options are DOM children but never
 * Ui children), so index-for-index navigation is sound.
 */
object Dom {

  /** a Backend over a document and a mount node — js.Dynamic, so a
   * real browser document and a test's fake both fit */
  def backend(document: js.Dynamic, root: js.Dynamic): Backend = new Backend:
    private val feed = Channel[Event]()
    private var mirror: Ui = Ui.Text("")

    for kind <- Seq("click", "input", "change") do
      root.addEventListener(kind,
        ((ev: js.Dynamic) => deliver(kind, ev)): js.Function1[js.Dynamic, Unit])

    def events: Source[Event] = Writer.of(feed)

    def apply(p: Patch): Unit ! Async = async {
      applyNow(p)
      mirror = Ui.patch(mirror, p)
    }

    private def kids(n: js.Dynamic): js.Array[js.Dynamic] =
      n.childNodes.asInstanceOf[js.Array[js.Dynamic]]
    private def at(path: List[Int]): js.Dynamic =
      path.foldLeft(kids(root)(0))((n, i) => kids(n)(i))

    private def applyNow(p: Patch): Unit = p match
      case Patch.Replace(Nil, ui) =>
        val built = build(React.elem(ui))
        if kids(root).length > 0 then root.replaceChild(built, kids(root)(0))
        else root.appendChild(built)
      case Patch.Replace(path, ui) =>
        val parent = at(path.init)
        parent.replaceChild(build(React.elem(ui)), kids(parent)(path.last))
      case Patch.SetText(path, s) => at(path).textContent = s
      case Patch.SetValue(path, v) => input(at(path)).value = v
      case Patch.SetChecked(path, on) => input(at(path)).checked = on
      case Patch.SetSelected(path, i) =>
        val n = at(path)
        n.selectedIndex = i
      case Patch.Remove(path, i) =>
        val n = at(path)
        n.removeChild(kids(n)(i))
      case Patch.Reorder(path, order) =>
        // appendChild MOVES a live node — the DOM's own primitive is
        // the patch's meaning, and a shuffle creates nothing
        val n = at(path)
        val snapshot = js.Array[js.Dynamic]()
        kids(n).foreach(snapshot.push(_))
        order.foreach(i => n.appendChild(snapshot(i)))
      case Patch.Insert(path, i, ui) =>
        val n = at(path)
        val ref = if i < kids(n).length then kids(n)(i) else null
        n.insertBefore(build(React.elem(ui)), ref)

    /** a leaf's editable element: the node itself, or the input
     * inside its label wrapper */
    private def input(n: js.Dynamic): js.Dynamic =
      val tag = n.tagName.toString.toLowerCase
      if tag == "input" || tag == "select" then n
      else kids(n).find(c => c.tagName.toString.toLowerCase == "input").getOrElse(n)

    /** an Elem, built — the plan is pure, this is the only builder */
    private def build(e: Elem): js.Dynamic =
      val n = document.createElement(e.tag)
      e.props.foreach { (k, v) =>
        if k == "className" then n.setAttribute("class", v)
        else if k == "checked" then n.updateDynamic("checked")(v == "true")
        else if k == "value" && e.tag != "select" then n.updateDynamic("value")(v)
        else if k == "value" then ()   // a select's value waits for its options
        else n.setAttribute(k, v)
      }
      e.text.foreach(t => n.textContent = t)
      e.children.foreach(c => n.appendChild(build(c)))
      if e.tag == "select" then
        val v = e.props.find(_._1 == "value").map(_._2).getOrElse("")
        n.selectedIndex = e.children.indexWhere(_.props.contains(("value", v)))
      n

    /** the delegation: nearest data-key up from the target, then the
     * SAME pure interpretation the React host uses, on the mirror */
    private def deliver(kind: String, ev: js.Dynamic): Unit =
      var n = ev.target
      var key = ""
      while key.isEmpty && n != null && !js.isUndefined(n) do
        if !js.isUndefined(n.getAttribute) then
          val a = n.getAttribute("data-key")
          if a != null && !js.isUndefined(a) then key = a.toString
        if key.isEmpty then n = n.parentNode
      if key.nonEmpty then
        val tv = ev.target.value
        val value = if js.isUndefined(tv) || tv == null then "" else tv.toString
        React.event(mirror, key, kind, value).foreach(feed.offer)
}
