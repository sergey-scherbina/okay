package okay.ui

import okay.*
import okay.given
import scala.scalajs.js

/**
 * The js glue, and only the glue: the mapping to elements is PURE
 * (`React.elem`, tested on the JVM); this file converts an Elem to a
 * real `createElement` call against whatever React-shaped global it
 * is given, and feeds DOM events back as ours by data-key.
 */
object ReactJs {

  /** an Elem as a real element of a React-shaped library */
  def element(react: js.Dynamic, e: Elem, emit: Event => Unit, tree: () => Ui): js.Any =
    val props = js.Dictionary[js.Any]()
    var key = ""
    e.props.foreach { (k, v) =>
      if k == "data-key" then { key = v; props(k) = v }
      else if k == "checked" then props(k) = (v == "true")
      else props(k) = v
    }
    if key.nonEmpty then
      val k = key
      props("onClick") = ((_: js.Any) =>
        React.event(tree(), k, "click", "").foreach(emit)): js.Function1[js.Any, Unit]
      props("onInput") = ((ev: js.Dynamic) =>
        React.event(tree(), k, "input", ev.target.value.toString).foreach(emit)): js.Function1[js.Dynamic, Unit]
      props("onChange") = ((ev: js.Dynamic) =>
        React.event(tree(), k, "change", ev.target.value.toString).foreach(emit)): js.Function1[js.Dynamic, Unit]
    val children = e.children.map(c => element(react, c, emit, tree))
    val args = js.Array[js.Any](e.tag, props)
    e.text.foreach(t => args.push(t))
    children.foreach(c => args.push(c))
    react.applyDynamic("createElement")(args.toSeq*)

  /** a Host over a React-shaped root: render hands the whole tree to
   * the library, which is exactly what it wants */
  def host(react: js.Dynamic, root: js.Dynamic): Host = new Host:
    private val feed = Channel[Event]()
    @volatile private var current: Ui = Ui.Text("")
    def render(ui: Ui): Unit ! Async = async {
      current = ui
      root.render(element(react, React.elem(ui), feed.send, () => current))
      ()
    }
    def events: Source[Event] = Writer.of(feed)
}
