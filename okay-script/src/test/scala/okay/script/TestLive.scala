package okay.script

import okay.*
import okay.given
import okay.http.{Frame, Http, Request, Response as HttpResponse}
import okay.script.api.{Live, mount}
import okay.ui.{Event, Patch, Style, Ui, Protocol}

import java.nio.file.{Files, Path}

/** okay-script-live: okay-ui as the front-end layer -- a page mounts
 * a server-driven app, the container serves its SSR, its script and
 * its WebSocket session. See specs/okay-script.md "Live pages".
 */
class TestLive extends munit.FunSuite:

  private def counter: Live[Int] =
    Live(0)(n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc")), "root"))(
      (n, e) => e match { case Event.Pressed("inc") => n + 1; case _ => n })

  test("Live.html renders every shape to the HTML React.elem implies, escaped") {
    val tree = Ui.Column(Vector(
      Ui.Text("a <b>", Style(bold = true)),
      Ui.Row(Vector(Ui.Button("go", "k1")), "r"),
      Ui.Input("v\"", "k2", "name"),
      Ui.Check(true, "k3", "yes"),
      Ui.Select(Vector("x", "y"), 1, "k4"),
    ))
    assertEquals(Live.html(tree),
      """<div class="okay-col">""" +
        """<span class="okay-bold">a &lt;b&gt;</span>""" +
        """<div data-key="r" class="okay-row"><button data-key="k1">go</button></div>""" +
        """<label><span>name</span><input data-key="k2" value="v&quot;"></label>""" +
        """<label><input data-key="k3" type="checkbox" checked><span>yes</span></label>""" +
        """<select data-key="k4" value="y"><option value="x">x</option><option value="y">y</option></select>""" +
        "</div>")
  }

  test("mount outside a Site yields the SSR content plus the script tag") {
    val html = mount("c", counter)
    assert(html.startsWith("""<div id="okay-live-c" data-okay-live="c"><div data-key="root" class="okay-col"><span>count: 0</span>"""), html)
    assert(html.contains("""<script src="/__okay/live.js"></script><script>okayLive("c")</script>"""), html)
  }

  private val page =
    """```scala declare
      |import okay.ui.*
      |import okay.script.api.*
      |val counter = Live(0)(n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc"))))(
      |  (n, e) => e match { case Event.Pressed("inc") => n + 1; case _ => n })
      |```
      |# Counter
      |
      |${mount("counter", counter)}
      |""".stripMargin

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith

  private def withSite[A](body: (Site, Path) => A): A =
    val root = Files.createTempDirectory("okay-script-live-")
    Files.writeString(root.resolve("counter.md"), page): Unit
    val site = Site(root)
    try body(site, root)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("through Site.handle: SSR, the script, and ws defined for the page's ?__live -- even before a render") {
    withSite { (site, _) =>
      // a socket before any render: the container renders once to register
      assert(site.ws.isDefinedAt(Request.get("/counter?__live=counter")))
      assert(!site.ws.isDefinedAt(Request.get("/counter?__live=nope")))
      assert(!site.ws.isDefinedAt(Request.get("/missing?__live=counter")))

      val r = site.handle(Request.get("/counter"))
      assertEquals(r.status, 200)
      val t = text(r)
      assert(t.contains("<h1>Counter</h1>") || t.contains("# Counter"), t)
      assert(t.contains("""<span>count: 0</span><button data-key="inc">+1</button>"""), t)
      assert(t.contains("""okayLive("counter")"""), t)

      val js = site.handle(Request.get("/__okay/live.js"))
      assertEquals(js.status, 200)
      assert(js.headers.exists((k, v) => k.equalsIgnoreCase("content-type") && v.startsWith("text/javascript")))
      assert(text(js).contains("window.okayLive = function"))
      assert(site.routes.isDefinedAt(Request.get("/__okay/live.js")))
    }
  }

  test("the session in-JVM: the tree first, a press yields one narrow patch, a forged key nothing, Close ends it") {
    withSite { (site, _) =>
      val stage = site.ws(Request.get("/counter?__live=counter"))
      def press(k: String) = Frame.Text(Protocol.eventLine(Event.Pressed(k)))
      val (out, _) = !.run(Writer.run(through(Writer.of(List(press("inc"), press("forged"), Frame.Close(1000, ""), press("inc"))))(stage)))
      val lines = out.collect { case Frame.Text(s) => s }
      assertEquals(lines.length, 2, lines.toString)
      assertEquals(Protocol.treeOf(lines(0)), Some(Ui.Column(Vector(Ui.Text("count: 0"), Ui.Button("+1", "inc")))))
      assertEquals(Protocol.patchOf(lines(1)), Some(Patch.SetText(List(0), "count: 1")))
    }
  }
