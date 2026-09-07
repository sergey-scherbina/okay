package okay.script

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Frame, Request}
import okay.ui.{Event, Patch, Ui, WireJson}

import java.nio.file.{Files, Path}

/** script-live-resume: a socket that carries the page's session cookie
 * resumes the state that cookie last reached; another cookie, or none,
 * starts from `init`. Driven in-JVM, no port. */
class TestLiveResume extends munit.FunSuite:

  private val page =
    """```scala declare
      |import okay.ui.*
      |import okay.script.api.*
      |val counter = Live(0)(n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc"))))(
      |  (n, e) => e match { case Event.Pressed("inc") => n + 1; case _ => n })
      |```
      |${mount("counter", counter)}
      |""".stripMargin

  private def withSite[A](f: Site => A): A =
    val root = Files.createTempDirectory("okay-script-live-resume-")
    Files.writeString(root.resolve("counter.md"), page): Unit
    val site = Site(root)
    try f(site)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  private def press(k: String) = Frame.Text(Json.print(WireJson.eventJson(Event.Pressed(k))))
  private val close = Frame.Close(1000, "")

  /** drive one socket: the frames in, the text lines out */
  private def drive(site: Site, cookie: Option[String], in: List[Frame]): Vector[String] =
    val headers = cookie.map(c => "Cookie" -> s"${Site.SessionCookie}=$c").toSeq
    val stage = site.ws(Request.get("/counter?__live=counter", headers))
    val (out, _) = !.run(Writer.run(through(Writer.of(in))(stage)))
    out.collect { case Frame.Text(s) => s }.toVector

  private def treeText(line: String): Option[Ui] = WireJson.uiOf(Json.parse(line))

  test("mounting a Live app opens the session, so the page sets the cookie a socket can resume by") {
    withSite { site =>
      val resp = site.handle(Request.get("/counter"))
      val setCookie = resp.headers.collect { case (k, v) if k.equalsIgnoreCase("set-cookie") => v }
      assert(setCookie.exists(_.startsWith(s"${Site.SessionCookie}=")), setCookie.toString)
    }
  }

  test("the same cookie resumes where the last socket left off; another cookie, or none, starts from init") {
    withSite { site =>
      val first = drive(site, Some("alice"), List(press("inc"), press("inc"), close))
      assertEquals(treeText(first(0)), Some(Ui.Column(Vector(Ui.Text("count: 0"), Ui.Button("+1", "inc")))))
      assertEquals(first.length, 3)
      // alice reconnects: her first frame is the full tree at the state she reached
      val again = drive(site, Some("alice"), List(press("inc"), close))
      assertEquals(treeText(again(0)), Some(Ui.Column(Vector(Ui.Text("count: 2"), Ui.Button("+1", "inc")))))
      assertEquals(WireJson.patchOf(Json.parse(again(1))), Some(Patch.SetText(List(0), "count: 3")))
      // and once more: the count kept climbing
      assertEquals(treeText(drive(site, Some("alice"), List(close))(0)),
        Some(Ui.Column(Vector(Ui.Text("count: 3"), Ui.Button("+1", "inc")))))
      // bob is not alice
      assertEquals(treeText(drive(site, Some("bob"), List(close))(0)),
        Some(Ui.Column(Vector(Ui.Text("count: 0"), Ui.Button("+1", "inc")))))
      // no cookie: from init, and nothing remembered
      assertEquals(treeText(drive(site, None, List(press("inc"), close))(0)),
        Some(Ui.Column(Vector(Ui.Text("count: 0"), Ui.Button("+1", "inc")))))
      assertEquals(treeText(drive(site, None, List(close))(0)),
        Some(Ui.Column(Vector(Ui.Text("count: 0"), Ui.Button("+1", "inc")))))
    }
  }
