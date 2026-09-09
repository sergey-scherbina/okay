package okay.script

import okay.*
import okay.given
import okay.http.{Frame, Request}
import okay.ui.{Event, Patch, Ui, Protocol}

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

  private val durablePage = page.replace("Live(0)", "Live.durable(0)")

  private def withSite[A](f: Site => A): A = withSiteOf(page, Sessions())(f)

  private def withSiteOf[A](text: String, sessions: Sessions)(f: Site => A): A =
    val root = Files.createTempDirectory("okay-script-live-resume-")
    Files.writeString(root.resolve("counter.md"), text): Unit
    val site = Site(root, sessions = sessions)
    try f(site)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  private def cookieOf(site: Site): String =
    val resp = site.handle(Request.get("/counter"))
    resp.headers.collect { case (k, v) if k.equalsIgnoreCase("set-cookie") && v.startsWith(s"${Site.SessionCookie}=") => v }
      .head.drop(Site.SessionCookie.length + 1).takeWhile(_ != ';')

  private def press(k: String) = Frame.Text(Protocol.eventLine(Event.Pressed(k)))
  private val close = Frame.Close(1000, "")

  /** drive one socket: the frames in, the text lines out */
  private def drive(site: Site, cookie: Option[String], in: List[Frame]): Vector[String] =
    val headers = cookie.map(c => "Cookie" -> s"${Site.SessionCookie}=$c").toSeq
    val stage = site.ws(Request.get("/counter?__live=counter", headers))
    val (out, _) = !.run(Writer.run(through(Writer.of(in))(stage)))
    out.collect { case Frame.Text(s) => s }.toVector

  private def treeText(line: String): Option[Ui] = Protocol.treeOf(line)

  test("durable: a Live.durable app keeps its state in the session, and a second Site over the same store resumes it") {
    val store = new okay.persist.MemoryStore
    val cookie = withSiteOf(durablePage, Sessions.persisted(store)) { site =>
      val c = cookieOf(site)
      val first = drive(site, Some(c), List(press("inc"), press("inc"), close))
      assertEquals(treeText(first(0)), Some(Ui.Column(Vector(Ui.Text("count: 0"), Ui.Button("+1", "inc")))))
      // the state is an attribute of the session the cookie names
      val sess = site.sessions.handle(Some(c))
      assert(sess.get("okay.live.counter").isDefined, sess.attributes.toString)
      c
    }
    // "a restart": a new Site, a new process's worth of memory, the same store
    withSiteOf(durablePage, Sessions.persisted(store)) { site =>
      assertEquals(treeText(drive(site, Some(cookie), List(press("inc"), close))(0)),
        Some(Ui.Column(Vector(Ui.Text("count: 2"), Ui.Button("+1", "inc")))))
      assertEquals(treeText(drive(site, Some(cookie), List(close))(0)),
        Some(Ui.Column(Vector(Ui.Text("count: 3"), Ui.Button("+1", "inc")))))
      // a cookie the store never saw binds nothing, mints nothing, resumes nothing
      val before = site.sessions.size
      assertEquals(treeText(drive(site, Some("stranger"), List(press("inc"), close))(0)),
        Some(Ui.Column(Vector(Ui.Text("count: 0"), Ui.Button("+1", "inc")))))
      assertEquals(site.sessions.size, before)
    }
    // the plain app forgets across Sites: memory only, as stated
    val plain = withSiteOf(page, Sessions.persisted(store)) { site =>
      val c = cookieOf(site)
      drive(site, Some(c), List(press("inc"), close)): Unit
      c
    }
    withSiteOf(page, Sessions.persisted(store)) { site =>
      assertEquals(treeText(drive(site, Some(plain), List(close))(0)),
        Some(Ui.Column(Vector(Ui.Text("count: 0"), Ui.Button("+1", "inc")))))
    }
  }

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
      assertEquals(Protocol.patchOf(again(1)), Some(Patch.SetText(List(0), "count: 3")))
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
