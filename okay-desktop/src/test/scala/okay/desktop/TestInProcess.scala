package okay.desktop

import okay.*
import okay.http.{Http, Method, Request, Response}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicInteger

/**
 * app-in-process (specs/app-in-process.md): a service's routes reached in
 * the process — a GET, a POST that redirects and the cookie it set, a 307
 * that keeps its method, the peer, the held answer, the `app://` scheme
 * itself — with no socket anywhere.
 */
class TestInProcess extends okay.testkit.Munit.Diagnosed:

  private val pages = AtomicInteger(0)
  private def ok(ct: String, s: String, more: (String, String)*): Response ! Async =
    pure(Response(200, Seq("content-type" -> ct) ++ more, Http.one(s.getBytes(UTF_8))))
  private def text(r: Request): String = new String(r.body.bytes, UTF_8)
  private def cookie(r: Request): String = r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("cookie") => v }.getOrElse("")

  private val routes: Routes =
    case r if r.url == "/page" => pages.incrementAndGet(); ok("text/html; charset=utf-8", "<p>page</p>")
    case r if r.method == Method.Post && r.url == "/login" =>
      pure(Response(303, Seq("location" -> "/me", "set-cookie" -> "s=abc; Path=/; HttpOnly"), Http.one(Array.emptyByteArray)))
    case r if r.url == "/me" => ok("text/plain", s"${r.method.name} cookie=${cookie(r)} peer=${r.peer.getOrElse("")} body=${text(r)}")
    case r if r.url == "/keep" => pure(Response(307, Seq("location" -> "/echo"), Http.one(Array.emptyByteArray)))
    case r if r.url == "/echo" => ok("text/plain", s"${r.method.name} ${text(r)}")
    case r if r.url == "/go" => pure(Response(303, Seq("location" -> "/page"), Http.one(Array.emptyByteArray)))
    case r if r.url == "/away" => pure(Response(302, Seq("location" -> "https://example.org/x"), Http.one(Array.emptyByteArray)))
    case r if r.url == "/report.csv" => ok("text/csv", "a,b\n1,2\n", "content-disposition" -> "attachment; filename=\"report.csv\"")
    case r if r.url == "/boom" => throw RuntimeException("boom")

  test("a GET answered; a POST that redirects lands on the GET, with the cookie it set and peer 127.0.0.1") {
    val s = InProcess.Server("t-send", routes)
    val page = s.send("GET", "/page")
    assertEquals((page.status, page.text, page.url), (200, "<p>page</p>", "app://t-send/page"))
    val landed = s.send("POST", "app://t-send/login", Seq("content-type" -> "application/x-www-form-urlencoded"), "who=anna".getBytes(UTF_8))
    note(s"landed: ${landed.status} ${landed.url} ${landed.text}")
    assertEquals(landed.url, "app://t-send/me", "the redirect was followed")
    assertEquals(landed.text, "GET cookie=s=abc peer=127.0.0.1 body=", "303 is a GET, the cookie kept, the body dropped")
    assertEquals(s.send("GET", "/me").text, "GET cookie=s=abc peer=127.0.0.1 body=", "the cookie goes with the next request")
  }

  test("307 keeps the method and the body; somewhere else is left as it is; a route that throws is a 500") {
    val s = InProcess.Server("t-307", routes)
    assertEquals(s.send("POST", "/keep", Nil, "x=1".getBytes(UTF_8)).text, "POST x=1")
    val away = s.send("GET", "/away")
    assertEquals((away.status, away.url), (302, "https://example.org/x"))
    val boom = s.send("GET", "/boom")
    assertEquals(boom.status, 500)
    assert(boom.text.contains("boom"), boom.text)
    assertEquals(s.send("GET", "/nothing").status, 404)
  }

  test("the held answer: the next GET of that URL is it, once") {
    val s = InProcess.Server("t-hold", routes)
    val a = Answer(200, Seq("content-type" -> "text/html"), "<p>held</p>".getBytes(UTF_8), "app://t-hold/page")
    s.hold("app://t-hold/page", a)
    assertEquals(s.take("/page").map(_.text), Some("<p>held</p>"))
    assertEquals(s.take("/page"), None, "once")
  }

  test("the app:// scheme: a page, a redirect as a page that goes on (its answer held), bytes with their type, an unknown host") {
    val s = InProcess.Server("t-scheme", routes)
    InProcess.install(s)
    def read(u: String): (String, String) =
      val c = java.net.URI.create(u).toURL.openConnection()
      (new String(c.getInputStream.readAllBytes(), UTF_8), c.getContentType)
    assertEquals(read("app://t-scheme/page"), ("<p>page</p>", "text/html; charset=utf-8"))
    val before = pages.get
    val (goOn, _) = read("app://t-scheme/go")
    assert(goOn.contains("url=app://t-scheme/page") && goOn.contains("location.replace('app://t-scheme/page')"), goOn)
    assertEquals(pages.get, before + 1, "the redirect's target was asked once")
    assertEquals(read("app://t-scheme/page")._1, "<p>page</p>")
    assertEquals(pages.get, before + 1, "the going-on page is answered from what the redirect already got")
    assertEquals(read("app://t-scheme/report.csv"), ("a,b\n1,2\n", "text/csv"))
    // ELSEWHERE (okay-watch's Go Pro): a page that asks the window for the system browser and steps back
    val (away, awayType) = read("app://t-scheme/away")
    assert(awayType.startsWith("text/html"), awayType)
    assert(away.contains("window.okayApp.external('https://example.org/x')") && away.contains("history.back()"), away)
    assert(read("app://nobody-here/page")._1.contains("Nothing here answers nobody-here"))
  }

  test("Transport.inProcess: the bytes a route gives and its file name; open holds the answer for its URL") {
    val s = InProcess.Server("t-transport", routes)
    val t = Transport.inProcess(s)
    val csv = t.send("GET", "app://t-transport/report.csv")
    assertEquals((csv.status, csv.text, csv.header("content-disposition")), (200, "a,b\n1,2\n", Some("attachment; filename=\"report.csv\"")))
    val landed = t.send("POST", "/login")
    assertEquals(t.open(landed), "app://t-transport/me")
    assertEquals(s.take("app://t-transport/me").map(_.text), Some(landed.text))
  }

  test("a host from an app's name") {
    assertEquals(InProcess.host("okay-watch"), "okay-watch")
    assertEquals(InProcess.host("My App 2"), "my-app-2")
    assertEquals(InProcess.host("***"), "app")
  }
