package okay2.http

import java.nio.charset.StandardCharsets.UTF_8
import okay2.pure
import okay2.async.Async
import Drive.answer

/** a declared answer that is not JSON (okay-http's TestRouterMedia): the
 * ROUTER writes the content-type from the value the entry declares, so
 * the two cannot drift; the last test is that property as a law */
class TestRouterMedia extends munit.FunSuite {

  test("html: the handler answers text, the router says what it is") {
    val r = Router.empty.html(Method.Get, Route.root)(_ => pure[Async, String]("<h1>hi</h1>"))
    assertEquals(r.entries.head.answers.head.media, "text/html")
    assertEquals(answer(r, Request.get("/")), (200, "text/html; charset=utf-8", "<h1>hi</h1>"))
  }

  test("bytes: the media is the author's, the encoding is the router's") {
    val r = Router.empty.bytes(Method.Get, Route / "app.js", "text/javascript")(_ => pure[Async, Array[Byte]]("export const x = 1".getBytes(UTF_8)))
    assertEquals(r.entries.head.answers.head.media, "text/javascript")
    assertEquals(answer(r, Request.get("/app.js")), (200, "text/javascript", "export const x = 1"))
  }

  test("events: a stream declares text/event-stream and still streams") {
    val r = Router.empty.events(Method.Get, Route / "events")(_ => pure[Async, okay2.stream.Source[okay2.stream.Chunk[Byte]]](Http.one("data: one\n\n".getBytes(UTF_8))))
    assertEquals(r.entries.head.answers.head.media, "text/event-stream")
    assertEquals(answer(r, Request.get("/events")), (200, "text/event-stream", "data: one\n\n"))
  }

  test("a path parameter reaches a media handler like any other") {
    val r = Router.empty.html(Method.Get, Route / "u" / Route[String]("name"))(n => pure[Async, String](s"<p>$n</p>"))
    assertEquals(answer(r, Request.get("/u/ann"))._3, "<p>ann</p>")
  }

  test("THE LAW: what the entry declares is what the wire carries") {
    val r = Router.empty
      .html(Method.Get, Route / "page")(_ => pure[Async, String]("<p>p</p>"))
      .bytes(Method.Get, Route / "bundle", "application/octet-stream")(_ => pure[Async, Array[Byte]](Array[Byte](1, 2)))
      .events(Method.Get, Route / "feed")(_ => pure[Async, okay2.stream.Source[okay2.stream.Chunk[Byte]]](Http.one("x".getBytes)))
      .out[Unit, Int](Method.Get, Route / "count")(_ => pure[Async, Int](7))
    r.entries.zip(Vector("/page", "/bundle", "/feed", "/count")).foreach { case (e, url) =>
      val (status, ct, _) = answer(r, Request.get(url))
      val declared = e.answers.find(_.status == status).map(_.media)
      val sent = ct.takeWhile(_ != ';').trim
      assertEquals(declared, Some(sent), s"$url declares $declared and sends $sent")
    }
  }
}
