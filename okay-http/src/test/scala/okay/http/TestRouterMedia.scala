package okay.http

import okay.*

/**
 * A DECLARED ANSWER THAT IS NOT JSON (openapi-media).
 *
 * `out`/`jsonOut` declare by construction — the router encodes with
 * the schema the entry carries — but they can only encode JSON, and
 * most services answer something else somewhere: a page, a stream, a
 * bundle. Those handlers built their own `Response`, so they declared
 * nothing and a document said `undeclared` for every one of them.
 *
 * These combinators keep the same property for media that has no
 * `Schema`: the ROUTER writes the content-type, from the same value
 * the entry declares, so the two cannot drift. The last test is that
 * property as a law rather than an example.
 */
class TestRouterMedia extends munit.FunSuite:

  // `runAsync`, not the blocking runner: this file is compiled for JS
  // too, where `CanBlock` does not exist at compile time
  import scala.concurrent.{Future, ExecutionContext}
  import ExecutionContext.Implicits.global
  private def answer(r: Router, req: Request): Future[Response] =
    Async.runAsync(r.routes(req))

  private def contentType(res: Response): String =
    res.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("content-type") => v }.getOrElse("")

  test("html: the handler answers text, the router says what it is") {
    val r = Router.empty.html(Method.Get, Route.root)(_ => pure("<h1>hi</h1>"))
    assertEquals(r.entries.head.answers.head.media, "text/html")
    answer(r, Request.get("/")).flatMap { res =>
      assertEquals(res.status, 200)
      assertEquals(contentType(res), "text/html; charset=utf-8")
      Async.runAsync(Http.text(res)).map(t => assertEquals(t, "<h1>hi</h1>"))
    }
  }

  test("bytes: the media is the author's, the encoding is the router's") {
    val r = Router.empty.bytes(Method.Get, Route / "app.js", "text/javascript")(
      _ => pure("export const x = 1".getBytes(java.nio.charset.StandardCharsets.UTF_8)))
    assertEquals(r.entries.head.answers.head.media, "text/javascript")
    answer(r, Request.get("/app.js")).flatMap { res =>
      assertEquals(contentType(res), "text/javascript")
      Async.runAsync(Http.text(res)).map(t => assertEquals(t, "export const x = 1"))
    }
  }

  test("events: a stream declares text/event-stream and still streams") {
    val r = Router.empty.events(Method.Get, Route / "events")(_ =>
      pure(Http.one("data: one\n\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))))
    assertEquals(r.entries.head.answers.head.media, "text/event-stream")
    answer(r, Request.get("/events")).flatMap { res =>
      assertEquals(contentType(res), "text/event-stream")
      Async.runAsync(Http.text(res)).map(t => assertEquals(t, "data: one\n\n"))
    }
  }

  test("a path parameter reaches a media handler like any other") {
    val r = Router.empty.html(Method.Get, Route / "u" / Route[String]("name"))(n => pure(s"<p>$n</p>"))
    answer(r, Request.get("/u/ann")).flatMap(res =>
      Async.runAsync(Http.text(res)).map(t => assertEquals(t, "<p>ann</p>")))
  }

  test("THE LAW: what the entry declares is what the wire carries") {
    // one router with every declaring shape in it; for each entry, the
    // media it declares is the media its own answer's content-type
    // names. A combinator that writes a header the entry does not
    // declare fails here, which is the whole reason to write the
    // header in the router and not in the handler.
    val r = Router.empty
      .html(Method.Get, Route / "page")(_ => pure("<p>p</p>"))
      .bytes(Method.Get, Route / "bundle", "application/octet-stream")(_ => pure(Array[Byte](1, 2)))
      .events(Method.Get, Route / "feed")(_ => pure(Http.one("x".getBytes)))
      .out[EmptyTuple, Int](Method.Get, Route / "count")(_ => pure(7))
    val urls = Vector("/page", "/bundle", "/feed", "/count")
    Future.traverse(r.entries.zip(urls).toVector) { (e, url) =>
      answer(r, Request.get(url)).map { res =>
        val declared = e.answers.find(_.status == res.status).map(_.media)
        val sent = contentType(res).takeWhile(_ != ';').trim
        assertEquals(declared, Some(sent), s"$url declares ${declared} and sends $sent")
      }
    }.map(_ => ())
  }
