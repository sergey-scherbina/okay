package okay.openapi

import okay.*
import okay.codec.{Json, Schema}
import okay.codec.Json.*
import okay.http.{Method, Response, Route, Router, Request}
import okay.http.syntax.*
import okay.given

/**
 * The document is a rendering of the router (specs/openapi.md stage 0).
 *
 * The law the tests are built around: every entry the router
 * dispatches appears exactly once, and the document names no path the
 * router does not dispatch. It compares the renderer with its own
 * input — which is why the spec calls it a test OF the renderer and
 * not a reason FOR it — but it is the property that keeps a service
 * from being documented and unserved.
 */
class TestOpenApi extends munit.FunSuite:

  final case class NewTask(title: String, done: Boolean) derives Schema

  private def ok(s: String): Response ! Async =
    pure(Response(200, Nil, okay.http.Http.one(s.getBytes("UTF-8"))))

  val board = Route / "board"
  val byId  = Route / "board" / Route[Int]("id")
  // one of each kind of query declaration: required, optional, repeated
  val search = Route / "search" :? "q".as[String] :? "page".opt[Int] :? "tag".all[String]

  val router: Router = Router.empty
    .on(Method.Get, board)(_ => ok("all"))
    .on(Method.Get, byId)(id => ok(s"one $id"))
    .json[EmptyTuple, NewTask](Method.Post, board)((_, t) => ok(t.title))
    .on(Method.Get, search)(_ => ok("found"))

  val api = Api("Board", "1.0", servers = Vector("https://board.example"))
  def doc: Json = OpenApi.document(api, router)

  private def obj(j: Json): Vector[(String, Json)] = j match
    case JObj(fs) => fs
    case other => fail(s"not an object: $other")
  private def at(j: Json, k: String): Json =
    obj(j).collectFirst { case (n, v) if n == k => v }.getOrElse(fail(s"no field $k in $j"))

  test("the document declares 3.1 and what only the service knows") {
    assertEquals(at(doc, "openapi"), JStr("3.1.0"))
    assertEquals(at(at(doc, "info"), "title"), JStr("Board"))
    assertEquals(at(at(doc, "info"), "version"), JStr("1.0"))
    assertEquals(at(doc, "servers"), JArr(Vector(JObj(Vector("url" -> JStr("https://board.example"))))))
  }

  test("THE LAW: every entry once, and no path the router does not dispatch") {
    val paths = obj(at(doc, "paths")).map(_._1)
    assertEquals(paths.sorted, router.entries.map(_.path).distinct.sorted)
    // every (path, method) pair of the router, and nothing else
    val rendered = obj(at(doc, "paths")).flatMap((p, item) => obj(item).map((m, _) => (p, m)))
    val dispatched = router.entries.map(e => (e.path, e.method.name.toLowerCase))
    assertEquals(rendered.sorted, dispatched.sorted)
  }

  test("a path with a variable is a template, and its parameter is declared WITH ITS KIND") {
    val item = at(at(doc, "paths"), "/board/{id}")
    val params = at(at(item, "get"), "parameters")
    // `Route[Int]("id")` is an integer, and it is an integer because
    // the entry carries the route's own `Seg.Var`, not a `{name}`
    // re-parsed out of the template (openapi-parameters)
    assertEquals(params, JArr(Vector(JObj(Vector(
      "name" -> JStr("id"), "in" -> JStr("path"),
      "required" -> JBool(true),
      "schema" -> JObj(Vector("type" -> JStr("integer"))))))))
  }

  test("query parameters are declared too, with required and repeated") {
    val get = at(at(at(doc, "paths"), "/search"), "get")
    assertEquals(at(get, "parameters"), JArr(Vector(
      JObj(Vector("name" -> JStr("q"), "in" -> JStr("query"),
        "required" -> JBool(true),
        "schema" -> JObj(Vector("type" -> JStr("string"))))),
      JObj(Vector("name" -> JStr("page"), "in" -> JStr("query"),
        "required" -> JBool(false),
        "schema" -> JObj(Vector("type" -> JStr("integer"))))),
      // `all` is a repeated parameter: an array of the element's kind,
      // which is what the route actually accepts
      JObj(Vector("name" -> JStr("tag"), "in" -> JStr("query"),
        "required" -> JBool(false),
        "schema" -> JObj(Vector("type" -> JStr("array"),
          "items" -> JObj(Vector("type" -> JStr("string"))))))))))
  }

  test("the query is NOT part of the path template, and the path is the one that dispatches") {
    val paths = obj(at(doc, "paths")).map(_._1)
    assert(paths.contains("/search"), paths.toString)
    assert(!paths.exists(_.contains("?")), paths.toString)
  }

  test("a route without variables declares no parameters") {
    val get = at(at(at(doc, "paths"), "/board"), "get")
    assert(!obj(get).exists(_._1 == "parameters"), get)
  }

  test("the request body is the schema the DECODER was derived from, not a second one") {
    val post = at(at(at(doc, "paths"), "/board"), "post")
    val schema = at(at(at(at(post, "requestBody"), "content"), "application/json"), "schema")
    assertEquals(schema, okay.codec.JsonSchema.of(summon[Schema[NewTask]]))
  }

  test("a handler that builds its own Response declares nothing, and the document says so") {
    val get = at(at(at(doc, "paths"), "/board"), "get")
    val d = at(at(at(get, "responses"), "default"), "description")
    assert(Json.print(d).contains("undeclared"), d)
  }

  final case class Task(id: Int, title: String) derives Schema

  test("a handler that answers a VALUE declares its response by its own type") {
    val r = Router.empty.out[Int *: EmptyTuple, Task](Method.Get, byId)(id => pure(Task(id, "x")))
    val op = at(at(at(OpenApi.document(api, r), "paths"), "/board/{id}"), "get")
    val ok = at(at(op, "responses"), "200")
    assertEquals(at(at(at(ok, "content"), "application/json"), "schema"),
      okay.codec.JsonSchema.of(summon[Schema[Task]]))
  }

  test("a page declares text/html, and the content is keyed by the media it answers") {
    val r = Router.empty.html(Method.Get, board)(_ => pure("<h1>the board</h1>"))
    val op = at(at(at(OpenApi.document(api, r), "paths"), "/board"), "get")
    val ok = at(at(op, "responses"), "200")
    // the key is the media type; there is no schema, because HTML has
    // none — and an empty schema object is how OpenAPI says "content
    // of this type, shape unstated" rather than lying about a shape
    assertEquals(obj(at(ok, "content")).map(_._1), Vector("text/html"))
    assert(!Json.print(ok).contains("application/json"), Json.print(ok))
    assert(!Json.print(ok).contains("undeclared"), Json.print(ok))
  }

  test("a stream and a bundle each carry their own media") {
    val r = Router.empty
      .events(Method.Get, Route / "feed")(_ => pure(okay.http.Http.one("x".getBytes)))
      .bytes(Method.Get, Route / "app.js", "text/javascript")(_ => pure(Array[Byte](1)))
    val doc2 = OpenApi.document(api, r)
    val feed = at(at(at(at(at(at(doc2, "paths"), "/feed"), "get"), "responses"), "200"), "content")
    val js = at(at(at(at(at(at(doc2, "paths"), "/app.js"), "get"), "responses"), "200"), "content")
    assertEquals(obj(feed).map(_._1), Vector("text/event-stream"))
    assertEquals(obj(js).map(_._1), Vector("text/javascript"))
  }

  test("the router declares the failure IT produces, without the author writing it") {
    val r = Router.empty.jsonOut[EmptyTuple, NewTask, Task](Method.Post, board)((_, t) => pure(Task(1, t.title)))
    val op = at(at(at(OpenApi.document(api, r), "paths"), "/board"), "post")
    val codes = obj(at(op, "responses")).map(_._1)
    assertEquals(codes, Vector("200", "400"))
    val err = at(at(at(at(at(op, "responses"), "400"), "content"), "application/json"), "schema")
    assertEquals(at(err, "required"), JArr(Vector(JStr("error"))))
  }

  test("a declared status other than 200 is what the document says") {
    val r = Router.empty.out[EmptyTuple, Task](Method.Post, board, status = 201)(_ => pure(Task(1, "t")))
    val op = at(at(at(OpenApi.document(api, r), "paths"), "/board"), "post")
    assertEquals(obj(at(op, "responses")).map(_._1), Vector("201"))
  }

  test("operation ids are derived, stable, and distinct") {
    val ids = obj(at(doc, "paths")).flatMap((_, item) => obj(item).map((_, op) => at(op, "operationId")))
    assertEquals(ids.distinct.size, ids.size)
    assert(ids.contains(JStr("getBoardById")), ids.toString)
    assertEquals(OpenApi.document(api, router), doc)   // same input, same document
  }

  // stage 2: the readers
  test("the served document is the rendered one, byte for byte") {
    val served = OpenApi.routes(api, router)
    val res = Async.run(served.routes(Request.get("/openapi.json"))).runWith
    assertEquals(res.status, 200)
    assert(res.headers.exists((k, v) => k == "content-type" && v == "application/json"), res.headers.toString)
    val text = Async.run(okay.http.Http.text(res)).runWith
    assertEquals(Json.parse(text), doc)
  }

  test("the page is rendered by the server: no script, no network") {
    val served = OpenApi.routes(api, router)
    val res = Async.run(served.routes(Request.get("/openapi"))).runWith
    assertEquals(res.status, 200)
    val html = Async.run(okay.http.Http.text(res)).runWith
    assert(html.contains("<!doctype html>"), html.take(80))
    // every operation is IN the html, so a reader with no javascript
    // sees the same thing a machine does
    assert(html.contains("/board/{id}"), html)
    assert(html.contains("GET") && html.contains("POST"), html)
    // the page a browser cannot render offline is not documentation
    // in an air-gapped cluster: nothing is fetched
    assert(!html.contains("<script"), "the page must not need javascript")
    assert(!html.contains("http://") && !html.contains("https://cdn"), "the page must not reach the network")
  }

  test("the document describes the APPLICATION, not the routes that serve it") {
    val served = OpenApi.routes(api, router)
    val paths = obj(at(OpenApi.document(api, router), "paths")).map(_._1)
    assert(!paths.contains("/openapi.json"), paths.toString)
    // and the two surfaces are joined where every surface in this
    // stack is joined — `orElse` over the partial functions
    val whole = router.routes orElse served.routes
    assert(whole.isDefinedAt(Request.get("/openapi.json")))
    assert(whole.isDefinedAt(Request.get("/board")))
  }

  test("the page says what an undeclared operation is, rather than implying a 200") {
    val html = OpenApi.page(api, router)
    assert(html.contains("undeclared"), html)
  }

  test("the document round-trips as JSON") {
    val text = OpenApi.print(api, router)
    assertEquals(Json.parse(text), doc)
  }
