package okay.openapi

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.codec.Json.*
import okay.http.{Method, Request, Response, Route, Router}

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

  val router: Router = Router.empty
    .on(Method.Get, board)(_ => ok("all"))
    .on(Method.Get, byId)(t => ok(s"one ${t.head}"))
    .json[EmptyTuple, NewTask](Method.Post, board)((_, t) => ok(t.title))

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

  test("a path with a variable is a template, and its parameter is declared") {
    val item = at(at(doc, "paths"), "/board/{id}")
    val params = at(at(item, "get"), "parameters")
    assertEquals(params, JArr(Vector(JObj(Vector(
      "name" -> JStr("id"), "in" -> JStr("path"),
      "required" -> JBool(true),
      // the KIND is not on the entry — see OpenApi's comment and
      // BACKLOG "openapi": `Route.int("id")` is a string here, and the
      // day okay-http carries the kind this assertion changes
      "schema" -> JObj(Vector("type" -> JStr("string"))))))))
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
    val r = Router.empty.out[Int *: EmptyTuple, Task](Method.Get, byId)(t => pure(Task(t.head, "x")))
    val op = at(at(at(OpenApi.document(api, r), "paths"), "/board/{id}"), "get")
    val ok = at(at(op, "responses"), "200")
    assertEquals(at(at(at(ok, "content"), "application/json"), "schema"),
      okay.codec.JsonSchema.of(summon[Schema[Task]]))
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

  test("the document round-trips as JSON") {
    val text = OpenApi.print(api, router)
    assertEquals(Json.parse(text), doc)
  }
