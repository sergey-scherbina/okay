package okay2.http

import java.nio.charset.StandardCharsets.UTF_8
import okay2.{!, pure}
import okay2.async.Async
import okay2.codec.Json
import Drive.run

final case class Login(email: String, code: String)

/** a declared request body (okay-http's TestRouteBody): a body that does
 * not decode is a 400 with data, and never reaches the handler — a
 * missing field and a non-JSON body are not "the empty string" */
class TestRouteBody extends munit.FunSuite {

  val blank: Response ! Async = pure[Async, Response](Response(200, Nil, Http.one(Array.empty[Byte])))

  def answer(r: Router, req: Request): Response = run(r.routes(req))
  def bodyOf(res: Response): String = new String(run(Http.bytes(res)).toArray, UTF_8)

  test("a declared body reaches the handler decoded") {
    var seen = Login("", "")
    val r = Router.json[Unit, Login](Method.Post, Route / "login")((_, b) => { seen = b; blank })
    assertEquals(answer(r, Request.post("/login", Body.Text("""{"email":"ann@example.com","code":"123456"}"""))).status, 200)
    assertEquals(seen, Login("ann@example.com", "123456"))
  }

  test("a body that does not decode is 400, and never reaches the handler") {
    var reached = false
    val r = Router.json[Unit, Login](Method.Post, Route / "login")((_, _) => { reached = true; blank })
    assertEquals(answer(r, Request.post("/login", Body.Text("not json"))).status, 400)
    assertEquals(answer(r, Request.post("/login", Body.Text("""{"email":"a@b.c"}"""))).status, 400)
    assert(!reached, "a request that did not decode reached the handler")
  }

  test("the refusal answers with data, not an exception") {
    val r = Router.json[Unit, Login](Method.Post, Route / "login")((_, _) => blank)
    val text = bodyOf(answer(r, Request.post("/login", Body.Text("not json"))))
    assert(text.contains("error"), text)
    assert(Json.parse(text).isInstanceOf[Json.JObj], text)
  }

  test("isDefinedAt does not run the handler") {
    var ran = 0
    val r = Router.json[Unit, Login](Method.Post, Route / "login")((_, _) => { ran += 1; blank })
    val req = Request.post("/login", Body.Text("""{"email":"a@b.c","code":"1"}"""))
    assert(r.routes.isDefinedAt(req))
    assertEquals(ran, 0, "isDefinedAt ran the handler")
    val _ = r.routes(req)
    assertEquals(ran, 1)
  }

  test("a handler that is not reached does not run either") {
    var ran = 0
    val r = Router.at(Method.Get, Route / "a")((_, _) => { ran += 1; blank }).at(Method.Get, Route / "b")((_, _) => { ran += 1; blank })
    val _ = r.routes(Request.get("/b"))
    assertEquals(ran, 1, "a route that did not match still ran")
  }

  test("a declared body is on the entry, for a renderer to read") {
    assert(Router.json[Unit, Login](Method.Post, Route / "login")((_, _) => blank).entries.head.body.isDefined)
    assertEquals(Router.on(Method.Get, Route / "healthz")(_ => blank).entries.head.body, None)
  }
}
