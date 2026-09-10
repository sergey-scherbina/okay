package okay.http

import okay.*
import okay.given
import java.nio.charset.StandardCharsets.UTF_8

/**
 * A declared request body (specs/optics-outside.md, stage 7).
 *
 * JVM-only, and not because it touches a socket — it binds nothing and
 * is not `Live`. It is here because the assertions need to RUN a
 * `Response ! Async`, and `!.run` only takes a pure program; the shared
 * suite next door stays cross-platform by never running one.
 *
 * The defect these tests exist for: okay-demo read its login fields
 * with `Chat.fieldOf(r.body, "email")`, which answers the empty string
 * both for a missing field and for a body that is not JSON. The two
 * were indistinguishable, so a malformed request reached
 * `Login.confirm("", "")` and its caller was told 401, wrong or expired
 * code — a diagnosis about their credentials for a broken request.
 */
class TestRouteBody extends munit.FunSuite {

  final case class Login(email: String, code: String)
  given okay.codec.Schema[Login] = okay.codec.Schema.derived

  val blank: Response ! Async = pure(Response(200, Nil, Http.one(Array.empty[Byte])))

  def answer(r: Router, req: Request): Response = r.routes(req).runWith
  def bodyOf(res: Response): String = new String(Http.bytes(res).runWith.toArray, UTF_8)

  test("a declared body reaches the handler decoded") {
    var seen = Login("", "")
    val r = Router.json[EmptyTuple, Login](Method.Post, Route / "login")((_, b) => { seen = b; blank })
    val res = answer(r, Request.post("/login", Body.Text("""{"email":"ann@example.com","code":"123456"}""")))
    assertEquals(res.status, 200)
    assertEquals(seen, Login("ann@example.com", "123456"))
  }

  test("a body that does not decode is 400, and never reaches the handler") {
    var reached = false
    val r = Router.json[EmptyTuple, Login](Method.Post, Route / "login")((_, _) => { reached = true; blank })

    // not JSON at all — the case fieldOf could not tell from a missing
    // field, and which used to arrive at the handler as ("", "")
    assertEquals(answer(r, Request.post("/login", Body.Text("not json"))).status, 400)
    // JSON, but a required field is missing
    assertEquals(answer(r, Request.post("/login", Body.Text("""{"email":"a@b.c"}"""))).status, 400)

    assert(!reached, "a request that did not decode reached the handler")
  }

  test("the refusal answers with data, not an exception") {
    val r = Router.json[EmptyTuple, Login](Method.Post, Route / "login")((_, _) => blank)
    val text = bodyOf(answer(r, Request.post("/login", Body.Text("not json"))))
    assert(text.contains("error"), text)
    assert(okay.codec.Json.parse(text).isInstanceOf[okay.codec.Json.JObj], text)
  }

  test("isDefinedAt does not run the handler") {
    // the defect this replaces: `routes` was Function.unlift(find), so
    // isDefinedAt called the handler to discover whether it matched.
    // Harmless while every handler merely BUILT a program, and a real
    // one the moment a handler did work outside it — okay-demo's
    // /login/confirm spends a one-time code, so an isDefinedAt followed
    // by an apply spent it twice and answered 401 to a correct code.
    var ran = 0
    val r = Router.json[EmptyTuple, Login](Method.Post, Route / "login")((_, _) => { ran += 1; blank })
    val req = Request.post("/login", Body.Text("""{"email":"a@b.c","code":"1"}"""))
    assert(r.routes.isDefinedAt(req))
    assertEquals(ran, 0, "isDefinedAt ran the handler")
    val _ = r.routes(req)
    assertEquals(ran, 1)
  }

  test("a handler that is not reached does not run either") {
    var ran = 0
    val r = Router.at(Method.Get, Route / "a")((_, _) => { ran += 1; blank })
      .at(Method.Get, Route / "b")((_, _) => { ran += 1; blank })
    val _ = r.routes(Request.get("/b"))
    assertEquals(ran, 1, "a route that did not match still ran")
  }

  test("a declared body is on the entry, for a renderer to read") {
    val r = Router.json[EmptyTuple, Login](Method.Post, Route / "login")((_, _) => blank)
    assert(r.entries.head.body.isDefined)
    val plain = Router.on(Method.Get, Route / "healthz")(_ => blank)
    assertEquals(plain.entries.head.body, None)
  }
}
