package okay.security

import okay.*
import okay.given
import okay.agent.{Agent, Compact, Context, Handlers, Model, Reply, Tool, ToolCall, ToolSpec}
import okay.codec.{Json, Schema}
import okay.http.{Http, Request, Response, Server, Transports}

/**
 * THE MODULE READMES, COMPILED — okay-security's, okay-agent's and
 * okay-http's, the way TestMcpReadme already does for okay-mcp's.
 *
 * A readme whose examples do not compile is worse than none, and the
 * only check that cannot be forgotten is one the build runs. It lives
 * here because okay-security is the module that can see the other
 * three; when an example there changes, change it here and let the
 * compiler agree.
 */
class TestReadmes extends munit.FunSuite {

  val secret = "a-shared-secret-for-the-readme".getBytes("UTF-8")
  val now = System.currentTimeMillis() / 1000
  val rootKey: Array[Byte] = Array.tabulate(32)(i => (i * 7 + 3).toByte)
  val deadline = System.currentTimeMillis() + 60_000

  // ── okay-security/README.md ────────────────────────────────────

  test("security: protecting a route") {
    val verify: String => Verified =
      (t: String) => Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now)

    val route = Secure.bearer(verify, Policy.scoped("read")) { principal =>
      { case r if Server.path(r) == "/api/data" =>
          Server.text(200, s"seen by ${principal.id}") }
    }

    val token = Jwt.sign(Claims(subject = Some("ann"), audience = Vector("api"),
      expires = Some(now + 600), scopes = Set("read")), Jwt.Key.Hmac(secret))
    val ok = Request.get("/api/data", Seq(("authorization", s"Bearer $token")))
    assert(route.isDefinedAt(ok))
    assertEquals(Async.run[Response, Pure](route(ok)).runWith.status, 200)

    // and the two refusals the readme claims
    val none = Request.get("/api/data")
    assertEquals(Async.run[Response, Pure](route(none)).runWith.status, 401)
    val thin = Jwt.sign(Claims(subject = Some("ann"), audience = Vector("api"),
      expires = Some(now + 600), scopes = Set("write")), Jwt.Key.Hmac(secret))
    assertEquals(Async.run[Response, Pure](route(
      Request.get("/api/data", Seq(("authorization", s"Bearer $thin"))))).runWith.status, 403)
  }

  test("security: narrowing authority without asking anybody") {
    val grant = Capability.issue(rootKey, "alice")
    val agent = grant.attenuate(Caveat.Scope("tool:search"))
      .attenuate(Caveat.Until(deadline))

    val now = System.currentTimeMillis()
    assert(agent.verify(rootKey, Capability.checking(now, Set("tool:search"))))
    assert(!agent.verify(rootKey, Capability.checking(now, Set("tool:delete"))))
  }

  // ── okay-agent/README.md ───────────────────────────────────────

  final case class SearchArgs(query: String, limit: Option[Int])
  given Schema[SearchArgs] = Schema.derived

  test("agent: a conversation with a tool in it") {
    val searchSpec = ToolSpec[SearchArgs]("search", "search the corpus")

    val tools = Handlers.tools(Map("search" -> { (c: ToolCall) =>
      ToolSpec.args[SearchArgs](c).fold(e => s"bad args: $e",
        a => s"${a.limit.getOrElse(10)} hits for '${a.query}'")
    }))
    val (_, ctx) = Handlers.context(Compact.all)
    val model = Handlers.scripted(Seq(
      Reply("looking", Seq(ToolCall("c1", "search",
        Json.JObj(Vector("query" -> Json.JStr("okay")))))),
      Reply("found them", Nil)))

    assertEquals(run(Agent.converse("find okay", Seq(searchSpec)))(model, tools, ctx),
      "found them")
  }

  /** the readme's `run`: one handler per effect, unioned along the row */
  def run[A](prog: A ! Agent)(model: okay.Handler[Model], tool: okay.Handler[Tool],
                              ctx: okay.Handler[Context]): A =
    given okay.Handler[Model] = model
    given okay.Handler[Tool] = tool
    given okay.Handler[Context] = ctx
    given rowCA: okay.Handler[Context + Async] = okay.Handler.union[Context, Async]
    given rowTCA: okay.Handler[Tool + (Context + Async)] = okay.Handler.union[Tool, Context + Async]
    given rowAll: okay.Handler[Agent] = okay.Handler.union[Model, Tool + (Context + Async)]
    prog.runWith

  // ── okay-http/README.md ────────────────────────────────────────

  test("http: a server and a client") {
    val route: Request => Response ! Async = {
      case r if Server.path(r) == "/hello" => Server.text(200, "hello")
      case _ => Server.notFound
    }
    val client = Transports.http()

    val answer = okay.Resource.run[String, Pure](Server.serve(0)(route).map { s =>
      Async.run[String, Pure](
        client.send(Request.get(s"http://127.0.0.1:${Server.port(s)}/hello"))
          .flatMap(Http.text)).runWith
    }).runWith
    assertEquals(answer, "hello")
  }
}
