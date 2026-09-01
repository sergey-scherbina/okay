package okay.security

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.http.{Body, Http, McpHttp, Method, Request, Response, Server, Transports}
import okay.mcp.{Client as McpClient, Mcp, Server as McpServer}
import okay.agent.{ToolCall, ToolSpec}

/**
 * The whole authorization loop around a real MCP server: a stranger
 * is challenged and told where to learn; discovery walks 401 →
 * resource metadata → AS metadata; a token is obtained by client
 * credentials from a stub AS; and the SAME agent call that works on
 * an open server works on the protected one — with nothing above the
 * link changed, which is the entire point of putting the bearer ON
 * the link.
 */
class TestMcpAuth extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  val now = System.currentTimeMillis() / 1000
  val secret = "the-resource-servers-hmac-secret".getBytes("UTF-8")
  val http = Transports.http()

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  def serving = McpServer.Serving(Mcp.Info("guarded", "0.1"),
    tools = Seq(ToolSpec[Add]("add", "add two numbers")),
    call = Map("add" -> { c =>
      ToolSpec.args[Add](c).fold(e => s"bad args: $e", x => (x.a + x.b).toString) }))

  /** the stub AS: token endpoint honouring client_credentials with a
   * scope-carrying JWT this resource server will accept */
  def stubAs(port: () => Int): PartialFunction[Request, Response ! Async] = {
    case r if r.url.startsWith("/.well-known/oauth-authorization-server") =>
      val base = s"http://127.0.0.1:${port()}"
      pure(Response(200, Seq(("content-type", "application/json")), Http.one(
        s"""{"issuer":"$base","authorization_endpoint":"$base/authorize",
            "token_endpoint":"$base/token"}""".getBytes("UTF-8"))))
    case r if r.url.startsWith("/token") && r.method == Method.Post =>
      val form = OAuth2.form(r.body match
        case Body.Bytes(b) => String(b.toArray, "UTF-8")
        case Body.Text(t) => t
        case _ => "")
      if form.get("grant_type") != Some("client_credentials")
         || form.get("client_id") != Some("robot") then
        pure(Response(400, Nil, Http.one("""{"error":"invalid_client"}""".getBytes)))
      else
        val token = Jwt.sign(Claims(subject = Some("robot"),
          audience = Vector("mcp"), expires = Some(now + 600),
          scopes = form.get("scope").map(_.split(' ').toSet).getOrElse(Set("mcp"))),
          Jwt.Key.Hmac(secret))
        pure(Response(200, Seq(("content-type", "application/json")),
          Http.one(s"""{"access_token":"$token","expires_in":600}""".getBytes("UTF-8"))))
  }

  def verify(t: String): Verified =
    Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("mcp"), now)

  /** one server carrying everything: the stub AS, the RFC 9728 doc,
   * and the protected MCP route */
  def world[A](policy: Policy = Policy.allowAll)(body: (Int) => A): A =
    var port0 = 0
    val (mcpRoute, _) = McpHttp.routed(serving)
    val route: PartialFunction[Request, Response ! Async] =
      stubAs(() => port0)
        .orElse(McpAuth.metadata("mcp", Seq(s"http://127.0.0.1:PORT")))
        .orElse { case r =>
          McpAuth.protect(verify, s"http://127.0.0.1:$port0${McpAuth.WellKnown}",
            policy)(mcpRoute)(r) }
    // the metadata document needs its own port inside — re-route with
    // a late-bound base by patching the doc route after bind
    Resource.run[A, Pure](Server.serve(0)({
      case r if r.url.startsWith(McpAuth.WellKnown) =>
        McpAuth.metadata("mcp", Seq(s"http://127.0.0.1:$port0"))(r)
      case r if route.isDefinedAt(r) => route(r)
    }).map { s =>
      port0 = Server.port(s)
      body(port0)
    }).runWith

  def run[A](p: A ! Async): A = Async.run[A, Pure](p).runWith

  test("a stranger is challenged, and the challenge teaches: 401 -> metadata -> AS") {
    world() { port =>
      val bare = run(http.send(Request.post(s"http://127.0.0.1:$port/mcp",
        Body.Text("{}"), Seq(("content-type", "application/json")))))
      assertEquals(bare.status, 401)
      val challenge = bare.header("www-authenticate").getOrElse("")
      assert(challenge.contains("resource_metadata="), challenge)

      val d = run(McpAuth.discover(http, s"http://127.0.0.1:$port/mcp"))
      d match
        case Right(found) =>
          assertEquals(found.resource, "mcp")
          assert(found.tokenEndpoint.endsWith("/token"), found.toString)
        case Left(e) => fail(e)
    }
  }

  test("the whole loop: connect by client credentials, then the SAME tool call works") {
    world() { port =>
      val link = run(McpAuth.connect(http, s"http://127.0.0.1:$port/mcp",
        "robot", None, Seq("mcp"))) match
        case Right(l) => l
        case Left(e) => fail(e)

      // nothing above the link knows authorization exists
      val session = McpClient.connect(link, Mcp.Info("t", "1")).runWith
      assertEquals(session.tools.runWith.map(_.name), Seq("add"))
      assertEquals(session.call(ToolCall("c", "add", Json.JObj(Vector(
        "a" -> Json.JNum(20), "b" -> Json.JNum(22))))).runWith, "42")
    }
  }

  test("a wrong client is the AS's refusal, surfaced; a wrong scope is the policy's 403") {
    world(policy = Policy.scoped("admin")) { port =>
      assertEquals(run(McpAuth.connect(http, s"http://127.0.0.1:$port/mcp",
        "not-robot", None, Nil)).left.map(_.contains("invalid_client")), Left(true))

      // a valid token without the scope the policy wants
      val link = run(McpAuth.connect(http, s"http://127.0.0.1:$port/mcp",
        "robot", None, Seq("mcp"))) match
        case Right(l) => l
        case Left(e) => fail(e)
      val r = run(http.send(Request.post(s"http://127.0.0.1:$port/mcp",
        Body.Text("{}"), Seq(("content-type", "application/json"),
          ("authorization", s"Bearer ${run(tokenFor(port, Set("mcp")))}")))))
      assertEquals(r.status, 403)
    }
  }

  test("the metadata documents are servable without any token") {
    world() { port =>
      assertEquals(run(http.send(Request.get(
        s"http://127.0.0.1:$port${McpAuth.WellKnown}"))).status, 200)
      assertEquals(run(http.send(Request.get(
        s"http://127.0.0.1:$port/.well-known/oauth-authorization-server"))).status, 200)
    }
  }

  private def tokenFor(port: Int, scopes: Set[String]): String ! Async =
    OAuth2.clientCredentials(http, OAuth2.Client("robot", None,
      s"http://127.0.0.1:$port/authorize", s"http://127.0.0.1:$port/token",
      "urn:ietf:wg:oauth:2.0:oob", scopes.toSeq)).map(_.toOption.get.access)
}
