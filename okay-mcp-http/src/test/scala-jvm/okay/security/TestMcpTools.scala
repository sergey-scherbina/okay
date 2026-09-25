package okay.security

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.http.{Body, Http, McpHttp, Request, Response}
import okay.mcp.{Mcp, Rpc, Server as McpServer}
import okay.agent.ToolSpec

/**
 * AUTHORIZATION REACHES THE TOOL (specs/security.md stage 7).
 *
 * `protect` can only say "this caller may POST /mcp", so the tests
 * that matter here are the ones that must be REFUSED while the rest
 * of the same server keeps working — and one that proves the refusal
 * happens before the table runs, because a check that answers "no"
 * after the write has happened is not a check.
 *
 * No port is bound: a route is a function, so the whole door is
 * exercised by calling it with a Request. That is also why this suite
 * is in the default gate while TestMcpAuth (which binds) is not.
 */
class TestMcpTools extends munit.FunSuite {

  private val secret = "the-resource-servers-hmac-secret".getBytes("UTF-8")
  private val nowSec = System.currentTimeMillis() / 1000
  private val nowMs = 1_700_000_000_000L
  private val root: Array[Byte] = Array.tabulate(32)(i => (i * 7 + 3).toByte)
  private val Meta = "http://rs.test/.well-known/oauth-protected-resource"

  final case class Arg(x: Int = 0)
  given Schema[Arg] = Schema.derived

  /** three tools and a ledger of what actually EXECUTED — the only
   * way to tell a refusal from a refusal-after-the-fact */
  final class Board:
    val ran = java.util.concurrent.ConcurrentHashMap[String, Integer]()
    private def tool(name: String) = name -> { (_: okay.agent.ToolCall) =>
      ran.merge(name, 1, (a, b) => a + b): Unit
      s"$name ok"
    }
    val serving: McpServer.Serving = McpServer.Serving(Mcp.Info("guarded", "0.1"),
      tools = Seq(ToolSpec[Arg]("read", "read something"),
        ToolSpec[Arg]("write", "write something"),
        ToolSpec[Arg]("admin", "do something drastic")),
      call = Map(tool("read"), tool("write"), tool("admin")))
    def count(name: String): Int = Option(ran.get(name)).map(_.intValue).getOrElse(0)

  // ── the doors under test ───────────────────────────────────────

  private def verify(t: String): Verified =
    Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), None, nowSec)

  private def tokenFor(scopes: Set[String]): String =
    Jwt.sign(Claims(subject = Some("agent"), expires = Some(nowSec + 600),
      scopes = scopes), Jwt.Key.Hmac(secret))

  /** the policy door, over a serving of the caller's choosing */
  private def guarded(board: Board, allow: String => Boolean,
                      serving: Option[McpServer.Serving] = None)
  : Request => Response ! Async =
    val s = serving.getOrElse(board.serving)
    McpAuth.tools(verify, Meta,
      (_, _, resource) => if allow(resource) then Decision.Permit
                          else Decision.Deny("not for you"))(McpHttp.route(s))

  /** the capability door, with the caller's list of what is off */
  private def byCapability(board: Board,
                           revoked: String => Boolean = _ => false)
  : Request => Response ! Async =
    McpAuth.capabilities(root, Meta, () => nowMs,
      revoked = revoked)(McpHttp.route(board.serving))

  // ── talking to it ──────────────────────────────────────────────

  private def run[A](p: A ! Async): A = Async.run[A, Pure](p).runWith

  private def post(route: Request => Response ! Async, body: String,
                   headers: Seq[(String, String)]): (Int, Seq[(String, String)], String) =
    run(route(Request.post("/mcp", Body.Text(body),
      headers :+ ("content-type", "application/json")))
      .flatMap(r => Http.text(r).map(t => (r.status, r.headers, t))))

  private def bearer(token: String) = Seq(("authorization", s"Bearer $token"))

  /** initialize, and the session id the server minted for it */
  private def session(route: Request => Response ! Async,
                      creds: Seq[(String, String)]): Seq[(String, String)] =
    val (st, headers, _) = post(route,
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}""", creds)
    assertEquals(st, 200)
    creds ++ headers.collect {
      case (k, v) if k.equalsIgnoreCase(McpHttp.SessionHeader) => (McpHttp.SessionHeader, v)
    }

  private def listed(route: Request => Response ! Async,
                     creds: Seq[(String, String)]): Seq[String] =
    val (_, _, body) = post(route,
      """{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}""", creds)
    Rpc.decode(body) match
      case Rpc.Answer(_, Json.JObj(fs)) => fs.collectFirst {
        case ("tools", Json.JArr(items)) => items.flatMap {
          case Json.JObj(t) => t.collectFirst { case ("name", Json.JStr(n)) => n }
          case _ => None
        }
      }.getOrElse(Nil)
      case _ => fail(s"not a tools answer: $body")

  private def called(route: Request => Response ! Async,
                     creds: Seq[(String, String)], name: String): String =
    val (_, _, body) = post(route,
      s"""{"jsonrpc":"2.0","id":3,"method":"tools/call",
           "params":{"name":"$name","arguments":{"x":1}}}""", creds)
    body

  // ── the narrowing itself, with no transport at all ─────────────

  test("Serving.only narrows the list and the table TOGETHER") {
    val b = Board()
    val narrowed = b.serving.only(Set("read", "write"))
    assertEquals(narrowed.tools.map(_.name), Seq("read", "write"))
    assertEquals(narrowed.call.keySet, Set("read", "write"))
    // the original is untouched: a Serving is a value
    assertEquals(b.serving.tools.length, 3)
  }

  test("a Serving narrowed to nothing declares NO tools") {
    val b = Board()
    val route = McpHttp.route(b.serving.only(_ => false))
    val creds = session(route, Nil)
    val (_, _, body) = post(route,
      """{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}""", creds)
    // MethodNotFound, because the handshake never advertised tools
    assert(body.contains(Rpc.MethodNotFound.toString), body)
  }

  // ── the policy door ────────────────────────────────────────────

  test("two of three tools: the list shows exactly what the policy permits") {
    val b = Board()
    val route = guarded(b, Set("read", "write"))
    assertEquals(listed(route, session(route, bearer(tokenFor(Set("mcp"))))),
      Seq("read", "write"))
  }

  test("the third answers 'no such tool' — and the table NEVER RUNS") {
    val b = Board()
    val route = guarded(b, Set("read"))
    val creds = session(route, bearer(tokenFor(Set("mcp"))))

    val refused = called(route, creds, "admin")
    assert(refused.contains("no such tool 'admin'"), refused)
    assert(refused.contains("isError"), refused)
    assertEquals(b.count("admin"), 0, "a refusal after the fact is not a refusal")

    // …and the gate is not simply breaking everything
    assert(called(route, creds, "read").contains("read ok"))
    assertEquals(b.count("read"), 1)
  }

  test("a refused tool is INDISTINGUISHABLE from a misspelled one") {
    val b = Board()
    val route = guarded(b, Set("read"))
    val creds = session(route, bearer(tokenFor(Set("mcp"))))
    assertEquals(
      called(route, creds, "admin").replace("admin", "X"),
      called(route, creds, "nosuchthing").replace("nosuchthing", "X"))
  }

  test("THE GATE IS PER REQUEST: a permission withdrawn between two calls") {
    val b = Board()
    var allowed = Set("write")
    val route = McpAuth.tools(verify, Meta,
      (_, _, resource) => if allowed(resource) then Decision.Permit
                          else Decision.Deny("withdrawn"))(McpHttp.route(b.serving))
    val creds = session(route, bearer(tokenFor(Set("mcp"))))

    assert(called(route, creds, "write").contains("write ok"))
    allowed = Set.empty
    // the SAME session, the same token, the next call
    assert(called(route, creds, "write").contains("no such tool 'write'"))
    assertEquals(b.count("write"), 1)
    assertEquals(listed(route, creds), Nil)
  }

  test("no bearer is the challenge that teaches, not a bare 401") {
    val b = Board()
    val (st, headers, _) = post(guarded(b, _ => true),
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}""", Nil)
    assertEquals(st, 401)
    val ch = headers.collectFirst {
      case (k, v) if k.equalsIgnoreCase("www-authenticate") => v }.getOrElse("")
    assert(ch.contains(s"""resource_metadata="$Meta""""), ch)
  }

  // ── the protocol's own branches still belong to the protocol ───

  test("tools/list before initialize is still InvalidRequest THROUGH the gate") {
    val b = Board()
    val (_, _, body) = post(guarded(b, _ => true),
      """{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}""",
      bearer(tokenFor(Set("mcp"))))
    assert(body.contains(Rpc.InvalidRequest.toString), body)
  }

  test("a server with no tools still answers MethodNotFound through the gate") {
    val b = Board()
    val route = guarded(b, _ => true, serving = Some(b.serving.only(_ => false)))
    val creds = session(route, bearer(tokenFor(Set("mcp"))))
    val (_, _, body) = post(route,
      """{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}""", creds)
    assert(body.contains(Rpc.MethodNotFound.toString), body)
    // and the call side of the same capability, which was the dead
    // branch: "no such tool" is a polite empty list by another name
    assert(called(route, creds, "read").contains(Rpc.MethodNotFound.toString))
  }

  test("the SSE GET passes through untouched") {
    val b = Board()
    val route = guarded(b, _ => true)
    // an unknown session: McpHttp's own 404, reaching us unrewritten —
    // the gate must not read a body it is meant to stream
    val r = run(route(Request.get("/mcp",
      bearer(tokenFor(Set("mcp"))) :+ (McpHttp.SessionHeader, "nobody"))))
    assertEquals(r.status, 404)
  }

  // ── the capability door: the holder's own question ─────────────

  test("a root capability sees every tool; attenuated, it sees one") {
    val b = Board()
    val route = byCapability(b)
    val whole = Capability.issue(root, "alice")
    assertEquals(listed(route, session(route, bearer(whole.encoded))).toSet,
      Set("read", "write", "admin"))

    // the holder narrows with NO key and NO issuer — the point
    val narrowed = whole.attenuate(Caveat.Scope("tool:read"))
    val creds = session(route, bearer(narrowed.encoded))
    assertEquals(listed(route, creds), Seq("read"))
    assert(called(route, creds, "read").contains("read ok"))
    assert(called(route, creds, "write").contains("no such tool 'write'"))
    assertEquals(b.count("write"), 0)
  }

  test("an expired capability is refused for everything, by the verifier's clock") {
    val b = Board()
    val route = byCapability(b)
    val stale = Capability.issue(root, "alice").attenuate(Caveat.Until(nowMs - 1))
    val (st, _, _) = post(route,
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}""",
      bearer(stale.encoded))
    assertEquals(st, 401)
  }

  test("A CAVEAT REMOVED FAILS THE CHAIN: widening is not narrowing") {
    val b = Board()
    val route = byCapability(b)
    val narrowed = Capability.issue(root, "alice").attenuate(Caveat.Scope("tool:read"))
    val widened = narrowed.copy(caveats = Vector.empty)   // keeping the tag
    val (st, _, _) = post(route,
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}""",
      bearer(widened.encoded))
    assertEquals(st, 401)
  }

  test("a caveat kind this verifier cannot enforce refuses, never ignores") {
    val b = Board()
    val route = byCapability(b)
    val odd = Capability.issue(root, "alice").attenuate("weekdays-only")
    val (st, _, _) = post(route,
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}""",
      bearer(odd.encoded))
    assertEquals(st, 401)
  }

  // ── hostile input answers, it does not throw ───────────────────

  test("a damaged body and a nameless call are ANSWERS") {
    val b = Board()
    val route = guarded(b, _ => true)
    val creds = session(route, bearer(tokenFor(Set("mcp"))))

    // the gate hands a line it cannot read to the protocol, which owes
    // an error with a null id — and the NEXT call must get its OWN
    // answer, not this one (okay-http/BUGS.md
    // mcp-unowed-answer-crosses-requests, found by this test)
    val damaged = post(route, "{{{not json at all", creds)._3
    assert(damaged.contains(Rpc.InvalidRequest.toString), damaged)

    val nameless = post(route,
      """{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{}}""", creds)._3
    assert(nameless.contains(Rpc.InvalidParams.toString), nameless)
    assertEquals(b.count("read") + b.count("write") + b.count("admin"), 0)
  }

  test("a bearer that is not a capability at all is a 401, not a crash") {
    val b = Board()
    val (st, _, _) = post(byCapability(b),
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}""",
      bearer("okc1.nonsense"))
    assertEquals(st, 401)
  }

  test("a revoked agent is refused — and so is everything attenuated FROM it") {
    val b = Board()
    val leaf = Capability.issue(root, "alice").attenuate(Caveat.Agent("a1"))
    // the holder narrows again, freely, with nobody's permission
    val child = leaf.attenuate(Caveat.Scope("tool:read"))
    val route = byCapability(b, revoked = _ == "a1")

    for cap <- Seq(leaf, child) do
      val (st, _, _) = post(route,
        """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}""",
        bearer(cap.encoded))
      assertEquals(st, 401, "a revoked branch cannot shed the caveat")

    // an unrelated branch of the SAME root still works
    val other = Capability.issue(root, "alice").attenuate(Caveat.Agent("a2"))
    val creds = session(route, bearer(other.encoded))
    assertEquals(listed(route, creds).toSet, Set("read", "write", "admin"))
  }

  test("revoking the root id voids every branch of that grant") {
    val b = Board()
    val whole = Capability.issue(root, "alice")
    val route = byCapability(b, revoked = _ == whole.id)
    for cap <- Seq(whole, whole.attenuate(Caveat.Agent("a1")),
                   whole.attenuate(Caveat.Agent("a1")).attenuate(Caveat.Scope("tool:read"))) do
      val (st, _, _) = post(route,
        """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}""",
        bearer(cap.encoded))
      assertEquals(st, 401)
  }

  test("an agent caveat costs nothing while nothing is revoked") {
    val b = Board()
    val route = byCapability(b)
    val cap = Capability.issue(root, "alice")
      .attenuate(Caveat.Agent("a1")).attenuate(Caveat.Scope("tool:read"))
    val creds = session(route, bearer(cap.encoded))
    assertEquals(listed(route, creds), Seq("read"))
    assert(called(route, creds, "read").contains("read ok"))
  }
}
