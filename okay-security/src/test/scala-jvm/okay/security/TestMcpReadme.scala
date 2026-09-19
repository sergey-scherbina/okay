package okay.security

import okay.*
import okay.given
import okay.agent.{Handlers, Tool, ToolCall, ToolSpec}
import okay.codec.{Json, Schema}
import okay.http.McpHttp
import okay.mcp.{Mcp, Rpc, Server as McpServer}

/**
 * okay-mcp/README.md, COMPILED.
 *
 * A readme whose examples do not compile is worse than none, and the
 * only check that cannot be forgotten is one the build runs. Every
 * shape below is lifted from that file; when an example there
 * changes, change it here and let the compiler agree.
 *
 * It lives in okay-security rather than okay-mcp because half the
 * readme is the access control, and the arrow points this way:
 * okay-security sees okay-mcp, never the reverse.
 */
class TestMcpReadme extends munit.FunSuite {

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val specs = Seq(ToolSpec[Add]("add", "add two numbers"),
    ToolSpec[Add]("search", "find something"),
    ToolSpec[Add]("read", "read something"))

  val table: Map[String, ToolCall => String] = Map(
    "add" -> { (c: ToolCall) =>
      ToolSpec.args[Add](c).fold(e => s"bad args: $e", x => (x.a + x.b).toString) },
    "search" -> (_ => "found"), "read" -> (_ => "read"))

  val serving = McpServer.Serving(Mcp.Info("adder", "1.0"),
    tools = specs, call = table)

  val rootKey: Array[Byte] = Array.tabulate(32)(i => (i * 7 + 3).toByte)
  val metadataUrl = "https://rs.test" + McpAuth.WellKnown

  test("serving: the tool table and the args reader are what the readme says") {
    assertEquals(table("add")(ToolCall("c", "add",
      Rpc.obj("a" -> Json.JNum(20), "b" -> Json.JNum(22)))), "42")
    // a server is a VALUE; the route over it needs nothing but a scheduler
    val _ : okay.http.Request => okay.http.Response ! Async = McpHttp.route(serving)
  }

  test("the handler swap: a local table and a session are the same Handler[Tool]") {
    val local: Handler[Tool] = Handlers.tools(table)
    // `session.handler` is the other one; a session needs a live link,
    // so what is pinned here is that the local side has the type the
    // readme claims and a program cannot tell them apart
    assert(local != null)
  }

  test("narrow the server: only() keeps the list and the table together") {
    val forThisCaller = serving.only(Set("search", "read"))
    assertEquals(forThisCaller.tools.map(_.name), Seq("search", "read"))
    assertEquals(forThisCaller.call.keySet, Set("search", "read"))
  }

  test("narrow the conversation: the policy shape compiles and decides") {
    val policy: Policy = (who, _, tool) =>
      if who.claims.scopes.contains("tool:" + tool) then Decision.Permit
      else Decision.Deny("not for this caller")

    val mine = Principal("a", "a", Claims(scopes = Set("tool:search")))
    assertEquals(policy(mine, Mcp.ToolsCall, "search"), Decision.Permit)
    assertEquals(policy(mine, Mcp.ToolsCall, "add"), Decision.Deny("not for this caller"))

    val verify: String => Verified = _ => Verified.Ok(mine)
    val _ = McpAuth.tools(verify, metadataUrl, policy)(McpHttp.route(serving))
  }

  test("let the holder narrow it: the capability example is real") {
    val grant = Capability.issue(rootKey, "alice")
    val agent = grant.attenuate(Caveat.Scope("tool:search"))
      .attenuate(Caveat.Agent("crawler-7"))

    val now = System.currentTimeMillis()
    assert(agent.verify(rootKey, Capability.checking(now, Set("tool:search"))))
    assert(!agent.verify(rootKey, Capability.checking(now, Set("tool:add"))))
    val _ = McpAuth.capabilities(rootKey, metadataUrl)(McpHttp.route(serving))
  }

  test("plug in a policy from outside: the Revocations example is real") {
    val now = () => System.currentTimeMillis()
    val list = Revocations(freshFor = 60_000, whileStale = Revocations.Stale.Allow)
    val source: Revocations.Source = () => pure(Right(Set("crawler-7")))

    Async.run[Unit, Pure](list.refresh(source)(now)).runWith
    assert(list.revoked(now)("crawler-7"))

    val _ = McpAuth.capabilities(rootKey, metadataUrl,
      revoked = list.revoked(now))(McpHttp.route(serving))
  }
}
