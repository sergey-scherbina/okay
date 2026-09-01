package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, Turn}
import okay.codec.Json

/**
 * The one thing every test so far could not do: talk to a server
 * nobody here wrote.
 *
 * This runs against the protocol's own reference implementation
 * (`@modelcontextprotocol/server-everything`, spawned by npx over
 * stdio) and is SKIPPED where node is absent — the same bargain
 * okay-agent's TestLive makes with a model.
 *
 *   OKAY_MCP_SERVER  the command to spawn (default: the reference one)
 *
 * What is under test is not their server's behaviour but OUR
 * assumptions: that the handshake we send is accepted, that a real
 * tools/list decodes into `ToolSpec`s, that a real call answers text
 * our `Tool` handler can return, that real resources become documents
 * and real prompts become turns.
 */
class TestLive extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(180, "s")

  val command: Seq[String] =
    sys.env.get("OKAY_MCP_SERVER").map(_.split(" ").toSeq)
      .getOrElse(Seq("npx", "-y", "@modelcontextprotocol/server-everything"))

  lazy val available: Boolean =
    try
      val p = ProcessBuilder("which", command.head).start()
      p.waitFor() == 0
    catch case _: Throwable => false

  /** a spawned server, its session, and the cleanup */
  def live[A](body: Session => A): A =
    val process = Stdio.spawn(command)
    try body(Client.connect(Stdio.of(process), Mcp.Info("okay-mcp", "0.1")).runWith)
    finally process.destroy()

  test("live: the handshake is accepted by a server nobody here wrote") {
    assume(available, s"'${command.head}' is not on the PATH")
    live { s =>
      assert(s.server.isDefined, "no serverInfo came back")
      // the reference server serves all three, and says so
      assert(s.has("tools"), "no tools capability")
      assert(s.has("resources"), "no resources capability")
      assert(s.has("prompts"), "no prompts capability")
      println(s"  live: connected to ${s.server.map(i => s"${i.name} ${i.version}").get}")
    }
  }

  test("live: a real tools/list decodes, and a real call answers") {
    assume(available, s"'${command.head}' is not on the PATH")
    live { s =>
      val tools = s.tools.runWith
      assert(tools.nonEmpty, "no tools")
      println(s"  live: ${tools.length} tools — ${tools.map(_.name).take(6).mkString(", ")}")
      // every declaration carries a schema we can read
      assert(tools.forall(t => Rpc.field(t.schema, "type").isDefined),
        tools.find(t => Rpc.field(t.schema, "type").isEmpty).toString)

      val echo = tools.find(_.name == "echo")
      assume(echo.isDefined, "this server has no 'echo' tool")
      val answer = s.call(ToolCall("c1", "echo",
        Rpc.obj("message" -> Json.JStr("okay")))).runWith
      assert(answer.contains("okay"), answer)
      println(s"  live: echo answered '${answer.take(60)}'")
    }
  }

  test("live: real resources are documents, real prompts are turns") {
    assume(available, s"'${command.head}' is not on the PATH")
    live { s =>
      val corpus = s.corpus.runWith
      assert(corpus.sources.nonEmpty, "no resources read")
      println(s"  live: ${corpus.sources.size} resources as documents")

      val prompts = s.prompts.runWith
      assert(prompts.nonEmpty, "no prompts")
      val p = prompts.head
      val args = p.arguments.filter(_.required).map(a => (a.name, "1")).toMap
      val turns = s.prompt(p.name, args).runWith
      assert(turns.nonEmpty, s"prompt '${p.name}' produced no turns")
      assert(turns.forall {
        case Turn.User(t) => t.nonEmpty
        case Turn.Assistant(t, _) => t.nonEmpty
        case _ => true
      }, turns.toString)
      println(s"  live: prompt '${p.name}' -> ${turns.length} turns")
    }
  }

  test("live: the reference server completes a prompt argument for our client") {
    assume(available, s"'${command.head}' is not on the PATH")
    live { s =>
      assume(s.has("completions"), "this server does not declare completions")
      val prompts = s.prompts.runWith
      val withArg = prompts.find(_.arguments.nonEmpty)
      assume(withArg.isDefined, "no prompt with arguments to complete")
      val p = withArg.get
      // the CONTENT is theirs; the SHAPE is ours to assert — the call
      // round-trips and answers a vector, empty or not, with no error
      val values = s.complete(Mcp.Ref.Prompt(p.name), p.arguments.head.name, "").runWith
      println(s"  live: complete('${p.name}', '${p.arguments.head.name}') -> " +
        s"${values.length} values ${values.take(3).mkString("[", ", ", ", ...]")}")
      assert(values.length <= 100)
    }
  }

  test("live: the reference server lists its resource templates") {
    assume(available, s"'${command.head}' is not on the PATH")
    live { s =>
      val ts = s.templates.runWith
      println(s"  live: ${ts.length} templates — ${ts.map(_.uriTemplate).take(3).mkString(", ")}")
      assert(ts.forall(_.uriTemplate.nonEmpty))
    }
  }

  test("live: a notification sent BEFORE the handshake answer is not lost") {
    assume(available, s"'${command.head}' is not on the PATH")
    // the reference server announces notifications/tools/list_changed
    // ahead of its own initialize result — which is exactly the case a
    // request/answer loop with a filter on it would drop
    live { s =>
      assert(s.server.isDefined)          // the answer was still correlated
      val n = s.notifications.receive()   // and the notification was kept
      assert(n.isDefined, "the early notification was dropped")
      println(s"  live: early notification '${n.get.method}' arrived")
    }
  }
}
