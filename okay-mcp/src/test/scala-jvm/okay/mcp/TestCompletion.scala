package okay.mcp

import okay.*
import okay.given
import okay.agent.Turn
import okay.codec.Json

/** completion/complete: the server's completer is data in, values
 * out; the capability follows the function; the wire caps at 100. */
class TestCompletion extends munit.FunSuite {

  val info = Mcp.Info("okay-mcp", "0.1")

  val languages = Vector("scala", "scheme", "smalltalk", "swift")

  def serving = Server.Serving(info,
    prompts = Seq(Mcp.Prompt("explain", "explain code",
      Seq(Mcp.Prompt.Arg("language", required = true)))),
    prompt = (n, _) => Option.when(n == "explain")(Seq(Turn.User("..."))),
    complete = Some { c =>
      c.ref match
        case Mcp.Ref.Prompt("explain") if c.argument == "language" =>
          // the context narrows: a resolved "family" cuts the list
          val pool = c.context.get("family") match
            case Some("lisp") => Vector("scheme")
            case _ => languages
          pool.filter(_.startsWith(c.value))
        case Mcp.Ref.Resource(uri) => Vector(s"completed-for:$uri")
        case _ => Vector.empty
    })

  def talk(serving: Server.Serving)(msgs: Rpc*): Seq[Rpc] =
    !.run(Writer.run(through(Writer.of(msgs.toList))(Server.serve(serving))))._1

  val hello = Rpc.Request(Json.JNum(1), Mcp.Initialize, Json.JObj(Vector.empty))
  def ask(c: Mcp.Complete, id: Int = 2): Rpc =
    Rpc.Request(Json.JNum(id), Mcp.CompletionComplete, McpDocs.completeParams(c))

  test("a prompt-ref completion answers values, the context narrowing them") {
    val out = talk(serving)(hello,
      ask(Mcp.Complete(Mcp.Ref.Prompt("explain"), "language", "s")),
      ask(Mcp.Complete(Mcp.Ref.Prompt("explain"), "language", "s",
        context = Map("family" -> "lisp")), id = 3))
    val Rpc.Answer(_, r1) = out(1): @unchecked
    assertEquals(McpDocs.completionOf(r1), languages)
    val Rpc.Answer(_, r2) = out(2): @unchecked
    assertEquals(McpDocs.completionOf(r2), Vector("scheme"))
  }

  test("more than 100 values cap at 100 with hasMore") {
    val many = serving.copy(complete = Some(_ => Vector.tabulate(150)(i => s"v$i")))
    val Rpc.Answer(_, r) = talk(many)(hello,
      ask(Mcp.Complete(Mcp.Ref.Prompt("p"), "a", "")))(1): @unchecked
    assertEquals(McpDocs.completionOf(r).length, 100)
    assertEquals(Rpc.field(r, "completion").flatMap(Rpc.field(_, "hasMore")),
      Some(Json.JBool(true)))
    assertEquals(Rpc.field(r, "completion").flatMap(Rpc.field(_, "total")),
      Some(Json.JNum(150)))
  }

  test("no completer, no capability — and the method refuses") {
    val bare = Server.Serving(info, prompts = serving.prompts, prompt = serving.prompt)
    val out = talk(bare)(hello, ask(Mcp.Complete(Mcp.Ref.Prompt("explain"), "language", "s")))
    val Rpc.Answer(_, init) = out(0): @unchecked
    assert(!Mcp.capability(init, "completions"))
    assertEquals(out(1),
      Rpc.Failed(Json.JNum(2), Rpc.MethodNotFound, Mcp.CompletionComplete))
    // and WITH one, the handshake says so
    val Rpc.Answer(_, init2) = talk(serving)(hello).head: @unchecked
    assert(Mcp.capability(init2, "completions"))
  }

  test("a resource ref passes its uri through; the client round-trips it") {
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = async(out.send(line))
      def lines: Source[String] = Writer.of(in)
    Async.spawn(Server.run(link(down, up), serving)): Unit
    val s = Client.connect(link(up, down), Mcp.Info("t", "1")).runWith
    assert(s.has("completions"))
    assertEquals(
      s.complete(Mcp.Ref.Resource("okay://files/{path}"), "path", "R").runWith,
      Vector("completed-for:okay://files/{path}"))
    assertEquals(
      s.complete(Mcp.Ref.Prompt("explain"), "language", "sc").runWith,
      Vector("scala", "scheme"))
  }
}
