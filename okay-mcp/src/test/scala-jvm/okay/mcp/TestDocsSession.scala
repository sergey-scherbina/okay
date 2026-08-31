package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, ToolSpec, Turn}
import okay.codec.{Json, Schema}
import okay.rag.Symbols

/**
 * The client half of v2, over a real wire — and the two bridges that
 * are the point of it: a server's resources become a CORPUS the
 * retriever indexes, and a server's prompt becomes the TURNS an agent
 * starts from.
 */
class TestDocsSession extends munit.FunSuite {

  val info = Mcp.Info("okay-mcp", "0.1")

  val files = Map(
    "okay://Greeter.scala" -> "object Greeter {\n  def greet(name: String) = s\"hi $name\"\n}\n",
    "okay://Adder.scala" -> "object Adder {\n  def add(a: Int, b: Int) = a + b\n}\n")

  val serving = Server.Serving(info,
    resources = files.keys.toSeq.sorted.map(u => Mcp.Resource(u, u.split('/').last,
      "a source file", Some("text/x-scala"))),
    read = files.get,
    prompts = Seq(Mcp.Prompt("explain", "explain a function",
      Seq(Mcp.Prompt.Arg("name", "the function", required = true)))),
    prompt = (n, args) => Option.when(n == "explain")(Seq(
      Turn.System("You explain code."),
      Turn.User(s"What does ${args.getOrElse("name", "?")} do?"))))

  def connected(serving: Server.Serving): Session =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = async(out.send(line))
      def lines: Source[String] = Writer.of(in)
    Async.spawn(Server.run(link(down, up), serving)): Unit
    Client.connect(link(up, down), Mcp.Info("test", "1")).runWith

  test("the handshake tells the client what to ask for") {
    val s = connected(serving)
    assert(s.has("resources"))
    assert(s.has("prompts"))
    assert(!s.has("tools"), "a server with no tools advertised them")
  }

  test("resources list and read across the wire") {
    val s = connected(serving)
    assertEquals(s.resources.runWith.map(_.name), Seq("Adder.scala", "Greeter.scala"))
    assertEquals(s.read("okay://Adder.scala").runWith, Some(files("okay://Adder.scala")))
    assertEquals(s.read("okay://nope").runWith, None)
  }

  test("a server's resources ARE a corpus, and the retriever indexes it") {
    val s = connected(serving)
    val corpus = s.corpus.runWith
    assertEquals(corpus.sources.keySet, files.keySet)

    // the retriever does not know or care that these came off a wire:
    // the same symbol index it builds for local files
    val index = Symbols.project(corpus.sources.values.toSeq)
    assert(index.names.contains("greet"), index.names.toString)
    assert(index.names.contains("add"), index.names.toString)
    val sym = index.definition("add").head
    assertEquals(sym.source, "okay://Adder.scala")
  }

  test("a server's prompt is the turns an agent starts from") {
    val s = connected(serving)
    assertEquals(s.prompts.runWith.map(_.name), Seq("explain"))
    assertEquals(s.prompt("explain", Map("name" -> "add")).runWith, Seq(
      Turn.User("You explain code."),
      Turn.User("What does add do?")))
    assertEquals(s.prompt("nope").runWith, Nil)
  }

  test("v1 and v2 interoperate: an older server simply has less") {
    final case class Add(a: Int, b: Int)
    given Schema[Add] = Schema.derived
    // the v1 entry point, unchanged, against the v2 client
    val s = connected(Server.Serving(info,
      tools = Seq(ToolSpec[Add]("add", "add two numbers")),
      call = Map("add" -> (_ => "42"))))
    assert(s.has("tools"))
    assert(!s.has("resources"))
    // and asking anyway is an answer, not a hang
    assertEquals(s.resources.runWith, Nil)
    assertEquals(s.prompt("anything").runWith, Nil)
    assertEquals(s.call(ToolCall("c", "add", Json.JObj(Vector.empty))).runWith, "42")
  }
}
