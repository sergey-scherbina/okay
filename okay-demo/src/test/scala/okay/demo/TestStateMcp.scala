package okay.demo

import okay.*
import okay.given
import okay.agent.ToolCall
import okay.codec.Json
import okay.mcp.{Client, Duplex, Link, Mcp, Server, Session}

import java.io.File
import java.nio.file.Files

/**
 * The state server over the real MCP protocol — initialize, list,
 * call — the same path Claude Code would drive, minus the pipes
 * (Server.serve is a pure Stage; TestDuplex's own `wire()` pattern).
 * Persistence is exercised across a real file and a fresh `Store`,
 * which is what surviving a restart (or a `/clear`) means here.
 */
class TestStateMcp extends munit.FunSuite {

  def wire(): (Link, Link) =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    (link(up, down), link(down, up))

  def connected(file: File): Session =
    val store = StateMcp.Store(file)
    val (client, server) = wire()
    Async.spawn(Server.run(server, Mcp.Info("okay-state", "0.1"),
      StateMcp.tools, StateMcp.handlers(store))): Unit
    Client.connect(client, Mcp.Info("test", "1"), Duplex.Peer()).runWith

  def call(s: Session, name: String, args: Json): String =
    s.call(ToolCall("1", name, args)).runWith

  def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)

  test("all three tools are declared, with the RFC 7396 contract in the description") {
    val dir = Files.createTempDirectory("state-mcp")
    val s = connected(dir.resolve("state.json").toFile)
    val declared = s.tools.runWith
    assertEquals(declared.map(_.name).toSet, Set("get_state", "update_state", "reset_state"))
    assert(declared.find(_.name == "update_state").get.description.contains("RFC 7396"))
  }

  test("get_state starts empty, update_state merges, a second field does not erase the first") {
    val dir = Files.createTempDirectory("state-mcp")
    val s = connected(dir.resolve("state.json").toFile)
    assertEquals(Json.parse(call(s, "get_state", obj())), obj())

    call(s, "update_state", obj("task" -> Json.JStr("pack-orders"))): Unit
    call(s, "update_state", obj("picked" -> Json.JNum(3))): Unit
    val got = Json.parse(call(s, "get_state", obj()))
    assertEquals(got, obj("task" -> Json.JStr("pack-orders"), "picked" -> Json.JNum(3)))
  }

  test("a null field deletes it; a repeated key is the last write, RFC 7396 through the wire") {
    val dir = Files.createTempDirectory("state-mcp")
    val s = connected(dir.resolve("state.json").toFile)
    call(s, "update_state", obj("a" -> Json.JNum(1), "b" -> Json.JNum(2))): Unit
    call(s, "update_state", obj("a" -> Json.JNull, "b" -> Json.JNum(9))): Unit
    assertEquals(Json.parse(call(s, "get_state", obj())), obj("b" -> Json.JNum(9)))
  }

  test("a non-object patch is refused, and Sigma is untouched") {
    val dir = Files.createTempDirectory("state-mcp")
    val s = connected(dir.resolve("state.json").toFile)
    call(s, "update_state", obj("kept" -> Json.JStr("yes"))): Unit
    val result = call(s, "update_state", Json.JArr(Vector(Json.JNum(1))))
    assert(result.startsWith("error:"), result)
    assertEquals(Json.parse(call(s, "get_state", obj())), obj("kept" -> Json.JStr("yes")))
  }

  test("reset_state clears it, and the clear is persisted") {
    val dir = Files.createTempDirectory("state-mcp")
    val file = dir.resolve("state.json").toFile
    val s = connected(file)
    call(s, "update_state", obj("x" -> Json.JNum(1))): Unit
    call(s, "reset_state", obj()): Unit
    assertEquals(Json.parse(call(s, "get_state", obj())), obj())
    assertEquals(Json.parse(Files.readString(file.toPath)), obj())
  }

  test("state survives a restart: a fresh Store over the same file sees the last write") {
    val dir = Files.createTempDirectory("state-mcp")
    val file = dir.resolve("state.json").toFile
    call(connected(file), "update_state", obj("survived" -> Json.JBool(true))): Unit

    // a NEW connection over the SAME file — no server process shared,
    // no in-memory state shared: only the file crosses this line
    val restarted = connected(file)
    assertEquals(Json.parse(call(restarted, "get_state", obj())), obj("survived" -> Json.JBool(true)))
  }

  test("a damaged state file starts empty rather than failing the server") {
    val dir = Files.createTempDirectory("state-mcp")
    val file = dir.resolve("state.json").toFile
    Files.writeString(file.toPath, "{not json")
    val s = connected(file)
    assertEquals(Json.parse(call(s, "get_state", obj())), obj())
  }
}
