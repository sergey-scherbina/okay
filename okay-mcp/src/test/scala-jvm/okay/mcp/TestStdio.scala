package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, ToolSpec}
import okay.codec.{Json, Schema}

import java.io.{PipedInputStream, PipedOutputStream}

/**
 * The transport itself, over a pair of real byte pipes: the same
 * client and server as everywhere else, but the lines now go through
 * `readLine` and `println` — which is exactly what a spawned server
 * gets, minus the process.
 */
class TestStdio extends munit.FunSuite {

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  test("a session over real byte streams") {
    // two pipes, crossed: what one end writes the other reads
    val clientOut = PipedOutputStream()
    val serverIn = PipedInputStream(clientOut)
    val serverOut = PipedOutputStream()
    val clientIn = PipedInputStream(serverOut)

    val spec = ToolSpec[Add]("add", "add two numbers")
    val table = Map[String, ToolCall => String]("add" -> { c =>
      ToolSpec.args[Add](c).fold(e => s"bad args: $e", x => (x.a + x.b).toString)
    })

    val fiber = Async.spawn(Server.run(Stdio.of(serverIn, serverOut),
      Mcp.Info("okay-mcp", "0.1"), Seq(spec), table))

    val session = Client.connect(Stdio.of(clientIn, clientOut),
      Mcp.Info("test", "1")).runWith
    assertEquals(session.server.map(_.name), Some("okay-mcp"))
    assertEquals(session.tools.runWith.map(_.name), Seq("add"))
    assertEquals(session.call(ToolCall("c1", "add", Json.JObj(Vector(
      "a" -> Json.JNum(2), "b" -> Json.JNum(40))))).runWith, "42")

    fiber.cancel()
  }
}
