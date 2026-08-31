package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, ToolSpec}
import okay.codec.{Json, Schema}

import java.net.{InetAddress, ServerSocket, Socket}

/**
 * The transport itself, over a real socket: the same client and
 * server as everywhere else, but the lines now go through `readLine`
 * and `println` on a byte stream — which is what a spawned server's
 * pipes are, minus the process.
 *
 * A socket rather than a `PipedInputStream`, and that is not a
 * detail: a pipe remembers WHICH THREAD wrote to it last and declares
 * the write end dead when that thread is gone. A duplex session
 * writes from whichever fiber is answering, so the pipe's thread
 * affinity is a property no honest transport has — testing on one
 * would have been testing the wrong thing.
 */
class TestStdio extends munit.FunSuite {

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  test("a session over a real socket") {
    val server = ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    val accepted = java.util.concurrent.CompletableFuture[Socket]()
    val acceptor = Thread.startVirtualThread(() => accepted.complete(server.accept()): Unit)
    val client = Socket(InetAddress.getLoopbackAddress, server.getLocalPort)
    val remote = accepted.get()
    acceptor.join()

    val spec = ToolSpec[Add]("add", "add two numbers")
    val table = Map[String, ToolCall => String]("add" -> { c =>
      ToolSpec.args[Add](c).fold(e => s"bad args: $e", x => (x.a + x.b).toString)
    })

    val fiber = Async.spawn(Server.run(
      Stdio.of(remote.getInputStream, remote.getOutputStream),
      Mcp.Info("okay-mcp", "0.1"), Seq(spec), table))

    val session = Client.connect(
      Stdio.of(client.getInputStream, client.getOutputStream),
      Mcp.Info("test", "1")).runWith
    assertEquals(session.server.map(_.name), Some("okay-mcp"))
    assertEquals(session.tools.runWith.map(_.name), Seq("add"))
    assertEquals(session.call(ToolCall("c1", "add", Json.JObj(Vector(
      "a" -> Json.JNum(2), "b" -> Json.JNum(40))))).runWith, "42")

    fiber.cancel()
    client.close(); remote.close(); server.close()
  }
}
