package okay.cluster

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.ServerSocket
import java.util.concurrent.TimeUnit

/**
 * The cross-platform policy's acceptance test (specs/cluster.md): a
 * JS client — the linked Node bundle of Client.scala — drives this
 * JVM server; both compile the ONE shared-source Acceptance object.
 */
class TestAcceptance extends munit.FunSuite {

  test("a JS client drives this JVM server with the same shared-source program") {
    val clientJs = sys.props.get("okay.client.js").filter(java.io.File(_).exists)
    assume(clientJs.isDefined, "linked JS client not found (build wires fastLinkJS)")

    val server = ServerSocket(0)
    var chunks = 0
    val fold = Thread.ofVirtual().start { () =>
      val sock = server.accept()
      val in = BufferedReader(InputStreamReader(sock.getInputStream))
      var acc = Acceptance.agg.init
      var line = in.readLine()
      while line != null do
        okay.codec.Json.read[List[Double]](line).foreach { xs =>
          acc = xs.foldLeft(acc)(Acceptance.agg.add)
          chunks += 1
        }
        line = in.readLine()
      val out = PrintWriter(sock.getOutputStream, true)
      out.println(okay.codec.Json.write(Acceptance.agg.present(acc)))
      sock.close()
    }

    val node = ProcessBuilder("node", clientJs.get, server.getLocalPort.toString)
      .inheritIO().start()
    assert(node.waitFor(60, TimeUnit.SECONDS), "node client timed out")
    assertEquals(node.exitValue(), 0, "the JS client rejected the server's answer")
    fold.join()
    assertEquals(chunks, Acceptance.frames.length)   // every frame arrived
    server.close()
  }
}
