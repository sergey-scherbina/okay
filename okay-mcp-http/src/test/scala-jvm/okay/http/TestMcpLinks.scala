package okay.http

import okay.*
import okay.given
import okay.codec.Json
import okay.mcp.{Mcp, NioLink, Rpc, WsLink}

/**
 * okay-mcp over okay-http's wires: a WebSocket and a bare TCP socket are
 * each a `Link`, so the same protocol runs over them unchanged. Moved
 * out of TestWs and TestNio with the links themselves
 * (http-mcp-agent-edge, 2026-09-25), so okay-http does not depend on
 * okay-mcp.
 */
class TestMcpLinks extends munit.FunSuite {
  // both bind a real port: out of the default gate, as TestWs and
  // TestNio are (nio-port-scope); `sbt integrationTest` runs them
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))

  val sockets = Transports.sockets()

  test("okay-mcp runs over a WebSocket, with no protocol code changed") {
    // MCP's two standard transports are stdio and HTTP+SSE; okay-mcp had
    // the first. A socket IS a Link, so the SAME Stage[Rpc, Rpc, Unit]
    // that runs over pipes runs over a socket, and this is the proof.
    val echo = WsEcho()
    try
      // the echo server bounces our own lines back, so a client sending
      // an initialize sees exactly the bytes a Link carries — enough to
      // prove the Link is well formed over frames
      val sent = Rpc.encode(Rpc.Request(Json.JNum(1), Mcp.Initialize,
        Mcp.initializeParams(Mcp.Info("client", "1"))))

      val back = Async.run[Seq[String], Pure](
        sockets.connect(echo.url).flatMap { sock =>
          val link = WsLink(sock)
          link.send(sent).flatMap(_ =>
            Writer.uncons[String, Unit, Async](link.lines).flatMap {
              case Right((line, _)) => sock.close().map(_ => Seq(line))
              case Left(_) => sock.close().map(_ => Seq.empty[String])
            })
        }).runWith

      assertEquals(back, Seq(sent))
      // and it is a well-formed message on the way back, not just bytes
      assertEquals(Rpc.decode(back.head), Rpc.Request(Json.JNum(1), Mcp.Initialize,
        Mcp.initializeParams(Mcp.Info("client", "1"))))
    finally echo.close()
  }

  test("MCP over a raw socket — the third transport, no HTTP anywhere") {
    val sent = Rpc.encode(Rpc.Request(Json.JNum(1), Mcp.Initialize,
      Mcp.initializeParams(Mcp.Info("client", "1"))))

    val back = Resource.run[Seq[String], Pure](
      // the server end: read one line, send it back, close
      Nio.listen(0) { conn =>
        val link = NioLink(conn)
        Writer.uncons[String, Unit, Async](link.lines).flatMap {
          case Right((line, _)) => link.send(line).flatMap(_ => conn.close())
          case Left(_) => conn.close()
        }
      }.map { server =>
        Async.run[Seq[String], Pure](
          Nio.connect("127.0.0.1", Nio.port(server)).flatMap { c =>
            val link = NioLink(c)
            link.send(sent).flatMap(_ =>
              Writer.uncons[String, Unit, Async](link.lines).flatMap {
                case Right((l, _)) => c.close().map(_ => Seq(l))
                case Left(_) => c.close().map(_ => Seq.empty[String])
              })
          }).runWith
      }).runWith

    assertEquals(back, Seq(sent))
    // and it decodes back to the identical message, not just to bytes
    assertEquals(Rpc.decode(back.head), Rpc.Request(Json.JNum(1), Mcp.Initialize,
      Mcp.initializeParams(Mcp.Info("client", "1"))))
  }
}
