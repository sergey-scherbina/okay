package okay.http

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.mcp.{Client, Duplex, Mcp, Rpc, Server as McpServer}
import okay.agent.{ToolCall, ToolSpec}

/**
 * MCP over streamable HTTP, against a real server on a real port —
 * and the claim under test is that nothing above the transport
 * noticed: the same `Serving` and the same session code as stdio.
 */
class TestMcpHttp extends munit.FunSuite {

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val info = Mcp.Info("okay-http-mcp", "0.1")
  val spec = ToolSpec[Add]("add", "add two numbers")
  val table = Map[String, ToolCall => String]("add" -> { c =>
    ToolSpec.args[Add](c).fold(e => s"bad args: $e", x => (x.a + x.b).toString)
  })
  val serving = McpServer.Serving(info, tools = Seq(spec), call = table)

  val http = Transports.http()

  /** the posted body, however the server handed it over (it is Bytes) */
  def text(r: Request): String = r.body match
    case Body.Text(s) => s
    case Body.Bytes(b) => String(b.toArray, java.nio.charset.StandardCharsets.UTF_8)
    case Body.Empty => ""

  /** a server on a free port, for the body of the test */
  def served[A](route: Request => Response ! Async)(body: String => A): A =
    Resource.run[A, Pure](Server.serve(0)(route).map(s =>
      body(s"http://127.0.0.1:${Server.port(s)}/mcp"))).runWith

  test("the whole session over HTTP: handshake, list, call") {
    served(McpHttp.route(serving)) { url =>
      val link = McpHttp.link(http, url)
      val s = Client.connect(link, Mcp.Info("test", "1")).runWith
      assertEquals(s.server, Some(info))
      assertEquals(s.tools.runWith.map(_.name), Seq("add"))
      assertEquals(s.call(ToolCall("c1", "add", Json.JObj(Vector(
        "a" -> Json.JNum(20), "b" -> Json.JNum(22))))).runWith, "42")
      // the server issued a session, and the link carries it
      assert(link.sessionId.isDefined, "no Mcp-Session-Id was issued")
    }
  }

  test("an unknown session id answers 404 — the reinitialize signal") {
    served(McpHttp.route(serving)) { url =>
      val hello = Rpc.encode(Rpc.Request(Json.JNum(1), Mcp.Initialize,
        Mcp.initializeParams(Mcp.Info("c", "1"))))
      // initialize first, so the server has a session that is not ours
      val _ = http.send(Request.post(url, Body.Text(hello),
        Seq(("content-type", "application/json")))).runWith

      val r = http.send(Request.post(url,
        Body.Text(Rpc.encode(Rpc.Request(Json.JNum(2), Mcp.ToolsList, Rpc.obj()))),
        Seq(("content-type", "application/json"),
          (McpHttp.SessionHeader, "not-a-session")))).runWith
      assertEquals(r.status, 404)
    }
  }

  test("a posted NOTIFICATION answers 202 with no body") {
    served(McpHttp.route(serving)) { url =>
      val hello = Rpc.encode(Rpc.Request(Json.JNum(1), Mcp.Initialize,
        Mcp.initializeParams(Mcp.Info("c", "1"))))
      val opened = http.send(Request.post(url, Body.Text(hello),
        Seq(("content-type", "application/json")))).runWith
      val id = opened.header(McpHttp.SessionHeader).get

      val r = http.send(Request.post(url,
        Body.Text(Rpc.encode(Rpc.Notify(Mcp.Initialized, Rpc.obj()))),
        Seq(("content-type", "application/json"), (McpHttp.SessionHeader, id)))).runWith
      assertEquals(r.status, 202)
      assertEquals(Http.text(r).runWith, "")
    }
  }

  test("an event-stream answer is the same to the client as a json one") {
    // a route that answers the SSE way: the client must not be able
    // to tell, which is the whole reason the link drains both onto
    // one channel
    val sse: Request => Response ! Async = request =>
      val body = text(request)
      Rpc.decode(body) match
        case Rpc.Request(id, Mcp.Initialize, _) =>
          val msg = Rpc.encode(Rpc.Answer(id, Mcp.initializeResult(info)))
          pure(Response(200, Seq(("content-type", "text/event-stream")),
            Http.one(s"data: $msg\n\n".getBytes("UTF-8"))))
        case Rpc.Request(id, Mcp.ToolsList, _) =>
          val msg = Rpc.encode(Rpc.Answer(id, Mcp.toolsResult(Seq(spec))))
          pure(Response(200, Seq(("content-type", "text/event-stream")),
            Http.one(s"data: $msg\n\n".getBytes("UTF-8"))))
        case _ => pure(Response(202, Nil, Http.one(Array.empty)))

    served(sse) { url =>
      val s = Client.connect(McpHttp.link(http, url), Mcp.Info("test", "1")).runWith
      assertEquals(s.server, Some(info))
      assertEquals(s.tools.runWith.map(_.name), Seq("add"))
    }
  }

  test("the GET stream carries what the server says unasked") {
    val notice = Rpc.encode(Duplex.updated("okay://a"))
    val route: Request => Response ! Async = request =>
      if request.method == Method.Get then
        pure(Response(200, Seq(("content-type", "text/event-stream")),
          Http.one(s"data: $notice\n\n".getBytes("UTF-8"))))
      else
        val body = text(request)
        Rpc.decode(body) match
          case Rpc.Request(id, Mcp.Initialize, _) =>
            pure(Response(200, Seq(("content-type", "application/json")),
              Http.one(Rpc.encode(Rpc.Answer(id, Mcp.initializeResult(info)))
                .getBytes("UTF-8"))))
          case _ => pure(Response(202, Nil, Http.one(Array.empty)))

    served(route) { url =>
      val link = McpHttp.link(http, url)
      val s = Client.connect(link, Mcp.Info("test", "1")).runWith
      link.open(): Unit
      // notifications arrive on the session exactly as over stdio
      assertEquals(Duplex.updatedUri(s.notifications.receiveBlocking().get), Some("okay://a"))
    }
  }

  test("GET without a session is 404 — the stream belongs to a session") {
    // v6: GET serves the push stream now (see okay-jetty's TestMcpPush
    // for the streaming half); with no session id there is nothing to
    // stream, and 404 says whose fault that is
    served(McpHttp.route(serving)) { url =>
      assertEquals(http.send(Request.get(url)).runWith.status, 404)
    }
  }

  test("a server that answers 202 to a REQUEST does not hang the caller") {
    // the protocol forbids it; a client that waited for ever anyway
    // would be broken by any server that gets it wrong
    val rude: Request => Response ! Async = _ =>
      pure(Response(202, Nil, Http.one(Array.empty)))
    served(rude) { url =>
      // connect asks and waits; with the guard it is TOLD there is no
      // answer coming, so it finishes — knowing nothing about the
      // server, which is the truth
      val s = Client.connect(McpHttp.link(http, url), Mcp.Info("test", "1")).runWith
      assertEquals(s.server, None)
    }
  }

  test("one Serving, three wires: the same server over HTTP and in memory") {
    // in memory
    val up = Channel[String]()
    val down = Channel[String]()
    def mem(out: Channel[String], in: Channel[String]): okay.mcp.Link = new okay.mcp.Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    Async.spawn(McpServer.run(mem(down, up), serving)): Unit
    val direct = Client.connect(mem(up, down), Mcp.Info("test", "1")).runWith

    // over HTTP, the same Serving value
    served(McpHttp.route(serving)) { url =>
      val overHttp = Client.connect(McpHttp.link(http, url), Mcp.Info("test", "1")).runWith
      val call = ToolCall("c1", "add", Json.JObj(Vector(
        "a" -> Json.JNum(1), "b" -> Json.JNum(2))))
      assertEquals(overHttp.call(call).runWith, direct.call(call).runWith)
      assertEquals(overHttp.tools.runWith, direct.tools.runWith)
    }
  }
}
