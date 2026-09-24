package scala2probe

import okay.codec.Schema
import okay.scala2._

object McpModel {
  final case class Add(a: Int, b: Int)
  object Add {
    implicit val schema: Schema[Add] = Schemas.product2("Add", "a", "b")(Add.apply)(x => (x.a, x.b))
  }
}

/** okay-mcp from Scala 2.13 (specs/scala2-facade.md, stage 15.5) */
class TestMcpFromScala2 extends munit.FunSuite {
  import McpModel._

  val tools = Tools.empty.on[Add]("add", "add two numbers")(x => (x.a + x.b).toString)

  test("a Scala 2 server and a Scala 2 client: the handshake, the tool list, a call") {
    val (serverEnd, clientEnd) = McpLink.pair()
    val prog = for {
      server <- Async.fork(McpServer.run(serverEnd, "calc", "1.0", tools))
      client <- McpClient.connect(clientEnd, "probe", "1")
      listed <- client.tools
      sum <- client.call("add", "{\"a\": 2, \"b\": 40}")
      _ <- server.cancel
    } yield (client.server, listed.map(_.name), listed.head.schema.contains("\"b\""), sum)
    assertEquals(prog.runWith, (Some(("calc", "1.0")), Seq("add"), true, "42"))
  }

  test("resources: listed by uri, read by uri, an absent one is None") {
    val (serverEnd, clientEnd) = McpLink.pair()
    val prog = for {
      server <- Async.fork(McpServer.run(serverEnd, "docs", "1.0", Tools.empty, Map("doc://readme" -> "hello")))
      client <- McpClient.connect(clientEnd, "probe", "1")
      listed <- client.resources
      text <- client.read("doc://readme")
      missing <- client.read("doc://nothing")
      _ <- server.cancel
    } yield (listed.map(_.uri), text, missing)
    assertEquals(prog.runWith, (Seq("doc://readme"), Some("hello"), None))
  }
}
