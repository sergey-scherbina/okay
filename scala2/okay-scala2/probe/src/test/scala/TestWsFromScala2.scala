package scala2probe

import okay.http.Frame
import okay.scala2._

object WsModel {
  // each text frame answered with how many have arrived so far
  val counting: WsSession = WsSession.fold(0) {
    case (n, Frame.Text(t)) => (n + 1, Seq(Frame.Text(s"${n + 1}: $t")))
    case (n, _) => (n, Seq.empty)
  }
}

/** WebSockets from Scala 2.13, no socket (specs/scala2-facade.md, stage 12) */
class TestWsFromScala2 extends munit.FunSuite {
  import WsModel._

  test("a session is a fold over frames, replayed without a socket") {
    // okay.Chunk is a Scala 3 top-level alias, invisible from Scala 2;
    // it IS ArraySeq, so an ArraySeq goes where a Chunk is asked for
    val ping = Frame.Ping(scala.collection.immutable.ArraySeq[Byte](1, 2))
    val out = WsSession.replay(counting, Seq(Frame.Text("a"), ping, WebSocket.binary(Array[Byte](3)), Frame.Text("b")))
    assertEquals(out, Vector[Frame](Frame.Text("1: a"), Frame.Text("2: b")))
  }

  test("binary frames from and to Array[Byte]") {
    assertEquals(WebSocket.bytes(WebSocket.binary(Array[Byte](7, 8))).map(_.toList), Some(List[Byte](7, 8)))
    assertEquals(WebSocket.bytes(Frame.Text("x")), None)
  }

  test("echo sends text frames straight back") {
    assertEquals(WsSession.replay(WsSession.echo, Seq(Frame.Text("hi"))), Vector[Frame](Frame.Text("hi")))
  }
}

/** a real Jetty server and a real client socket; Live, like every
 * suite here that binds a port */
class TestWsLiveFromScala2 extends munit.FunSuite {
  import WsModel._

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  test("a client talks to a fold session over a real socket") {
    val routes = Routes { case GET(Path("health")) => pure(Response.text("ok")) }
    val got = (WsServer.use(0)(routes)({ case _ => counting }) { port =>
      for {
        ws <- WebSocket.connect("ws://127.0.0.1:" + port + "/count")
        _ <- ws.sendText("x")
        _ <- ws.sendText("y")
        replies <- ws.texts.take(2).runCollect
        _ <- ws.close()
        health <- Client().get("http://127.0.0.1:" + port + "/health")
      } yield (replies, health.text)
    }).runWith
    assertEquals(got, (Vector("1: x", "2: y"), "ok"))
  }
}
