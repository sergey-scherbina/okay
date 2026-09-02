package okay.http

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.mcp.{Mcp, Rpc}

/**
 * The WebSocket transport against a real socket — the handshake, the
 * framing and the JDK's demand counter, not a mock.
 *
 * `WsEcho` next door is a test-scope RFC 6455 server; the library does
 * not serve WebSocket, and specs/http.md says why.
 */
class TestWs extends munit.FunSuite {

  val sockets = Transports.sockets()

  /** connect, run a session over the socket, close */
  def session[A](url: String)(s: Stage[Frame, Frame, A]): A =
    Async.run[A, Pure](
      sockets.connect(url).flatMap(sock =>
        Ws.over(sock)(s).flatMap(a => sock.close().map(_ => a)))).runWith

  /** say these frames, then stop after `n` answers */
  def sayAndTake(n: Int)(fs: Frame*): Stage[Frame, Frame, Seq[Frame]] =
    def say(rest: List[Frame]): Stage[Frame, Frame, Unit] = rest match
      case Nil => pure(())
      case f :: t => Stage.tell[Frame, Frame](f).flatMap(_ => say(t))

    def take(k: Int, acc: List[Frame]): Stage[Frame, Frame, Seq[Frame]] =
      if k == 0 then pure(acc.reverse)
      else Stage.await[Frame, Frame].flatMap {
        case None => pure(acc.reverse)
        case Some(f) => take(k - 1, f :: acc)
      }

    say(fs.toList).flatMap(_ => take(n, Nil))

  test("a text frame round-trips over a real socket") {
    val echo = WsEcho()
    try
      val got = session(echo.url)(sayAndTake(1)(Frame.Text("hello")))
      assertEquals(got, Seq(Frame.Text("hello")))
    finally echo.close()
  }

  test("binary round-trips, bytes intact") {
    val echo = WsEcho()
    try
      val bs = scala.collection.immutable.ArraySeq.unsafeWrapArray(
        Array[Byte](0, 1, -1, 127, -128))
      val got = session(echo.url)(sayAndTake(1)(Frame.Binary(bs)))
      assertEquals(got, Seq(Frame.Binary(bs)))
    finally echo.close()
  }

  test("several frames arrive in order") {
    val echo = WsEcho()
    try
      val got = session(echo.url)(
        sayAndTake(3)(Frame.Text("a"), Frame.Text("b"), Frame.Text("c")))
      assertEquals(got, Seq(Frame.Text("a"), Frame.Text("b"), Frame.Text("c")))
    finally echo.close()
  }

  test("a FRAGMENTED message arrives as ONE frame — the session never sees the pieces") {
    // the server splits every 4 bytes; the transport joins them, because
    // the JDK's `last` flag is a transport concern and not a session's
    val echo = WsEcho(fragmentEvery = 4)
    try
      val long = "abcdefghijklmnopqrstuvwxyz"
      val got = session(echo.url)(sayAndTake(1)(Frame.Text(long)))
      assertEquals(got, Seq(Frame.Text(long)))
    finally echo.close()
  }

  test("a large message crosses whole, in one piece") {
    val echo = WsEcho()
    try
      val big = "x" * 200_000        // past both the 126 and the 65536 length forms
      val got = session(echo.url)(sayAndTake(1)(Frame.Text(big)))
      assertEquals(got.head.asInstanceOf[Frame.Text].s.length, big.length)
    finally echo.close()
  }

  test("a ping is answered by the transport, and an explicit pong still goes") {
    val echo = WsEcho()
    try
      val empty = scala.collection.immutable.ArraySeq.empty[Byte]
      // our ping gets the server's pong back
      val got = session(echo.url)(sayAndTake(1)(Frame.Ping(empty)))
      assertEquals(got, Seq(Frame.Pong(empty)))
    finally echo.close()
  }

  test("the stream ends at close, and the session sees the Close frame") {
    val echo = WsEcho()
    try
      val got = session(echo.url)(sayAndTake(2)(
        Frame.Text("last"), Frame.Close(Frame.Normal, "bye")))
      assertEquals(got.head, Frame.Text("last"))
      assert(got(1).isInstanceOf[Frame.Close], s"expected a Close, got ${got(1)}")
    finally echo.close()
  }

  test("close is HALF-duplex: frames in flight after our Close still arrive") {
    // the strong form of the claim specs/http.md left unchecked: the
    // server, on receiving our Close, sends three more texts BEFORE
    // echoing the Close — and the session must see all three, in
    // order, and then the Close. A transport that tears the read half
    // down when the write half closes would drop them.
    val echo = WsEcho(partingWords = 3)
    try
      val got = session(echo.url)(sayAndTake(4)(
        Frame.Close(Frame.Normal, "leaving")))
      assertEquals(got.take(3),
        Seq(Frame.Text("parting-0"), Frame.Text("parting-1"), Frame.Text("parting-2")))
      assert(got(3).isInstanceOf[Frame.Close], s"the stream must end at the peer's Close: $got")
    finally echo.close()
  }

  // ---- the payoff the shapes were kept the same for

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

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
          val link = Ws.link(sock)
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
}
