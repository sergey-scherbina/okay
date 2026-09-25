package okay2.http

import scala.collection.immutable.ArraySeq
import okay2.{!, Pure, pure}
import okay2.async.Async
import okay2.platform._
import okay2.stream.Stage

/** the WebSocket transport against a real socket: the handshake, the
 * framing and the JDK's demand counter (okay-http's TestWs) */
class TestWs extends Live {

  val sockets = Transports.sockets()

  /** connect, run a session over the socket, close */
  def session[A](url: String)(s: Stage[Frame, Frame, A]): A =
    !.run(Async.run[A, Pure](sockets.connect(url).flatMap(sock => Ws.over(sock)(s).flatMap(a => sock.close().map(_ => a)))))

  /** say these frames, then stop after `n` answers */
  def sayAndTake(n: Int)(fs: Frame*): Stage[Frame, Frame, Seq[Frame]] = {
    def say(rest: List[Frame]): Stage[Frame, Frame, Unit] = rest match {
      case Nil => pure(())
      case f :: t => Stage.tell[Frame, Frame](f).flatMap(_ => say(t))
    }
    def take(k: Int, acc: List[Frame]): Stage[Frame, Frame, Seq[Frame]] =
      if (k == 0) pure(acc.reverse)
      else Stage.await[Frame, Frame].flatMap {
        case None => pure(acc.reverse)
        case Some(f) => take(k - 1, f :: acc)
      }
    say(fs.toList).flatMap(_ => take(n, Nil))
  }

  def withEcho[A](e: WsEcho)(f: WsEcho => A): A = try f(e) finally e.close()

  test("a text frame round-trips over a real socket") {
    withEcho(new WsEcho())(e => assertEquals(session(e.url)(sayAndTake(1)(Frame.Text("hello"))), Seq(Frame.Text("hello"))))
  }

  test("binary round-trips, bytes intact") {
    val bs = ArraySeq.unsafeWrapArray(Array[Byte](0, 1, -1, 127, -128))
    withEcho(new WsEcho())(e => assertEquals(session(e.url)(sayAndTake(1)(Frame.Binary(bs))), Seq(Frame.Binary(bs))))
  }

  test("several frames arrive in order") {
    withEcho(new WsEcho())(e => assertEquals(session(e.url)(sayAndTake(3)(Frame.Text("a"), Frame.Text("b"), Frame.Text("c"))),
      Seq(Frame.Text("a"), Frame.Text("b"), Frame.Text("c"))))
  }

  test("a FRAGMENTED message arrives as ONE frame: the session never sees the pieces") {
    val long = "abcdefghijklmnopqrstuvwxyz"
    withEcho(new WsEcho(fragmentEvery = 4))(e => assertEquals(session(e.url)(sayAndTake(1)(Frame.Text(long))), Seq(Frame.Text(long))))
  }

  test("a large message crosses whole, in one piece") {
    val big = "x" * 200000
    withEcho(new WsEcho()) { e =>
      session(e.url)(sayAndTake(1)(Frame.Text(big))) match {
        case Seq(Frame.Text(s)) => assertEquals(s.length, big.length)
        case other => fail(s"expected one text frame, got ${other.map(_.getClass.getSimpleName)}")
      }
    }
  }

  test("a ping is answered by the peer as a pong the session sees") {
    val empty = ArraySeq.empty[Byte]
    withEcho(new WsEcho())(e => assertEquals(session(e.url)(sayAndTake(1)(Frame.Ping(empty))), Seq(Frame.Pong(empty))))
  }

  test("the stream ends at close, and the session sees the Close frame") {
    withEcho(new WsEcho()) { e =>
      val got = session(e.url)(sayAndTake(2)(Frame.Text("last"), Frame.Close(Frame.Normal, "bye")))
      assertEquals(got.head, Frame.Text("last"))
      assert(got(1).isInstanceOf[Frame.Close], s"expected a Close, got ${got(1)}")
    }
  }

  test("close is HALF-duplex: frames in flight after our Close still arrive") {
    withEcho(new WsEcho(partingWords = 3)) { e =>
      val got = session(e.url)(sayAndTake(4)(Frame.Close(Frame.Normal, "leaving")))
      assertEquals(got.take(3), Seq(Frame.Text("parting-0"), Frame.Text("parting-1"), Frame.Text("parting-2")))
      assert(got(3).isInstanceOf[Frame.Close], s"the stream must end at the peer's Close: $got")
    }
  }
}
