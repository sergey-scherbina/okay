package okay2.persist

import scala.scalajs.js
import scala.scalajs.js.typedarray._
import scala.concurrent.{Future, Promise}
import okay2.!
import okay2.async.Async
import okay2.codec.Cbor
import okay2.platform._
import WireProtocol.{Req, Resp}

/**
 * The openness acceptance, literal (okay-persist's TestWireNode;
 * specs/net.md): a SCRIPTED NODE SERVER answers the documented frames —
 * encoded with the SAME shared types — and the SAME shared client talks
 * to it, with no JVM anywhere in this process. `Live`: it binds a port.
 */
class TestWireNode extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  implicit val ec: scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  /** a Node net server that parses [len][CBOR] frames and answers by
   * script — the other end of the documented surface */
  def scripted(answer: Req => Resp): Future[(js.Dynamic, Int)] = {
    val p = Promise[(js.Dynamic, Int)]()
    val net = js.Dynamic.global.require("net")
    val server = net.createServer({ (sock: js.Dynamic) =>
      var buf = new Array[Byte](0)
      val _ = sock.on("data", { (d: js.Dynamic) =>
        // a Node Buffer IS a Uint8Array: the one view this callback gets
        val u = d.asInstanceOf[Uint8Array]
        val add = new Array[Byte](u.length)
        var i = 0
        while (i < u.length) { add(i) = (u(i).toInt & 0xff).toByte; i += 1 }
        buf = buf ++ add
        var going = true
        while (going && buf.length >= 4) {
          val len = ((buf(0) & 0xff) << 24) | ((buf(1) & 0xff) << 16) | ((buf(2) & 0xff) << 8) | (buf(3) & 0xff)
          if (buf.length < 4 + len) going = false
          else {
            val body = buf.slice(4, 4 + len)
            buf = buf.drop(4 + len)
            val resp = Cbor.read[Req](body).fold(e => Resp.Refused(s"damaged: $e"), answer)
            val bs = Cbor.write(resp)
            val frame = new Array[Byte](4 + bs.length)
            frame(0) = (bs.length >> 24).toByte
            frame(1) = (bs.length >> 16).toByte
            frame(2) = (bs.length >> 8).toByte
            frame(3) = bs.length.toByte
            System.arraycopy(bs, 0, frame, 4, bs.length)
            val _ = sock.write(byteArray2Int8Array(frame))
          }
        }
      }: js.Function1[js.Dynamic, Unit])
    }: js.Function1[js.Dynamic, Unit])
    val _ = server.listen(0, { () =>
      // `address()` is an object with a numeric port once listening
      val _ = p.success((server, server.address().port.asInstanceOf[Int]))
    }: js.Function0[Unit])
    p.future
  }

  test("the shared client speaks to a Node server: no JVM in this process") {
    scripted {
      case Req.Hello(v, "friend") => Resp.Granted(v, Vector("events"))
      case Req.Hello(_, _) => Resp.Refused("the token opens nothing here")
      case Req.Append("events", 0, _, _, _) => Resp.Appended(7L)
      case Req.End("events", 0) => Resp.Offset(8L)
      case Req.Read("events", 0, _, _) => Resp.TooEarly(3L)
      case other => Resp.Refused(s"unscripted: $other")
    }.flatMap { case (server, port) =>
      val prog: (Vector[String], Long, Long, Topic.Read) ! Async =
        WireProtocol.Client.connect("127.0.0.1", port, "friend").flatMap { c =>
          c.append("events", 0, Array.empty[Byte], "v".getBytes("UTF-8")).flatMap { off =>
            c.end("events", 0).flatMap { end =>
              c.read("events", 0, 0L, 10).map(rd => (c.topics, off, end, rd))
            }
          }
        }
      Async.runAsync(prog).map { case (topics, off, end, rd) =>
        val _ = server.close()
        assertEquals(topics, Vector("events"))
        assertEquals(off, 7L)
        assertEquals(end, 8L)
        assertEquals(rd, Topic.Read.TooEarly(3L): Topic.Read)
      }
    }
  }

  test("a refusal crosses the Node wire by name") {
    scripted {
      case Req.Hello(_, _) => Resp.Refused("the token opens nothing here")
      case other => Resp.Refused(s"unscripted: $other")
    }.flatMap { case (server, port) =>
      Async.runAsync(WireProtocol.Client.connect("127.0.0.1", port, "stranger")).transform {
        case scala.util.Failure(e: WireProtocol.WireRefused) =>
          val _ = server.close()
          assert(e.reason.contains("token"), e.reason)
          scala.util.Success(())
        case other =>
          val _ = server.close()
          scala.util.Failure(new AssertionError(s"expected WireRefused, got $other"))
      }
    }
  }
}
