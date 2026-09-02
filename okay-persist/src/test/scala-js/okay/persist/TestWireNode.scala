package okay.persist

import scala.scalajs.js
import scala.scalajs.js.typedarray.*
import scala.concurrent.{Future, Promise}
import okay.{!, Async}
import okay.given
import okay.codec.Cbor
import WireProtocol.{Req, Resp}

/**
 * The openness acceptance, literal (specs/net.md): a SCRIPTED NODE
 * SERVER answers the documented frames — encoded with the SAME
 * shared enums — and the SAME shared client talks to it, with no
 * JVM anywhere in this process.
 */
class TestWireNode extends munit.FunSuite:

  given scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  /** a Node net server that parses [len][CBOR] frames and answers
   * by script — the other end of the documented surface */
  def scripted(answer: Req => Resp): Future[(js.Dynamic, Int)] =
    val p = Promise[(js.Dynamic, Int)]()
    val net = js.Dynamic.global.require("net")
    val server = net.createServer({ (sock: js.Dynamic) =>
      var buf = new Array[Byte](0)
      val _ = sock.on("data", { (d: js.Dynamic) =>
        val u = d.asInstanceOf[Uint8Array]
        val add = new Array[Byte](u.length)
        var i = 0
        while i < u.length do { add(i) = (u(i).toInt & 0xff).toByte; i += 1 }
        buf = buf ++ add
        var going = true
        while going && buf.length >= 4 do
          val len = ((buf(0) & 0xff) << 24) | ((buf(1) & 0xff) << 16) |
            ((buf(2) & 0xff) << 8) | (buf(3) & 0xff)
          if buf.length < 4 + len then going = false
          else
            val body = buf.slice(4, 4 + len)
            buf = buf.drop(4 + len)
            val resp = Cbor.read[Req](body)
              .fold(e => Resp.Refused(s"damaged: $e"), answer)
            val bs = Cbor.write(resp)
            val frame = new Array[Byte](4 + bs.length)
            frame(0) = (bs.length >> 24).toByte
            frame(1) = (bs.length >> 16).toByte
            frame(2) = (bs.length >> 8).toByte
            frame(3) = bs.length.toByte
            System.arraycopy(bs, 0, frame, 4, bs.length)
            val _ = sock.write(byteArray2Int8Array(frame))
            ()
      }: js.Function1[js.Dynamic, Unit])
      ()
    }: js.Function1[js.Dynamic, Unit])
    val _ = server.listen(0, { () =>
      p.success((server, server.address().port.asInstanceOf[Int]))
    }: js.Function0[Unit])
    p.future

  test("the shared client speaks to a Node server: no JVM in this process") {
    scripted {
      case Req.Hello(v, "friend") => Resp.Granted(v, Vector("events"))
      case Req.Hello(_, _) => Resp.Refused("the token opens nothing here")
      case Req.Append("events", 0, _, _, _) => Resp.Appended(7L)
      case Req.End("events", 0) => Resp.Offset(8L)
      case Req.Read("events", 0, from, _) => Resp.TooEarly(3L)
      case other => Resp.Refused(s"unscripted: $other")
    }.flatMap { (server, port) =>
      val prog: (Vector[String], Long, Long, Topic.Read) ! Async =
        for
          c <- WireProtocol.Client.connect("127.0.0.1", port, "friend")
          off <- c.append("events", 0, Array.empty, "v".getBytes("UTF-8"))
          end <- c.end("events", 0)
          rd <- c.read("events", 0, 0L, 10)
        yield (c.topics, off, end, rd)
      Async.runAsync(prog).map { (topics, off, end, rd) =>
        val _ = server.close()
        assertEquals(topics, Vector("events"))
        assertEquals(off, 7L)
        assertEquals(end, 8L)
        assertEquals(rd, Topic.Read.TooEarly(3L))
      }
    }
  }

  test("a refusal crosses the Node wire by name") {
    scripted {
      case Req.Hello(_, _) => Resp.Refused("the token opens nothing here")
      case other => Resp.Refused(s"unscripted: $other")
    }.flatMap { (server, port) =>
      Async.runAsync(WireProtocol.Client.connect("127.0.0.1", port, "stranger"))
        .transform {
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
