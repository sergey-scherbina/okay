package okay2.platform

import scala.scalajs.js
import scala.scalajs.js.typedarray._
import okay2._
import okay2.async._

/**
 * The Node leg of the byte-stream seam — the Scala 3 core's
 * `NetNode.scala`: `data` events fill a buffer, an `Await` drains it, so
 * every protocol pump stays a sequential program. One pending reader at
 * a time, which is what the pumps are.
 */
private final class NodeConn(sock: js.Dynamic) extends NetConn {
  private var buf = new Array[Byte](0)
  private var eof = false
  private var failed: Throwable = null
  private var waiter: Option[(Int, Either[Throwable, Array[Byte]] => Unit)] = None

  locally {
    // a "data" event hands a Buffer, which IS a Uint8Array: typed at
    // the callback, not cast inside it
    val _ = sock.on("data", { (u: Uint8Array) =>
      val add = new Array[Byte](u.length)
      var i = 0
      while (i < u.length) { add(i) = (u(i).toInt & 0xff).toByte; i += 1 }
      buf = buf ++ add
      pump()
    }: js.Function1[Uint8Array, Unit])
    val _ = sock.on("end", { () => eof = true; pump() }: js.Function0[Unit])
    val _ = sock.on("close", { () => eof = true; pump() }: js.Function0[Unit])
    val _ = sock.on("error", { (e: js.Dynamic) =>
      failed = js.JavaScriptException(e)
      eof = true
      pump()
    }: js.Function1[js.Dynamic, Unit])
  }

  private def pump(): Unit = waiter match {
    case Some((n, k)) if buf.length >= n =>
      val out = buf.take(n)
      buf = buf.drop(n)
      waiter = None
      k(Right(out))
    case Some((n, k)) if eof =>
      waiter = None
      k(Left(if (failed != null) failed else NetEof(n, buf.length)))
    case _ => ()
  }

  def readFully(n: Int): Array[Byte] ! Async =
    Async.await[Array[Byte]] { k =>
      waiter = Some((n, k))
      pump()
      () => { waiter = None }
    }

  def write(bytes: Array[Byte]): Unit ! Async = Async {
    val _ = sock.write(byteArray2Int8Array(bytes))
    ()
  }

  def close(): Unit = { val _ = sock.end(); () }
}

private[platform] object NodeNet extends Net {
  def connect(host: String, port: Int): NetConn ! Async =
    Async.await[NetConn] { k =>
      val net = js.Dynamic.global.require("net")
      var settled = false
      val sock = net.connect(port, host)
      val conn = new NodeConn(sock)
      val _ = sock.once("connect", { () =>
        if (!settled) { settled = true; k(Right(conn)) }
      }: js.Function0[Unit])
      val _ = sock.once("error", { (e: js.Dynamic) =>
        if (!settled) { settled = true; k(Left(js.JavaScriptException(e))) }
      }: js.Function1[js.Dynamic, Unit])
      () => ()
    }
}
