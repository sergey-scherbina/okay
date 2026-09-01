package okay

import scala.scalajs.js
import scala.scalajs.js.typedarray.*

/**
 * The Node leg (specs/net.md): `data` events fill a buffer, an
 * Await drains it — the adapter absorbs the push/pull impedance
 * once, so every protocol pump stays a sequential program. One
 * pending reader at a time, which is what the pumps are.
 */
private final class NodeConn(sock: js.Dynamic) extends NetConn:
  private var buf = new Array[Byte](0)
  private var eof = false
  private var failed: Throwable | Null = null
  private var waiter: Option[(Int, Either[Throwable, Array[Byte]] => Unit)] = None

  locally {
    sock.on("data", { (d: js.Dynamic) =>
      val u = d.asInstanceOf[Uint8Array]
      val add = new Array[Byte](u.length)
      var i = 0
      while i < u.length do { add(i) = (u(i).toInt & 0xff).toByte; i += 1 }
      buf = buf ++ add
      pump()
    }: js.Function1[js.Dynamic, Unit])
    sock.on("end", { () => eof = true; pump() }: js.Function0[Unit])
    sock.on("close", { () => eof = true; pump() }: js.Function0[Unit])
    sock.on("error", { (e: js.Dynamic) =>
      failed = js.JavaScriptException(e)
      eof = true
      pump()
    }: js.Function1[js.Dynamic, Unit])
    ()
  }

  private def pump(): Unit = waiter match
    case Some((n, k)) if buf.length >= n =>
      val out = buf.take(n)
      buf = buf.drop(n)
      waiter = None
      k(Right(out))
    case Some((n, k)) if eof =>
      waiter = None
      k(Left(if failed != null then failed else NetEof(n, buf.length)))
    case _ => ()

  def readFully(n: Int): Array[Byte] ! Async =
    Async.await[Array[Byte]] { k =>
      waiter = Some((n, k))
      pump()
      () => { waiter = None }
    }

  def write(bytes: Array[Byte]): Unit ! Async = async {
    sock.write(byteArray2Int8Array(bytes))
    ()
  }

  def close(): Unit =
    sock.end()
    ()

given Net = new Net:
  def connect(host: String, port: Int): NetConn ! Async =
    Async.await[NetConn] { k =>
      val net = js.Dynamic.global.require("net")
      var settled = false
      val sock = net.connect(port, host)
      val conn = NodeConn(sock)
      sock.once("connect", { () =>
        if !settled then { settled = true; k(Right(conn)) }
      }: js.Function0[Unit])
      sock.once("error", { (e: js.Dynamic) =>
        if !settled then { settled = true; k(Left(js.JavaScriptException(e))) }
      }: js.Function1[js.Dynamic, Unit])
      () => ()
    }
