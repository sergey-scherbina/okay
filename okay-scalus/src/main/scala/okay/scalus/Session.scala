package okay.scalus

import N2N.Segment

/** the byte transport under a session: a socket, or a recording */
trait Wire:
  def read(): Wire.Read
  def write(s: Segment): Unit
  def close(): Unit

object Wire:
  enum Read:
    case Got(segment: Segment)
    /** nothing arrived within the wire's idle interval — the session
     * sends a keep-alive and waits again */
    case Idle
    case Closed

  /** a TCP connection to a relay, on the calling (virtual) thread */
  def tcp(host: String, port: Int, idleMillis: Int = 20_000): Wire =
    val s = java.net.Socket(host, port)
    s.setSoTimeout(idleMillis)
    s.setTcpNoDelay(true)
    val in = java.io.DataInputStream(java.io.BufferedInputStream(s.getInputStream))
    val out = s.getOutputStream
    new Wire:
      def read(): Read =
        try
          val h = new Array[Byte](8)
          in.readFully(h)
          val (time, responder, protocol, length) = Segment.header(h)
          val p = new Array[Byte](length)
          in.readFully(p)
          Read.Got(Segment(time, responder, protocol, p))
        catch
          case _: java.net.SocketTimeoutException => Read.Idle
          case _: java.io.EOFException => Read.Closed
      def write(seg: Segment): Unit = { out.write(seg.bytes); out.flush() }
      def close(): Unit = s.close()

/**
 * One node-to-node connection, single-threaded: `receive(p)` pumps
 * segments off the wire until protocol `p` has a whole message,
 * queueing other protocols' messages as they pass. While a receive
 * waits on a quiet chain, every idle interval sends a keep-alive — the
 * relay drops a peer that says nothing — and its answers are dropped.
 */
final class Session private (wire: Wire):
  private val demux = N2N.Demux()
  private val queues = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.Queue[Cv]]
  private val started = System.nanoTime()
  private var cookie = 0

  private def now: Int = ((System.nanoTime() - started) / 1000).toInt

  def send(protocol: Int, message: Array[Byte]): Unit =
    N2N.segments(protocol, message, now).foreach(wire.write)

  def receive(protocol: Int): Either[String, Cv] =
    var got: Option[Either[String, Cv]] = None
    while got.isEmpty do
      queues.get(protocol).filter(_.nonEmpty) match
        case Some(q) => got = Some(Right(q.dequeue()))
        case None => wire.read() match
          case Wire.Read.Got(s) => demux.feed(s) match
            case Left(e) => got = Some(Left(e))
            case Right(_) if s.protocol == N2N.KeepAlive => ()   // answers to our pings
            case Right(ms) => queues.getOrElseUpdate(s.protocol, scala.collection.mutable.Queue.empty) ++= ms
          case Wire.Read.Idle =>
            cookie = (cookie + 1) & 0xFFFF
            send(N2N.KeepAlive, N2N.keepAlive(cookie))
          case Wire.Read.Closed => got = Some(Left("the relay closed the connection"))
    got.get

  def close(): Unit = wire.close()

object Session:
  /** a session over `wire`, handshaken for `magic` */
  def open(wire: Wire, magic: Long): Either[String, Session] =
    val s = new Session(wire)
    s.send(N2N.Handshake, N2N.proposeVersions(magic))
    s.receive(N2N.Handshake).flatMap(N2N.handshaken).flatMap {
      case N2N.Handshaken.Accepted(_, m) if m == magic => Right(s)
      case N2N.Handshaken.Accepted(v, m) => Left(s"version $v accepted for network magic $m, asked $magic")
      case N2N.Handshaken.Refused(why) => Left(s"handshake refused: $why")
    }
