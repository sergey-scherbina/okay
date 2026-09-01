package okay.persist

import okay.{!, Async, Net, NetConn}
import okay.codec.{Cbor, Schema}

/**
 * The wire's SHARED half (specs/persist.md "The wire", specs/
 * net.md): the message enums are the one source of truth for both
 * ends and every platform — the JVM server, the JVM client, and
 * the Node client all speak these exact frames. `[len: int32 BE]
 * [CBOR]`, nothing else.
 */
object WireProtocol {

  // Version 2 added the replication surface (Produce/Promote) and
  // Compact, so the wire covers the whole Topic and the stage-2
  // coordinator (specs/persist.md, persist-wire-repl). New Req/Resp
  // cases are APPENDED so the CBOR ordinals of v1 messages never move.
  val Version = 2

  given Schema[Record] = Schema.derived

  enum Req derives Schema:
    case Hello(version: Int, token: String)
    case Append(topic: String, partition: Int,
                key: Array[Byte], value: Array[Byte], ack: Int)
    case Read(topic: String, partition: Int, from: Long, max: Int)
    case Begin(topic: String, partition: Int)
    case End(topic: String, partition: Int)
    // v2: the Topic surface completed, and the coordinator's calls
    case Compact(topic: String, partition: Int)
    case Produce(topic: String, partition: Int, producerId: String,
                 seq: Long, key: Array[Byte], value: Array[Byte], ack: Int)
    case Promote(topic: String, partition: Int, replica: Int)

  enum Resp derives Schema:
    case Granted(version: Int, topics: Vector[String])
    case Appended(offset: Long)
    case Records(records: Vector[Record])
    case TooEarly(begin: Long)
    case Offset(value: Long)
    case Refused(reason: String)
    // v2: the ack-only answer (Compact, Promote)
    case Done()

  final case class WireRefused(reason: String)
    extends RuntimeException(s"the persist node refused: $reason")

  private[persist] def ackOf(i: Int): Ack = i match
    case 0 => Ack.Received
    case 2 => Ack.Replicated
    case _ => Ack.Durable

  private[persist] def ackCode(a: Ack): Int = a match
    case Ack.Received => 0
    case Ack.Durable => 1
    case Ack.Replicated => 2

  // ── frames over the Net seam ───────────────────────────────────

  def writeFrame[A](conn: NetConn, a: A)(using Schema[A]): Unit ! Async =
    val bs = Cbor.write(a)
    val out = new Array[Byte](4 + bs.length)
    out(0) = (bs.length >> 24).toByte
    out(1) = (bs.length >> 16).toByte
    out(2) = (bs.length >> 8).toByte
    out(3) = bs.length.toByte
    System.arraycopy(bs, 0, out, 4, bs.length)
    conn.write(out)

  def readFrame[A](conn: NetConn)(using Schema[A]): A ! Async =
    conn.readFully(4).flatMap { l =>
      val len = ((l(0) & 0xff) << 24) | ((l(1) & 0xff) << 16) |
        ((l(2) & 0xff) << 8) | (l(3) & 0xff)
      if len < 0 || len > 64 * 1024 * 1024 then
        throw WireRefused(s"frame length $len is not a frame")
      conn.readFully(len).map { bs =>
        Cbor.read[A](bs).fold(e => throw WireRefused(s"a damaged frame: $e"), identity)
      }
    }

  /**
   * The cross-platform client: the SAME code on the JVM (blocking
   * socket underneath) and on Node (buffered pulls underneath) —
   * which platform moves the bytes is the `given Net`'s business.
   * One logical thread of control per client, the driver contract.
   */
  final class Client private[WireProtocol] (conn: NetConn, val topics: Vector[String]):

    private def call(req: Req): Resp ! Async =
      writeFrame(conn, req).flatMap(_ => readFrame[Resp](conn))

    def append(topic: String, partition: Int, key: Array[Byte],
               value: Array[Byte], ack: Ack = Ack.Durable): Long ! Async =
      call(Req.Append(topic, partition, key, value, ackCode(ack))).map {
        case Resp.Appended(off) => off
        case Resp.Refused(r) => throw WireRefused(r)
        case other => throw WireRefused(s"unexpected answer $other")
      }

    def read(topic: String, partition: Int, from: Long, max: Int): Topic.Read ! Async =
      call(Req.Read(topic, partition, from, max)).map {
        case Resp.Records(rs) => Topic.Read.Records(rs)
        case Resp.TooEarly(b) => Topic.Read.TooEarly(b)
        case Resp.Refused(r) => throw WireRefused(r)
        case other => throw WireRefused(s"unexpected answer $other")
      }

    def begin(topic: String, partition: Int): Long ! Async =
      offsetOf(Req.Begin(topic, partition))
    def end(topic: String, partition: Int): Long ! Async =
      offsetOf(Req.End(topic, partition))

    private def offsetOf(req: Req): Long ! Async = call(req).map {
      case Resp.Offset(v) => v
      case Resp.Refused(r) => throw WireRefused(r)
      case other => throw WireRefused(s"unexpected answer $other")
    }

    /** the force-compact admin call, over the wire */
    def compact(topic: String, partition: Int): Unit ! Async =
      done(Req.Compact(topic, partition))

    /** the idempotent producer: a retry with the same (producerId,
     * seq) lands once and answers the ORIGINAL offset — the server's
     * topic must be a replicated coordinator, else it refuses */
    def produce(topic: String, partition: Int, producerId: String, seq: Long,
                key: Array[Byte], value: Array[Byte], ack: Ack = Ack.Replicated): Long ! Async =
      call(Req.Produce(topic, partition, producerId, seq, key, value, ackCode(ack))).map {
        case Resp.Appended(off) => off
        case Resp.Refused(r) => throw WireRefused(r)
        case other => throw WireRefused(s"unexpected answer $other")
      }

    /** the operator's failover, driven remotely */
    def promote(topic: String, partition: Int, replica: Int): Unit ! Async =
      done(Req.Promote(topic, partition, replica))

    private def done(req: Req): Unit ! Async = call(req).map {
      case Resp.Done() => ()
      case Resp.Refused(r) => throw WireRefused(r)
      case other => throw WireRefused(s"unexpected answer $other")
    }

    def close(): Unit = conn.close()

  object Client:
    /** connect + Hello; the answer's capability list IS the offer */
    def connect(host: String, port: Int, token: String)(using Net): Client ! Async =
      Net.connect(host, port).flatMap { conn =>
        writeFrame(conn, Req.Hello(Version, token))
          .flatMap(_ => readFrame[Resp](conn))
          .map {
            case Resp.Granted(_, topics) => new Client(conn, topics)
            case Resp.Refused(r) => conn.close(); throw WireRefused(r)
            case other => conn.close(); throw WireRefused(s"a broken handshake: $other")
          }
      }
}
