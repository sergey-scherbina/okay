package okay2.persist

import okay2.!
import okay2.async.Async
import okay2.codec.{Codecs, Schema}
import okay2.platform.{Net, NetConn}

/**
 * The wire's SHARED half (okay-persist's WireProtocol.scala;
 * specs/persist.md "The wire", specs/net.md): the message types are the
 * one source of truth for both ends and every platform — the JVM server,
 * the JVM client and the Node client speak these exact frames,
 * `[len: int32 BE][CBOR]`, nothing else.
 */
object WireProtocol {

  // Version 2 added the replication surface (Produce/Promote) and
  // Compact. New Req/Resp cases are APPENDED so the ordinals of v1
  // messages never move.
  val Version = 2

  implicit lazy val recordSchema: Schema[Record] = Schema.derived

  sealed trait Req
  object Req {
    final case class Hello(version: Int, token: String) extends Req
    final case class Append(topic: String, partition: Int, key: Array[Byte], value: Array[Byte], ack: Int) extends Req
    final case class Read(topic: String, partition: Int, from: Long, max: Int) extends Req
    final case class Begin(topic: String, partition: Int) extends Req
    final case class End(topic: String, partition: Int) extends Req
    // v2: the Topic surface completed, and the coordinator's calls
    final case class Compact(topic: String, partition: Int) extends Req
    final case class Produce(topic: String, partition: Int, producerId: String,
                             seq: Long, key: Array[Byte], value: Array[Byte], ack: Int) extends Req
    final case class Promote(topic: String, partition: Int, replica: Int) extends Req
    implicit lazy val schema: Schema[Req] = Schema.derived
  }

  sealed trait Resp
  object Resp {
    final case class Granted(version: Int, topics: Vector[String]) extends Resp
    final case class Appended(offset: Long) extends Resp
    final case class Records(records: Vector[Record]) extends Resp
    final case class TooEarly(begin: Long) extends Resp
    final case class Offset(value: Long) extends Resp
    final case class Refused(reason: String) extends Resp
    // v2: the ack-only answer (Compact, Promote)
    case object Done extends Resp
    implicit lazy val schema: Schema[Resp] = Schema.derived
  }

  final case class WireRefused(reason: String) extends RuntimeException(s"the persist node refused: $reason")

  private[persist] def ackOf(i: Int): Ack = i match {
    case 0 => Ack.Received
    case 2 => Ack.Replicated
    case _ => Ack.Durable
  }

  private[persist] def ackCode(a: Ack): Int = a match {
    case Ack.Received => 0
    case Ack.Durable => 1
    case Ack.Replicated => 2
  }

  // ── frames over the Net seam ───────────────────────────────────

  def writeFrame[A](conn: NetConn, a: A)(implicit s: Schema[A]): Unit ! Async = {
    val bs = Codecs.writeCbor(a)
    val out = new Array[Byte](4 + bs.length)
    out(0) = (bs.length >> 24).toByte
    out(1) = (bs.length >> 16).toByte
    out(2) = (bs.length >> 8).toByte
    out(3) = bs.length.toByte
    System.arraycopy(bs, 0, out, 4, bs.length)
    conn.write(out)
  }

  def readFrame[A](conn: NetConn)(implicit s: Schema[A]): A ! Async =
    conn.readFully(4).flatMap { l =>
      val len = ((l(0) & 0xff) << 24) | ((l(1) & 0xff) << 16) | ((l(2) & 0xff) << 8) | (l(3) & 0xff)
      if (len < 0 || len > 64 * 1024 * 1024) throw WireRefused(s"frame length $len is not a frame")
      conn.readFully(len).map { bs =>
        Codecs.readCbor[A](bs).fold(e => throw WireRefused(s"a damaged frame: $e"), identity)
      }
    }

  private def unexpected(other: Resp): Nothing = other match {
    case Resp.Refused(r) => throw WireRefused(r)
    case _ => throw WireRefused(s"unexpected answer $other")
  }

  /**
   * The cross-platform client: the SAME code on the JVM (blocking socket
   * underneath) and on Node (buffered pulls underneath) — which platform
   * moves the bytes is the implicit `Net`'s business. One logical thread
   * of control per client.
   */
  final class Client private[WireProtocol] (conn: NetConn, val topics: Vector[String]) {

    private def call(req: Req): Resp ! Async =
      writeFrame(conn, req).flatMap(_ => readFrame[Resp](conn))

    def append(topic: String, partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack = Ack.Durable): Long ! Async =
      call(Req.Append(topic, partition, key, value, ackCode(ack))).map {
        case Resp.Appended(off) => off
        case other => unexpected(other)
      }

    def read(topic: String, partition: Int, from: Long, max: Int): Topic.Read ! Async =
      call(Req.Read(topic, partition, from, max)).map {
        case Resp.Records(rs) => Topic.Read.Records(rs)
        case Resp.TooEarly(b) => Topic.Read.TooEarly(b)
        case other => unexpected(other)
      }

    def begin(topic: String, partition: Int): Long ! Async = offsetOf(Req.Begin(topic, partition))
    def end(topic: String, partition: Int): Long ! Async = offsetOf(Req.End(topic, partition))

    private def offsetOf(req: Req): Long ! Async = call(req).map {
      case Resp.Offset(v) => v
      case other => unexpected(other)
    }

    /** the force-compact admin call, over the wire */
    def compact(topic: String, partition: Int): Unit ! Async = done(Req.Compact(topic, partition))

    /** the idempotent producer: a retry with the same (producerId, seq)
     * lands once and answers the ORIGINAL offset — the server's topic
     * must be a replicated coordinator, else it refuses */
    def produce(topic: String, partition: Int, producerId: String, seq: Long,
                key: Array[Byte], value: Array[Byte], ack: Ack = Ack.Replicated): Long ! Async =
      call(Req.Produce(topic, partition, producerId, seq, key, value, ackCode(ack))).map {
        case Resp.Appended(off) => off
        case other => unexpected(other)
      }

    /** the operator's failover, driven remotely */
    def promote(topic: String, partition: Int, replica: Int): Unit ! Async = done(Req.Promote(topic, partition, replica))

    private def done(req: Req): Unit ! Async = call(req).map {
      case Resp.Done => ()
      case other => unexpected(other)
    }

    def close(): Unit = conn.close()
  }

  object Client {
    /** connect + Hello; the answer's capability list IS the offer */
    def connect(host: String, port: Int, token: String)(implicit net: Net): Client ! Async =
      Net.connect(host, port).flatMap { conn =>
        writeFrame[Req](conn, Req.Hello(Version, token))
          .flatMap(_ => readFrame[Resp](conn))
          .map {
            case Resp.Granted(_, topics) => new Client(conn, topics)
            case Resp.Refused(r) => conn.close(); throw WireRefused(r)
            case other => conn.close(); throw WireRefused(s"a broken handshake: $other")
          }
      }
  }
}
