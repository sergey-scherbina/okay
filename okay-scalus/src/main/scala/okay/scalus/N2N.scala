package okay.scalus

import okay.codec.Cbor

/**
 * Ouroboros node-to-node, the parts a READER needs (specs/scalus.md §1;
 * message shapes from ouroboros-network's CDDL, cardano-diffusion/
 * protocols/cddl/specs at a3d8017): the mux framing and the handshake,
 * chain-sync, block-fetch and keep-alive messages. Pure: bytes in,
 * values out, and the reverse; the socket is `Session`'s.
 */
object N2N:
  val Handshake = 0
  val ChainSync = 2
  val BlockFetch = 3
  val KeepAlive = 8

  /** a mux segment: an 8-byte header (32-bit timestamp, the responder
   * bit and a 15-bit protocol id, a 16-bit length) and its payload */
  final case class Segment(time: Int, responder: Boolean, protocol: Int, payload: Array[Byte]):
    def bytes: Array[Byte] =
      val h = new Array[Byte](8 + payload.length)
      h(0) = (time >>> 24).toByte; h(1) = (time >>> 16).toByte; h(2) = (time >>> 8).toByte; h(3) = time.toByte
      val pm = (if responder then 0x8000 else 0) | (protocol & 0x7FFF)
      h(4) = (pm >>> 8).toByte; h(5) = pm.toByte
      h(6) = (payload.length >>> 8).toByte; h(7) = payload.length.toByte
      System.arraycopy(payload, 0, h, 8, payload.length)
      h

  object Segment:
    /** the largest payload a node-to-node segment carries */
    val MaxPayload = 12288

    def header(h: Array[Byte]): (Int, Boolean, Int, Int) =
      def u(i: Int) = h(i) & 0xFF
      val time = (u(0) << 24) | (u(1) << 16) | (u(2) << 8) | u(3)
      val pm = (u(4) << 8) | u(5)
      (time, (pm & 0x8000) != 0, pm & 0x7FFF, (u(6) << 8) | u(7))

  /** a message's bytes cut into segments no longer than MaxPayload */
  def segments(protocol: Int, message: Array[Byte], time: Int): Vector[Segment] =
    message.grouped(Segment.MaxPayload).map(p => Segment(time, false, protocol, p)).toVector match
      case v if v.isEmpty => Vector(Segment(time, false, protocol, Array.emptyByteArray))
      case v => v

  /**
   * Whole messages out of a protocol's byte stream: a message may span
   * segments, and one segment may end one message and start the next.
   */
  final class Demux:
    private val pending = scala.collection.mutable.Map.empty[Int, Array[Byte]]
    def feed(s: Segment): Either[String, Vector[Cv]] =
      var buf = pending.getOrElse(s.protocol, Array.emptyByteArray) ++ s.payload
      val out = Vector.newBuilder[Cv]
      var err: Option[String] = None
      var more = buf.nonEmpty
      while more && err.isEmpty do
        Cv.read(buf) match
          case Cv.Read.Done(v, n) => out += v; buf = buf.drop(n); more = buf.nonEmpty
          case Cv.Read.Incomplete => more = false
          case Cv.Read.Bad(m) => err = Some(s"protocol ${s.protocol}: $m")
      pending(s.protocol) = buf
      err.toLeft(out.result())

  // ---- points and tips ----------------------------------------------

  /** a chain-sync point: a slot and a header hash, or the origin */
  final case class Pt(slot: Long, hash: Array[Byte]):
    def hex: String = hash.map(b => f"${b & 0xFF}%02x").mkString
    override def equals(o: Any): Boolean = o match
      case p: Pt => p.slot == slot && java.util.Arrays.equals(p.hash, hash)
      case _ => false
    override def hashCode: Int = slot.## * 31 + java.util.Arrays.hashCode(hash)
    override def toString: String = s"Pt($slot, $hex)"

  final case class Tip(point: Option[Pt], blockNo: Long)

  private def putPoint(o: Cbor.Out, p: Option[Pt]): Unit = p match
    case None => o.arrayHeader(0)
    case Some(Pt(slot, h)) => o.arrayHeader(2); o.integer(slot); o.byteString(h)

  private def point(v: Cv): Either[String, Option[Pt]] = v match
    case Cv.Arr(Vector()) => Right(None)
    case Cv.Arr(Vector(Cv.UInt(s), Cv.Bytes(h))) => Right(Some(Pt(s.toLong, h)))
    case other => Left(s"not a point: $other")

  private def tip(v: Cv): Either[String, Tip] = v match
    case Cv.Arr(Vector(p, Cv.UInt(n))) => point(p).map(Tip(_, n.toLong))
    case other => Left(s"not a tip: $other")

  private def out(f: Cbor.Out => Unit): Array[Byte] = { val o = Cbor.Out(); f(o); o.toArray }

  // ---- handshake ------------------------------------------------------

  /** propose versions 14..16 for `magic`, as an initiator-only reader
   * that shares no peers and is not querying */
  def proposeVersions(magic: Long): Array[Byte] = out { o =>
    o.arrayHeader(2); o.integer(0)
    o.mapHeader(3)
    for v <- 14 to 15 do
      o.integer(v); o.arrayHeader(4); o.integer(magic); o.bool(true); o.integer(0); o.bool(false)
    o.integer(16); o.arrayHeader(5); o.integer(magic); o.bool(true); o.integer(0); o.bool(false); o.bool(false)
  }

  enum Handshaken:
    case Accepted(version: Int, magic: Long)
    case Refused(reason: String)

  def handshaken(v: Cv): Either[String, Handshaken] = v match
    case Cv.Arr(Vector(Cv.UInt(t), Cv.UInt(ver), Cv.Arr(data))) if t == 1 =>
      data.headOption match
        case Some(Cv.UInt(m)) => Right(Handshaken.Accepted(ver.toInt, m.toLong))
        case _ => Left(s"accepted version $ver with unreadable data $data")
    case Cv.Arr(Cv.UInt(t) +: rest) if t == 2 => Right(Handshaken.Refused(rest.mkString(" ")))
    case other => Left(s"not a handshake answer: $other")

  // ---- chain-sync -----------------------------------------------------

  val requestNext: Array[Byte] = out { o => o.arrayHeader(1); o.integer(0) }
  val chainSyncDone: Array[Byte] = out { o => o.arrayHeader(1); o.integer(7) }
  def findIntersect(points: Seq[Option[Pt]]): Array[Byte] = out { o =>
    o.arrayHeader(2); o.integer(4); o.arrayHeader(points.size.toLong); points.foreach(putPoint(o, _))
  }

  enum Sync:
    case AwaitReply
    /** `era` is the hard-fork combinator's index (Conway = 6); `header`
     * the era's header bytes, whose Blake2b-256 IS the block hash */
    case RollForward(era: Int, header: Array[Byte], tip: Tip)
    case RollBackward(to: Option[Pt], tip: Tip)
    case IntersectFound(at: Option[Pt], tip: Tip)
    case IntersectNotFound(tip: Tip)

  def sync(v: Cv): Either[String, Sync] = v match
    case Cv.Arr(Vector(Cv.UInt(t))) if t == 1 => Right(Sync.AwaitReply)
    case Cv.Arr(Vector(Cv.UInt(t), h, tp)) if t == 2 => h match
      case Cv.Arr(Vector(Cv.UInt(era), Cv.Tag(24, Cv.Bytes(b), _))) => tip(tp).map(Sync.RollForward(era.toInt, b, _))
      case Cv.Arr(Vector(Cv.UInt(era), _)) if era == 0 =>
        Left("a Byron header: Byron is not modelled (specs/scalus.md §1), start at a Shelley-or-later point")
      case other => Left(s"not a header: $other")
    case Cv.Arr(Vector(Cv.UInt(t), p, tp)) if t == 3 => for a <- point(p); b <- tip(tp) yield Sync.RollBackward(a, b)
    case Cv.Arr(Vector(Cv.UInt(t), p, tp)) if t == 5 => for a <- point(p); b <- tip(tp) yield Sync.IntersectFound(a, b)
    case Cv.Arr(Vector(Cv.UInt(t), tp)) if t == 6 => tip(tp).map(Sync.IntersectNotFound(_))
    case other => Left(s"not a chain-sync message: $other")

  // ---- block-fetch ----------------------------------------------------

  def requestRange(from: Pt, to: Pt): Array[Byte] = out { o =>
    o.arrayHeader(3); o.integer(0); putPoint(o, Some(from)); putPoint(o, Some(to))
  }
  val clientDone: Array[Byte] = out { o => o.arrayHeader(1); o.integer(1) }

  enum Fetch:
    case StartBatch, NoBlocks, BatchDone
    /** the block as `[era, block]` bytes — scalus's `BlockFile`, whose
     * era counts Byron's boundary blocks separately (Conway = 7) */
    case Block(bytes: Array[Byte])

  def fetch(v: Cv): Either[String, Fetch] = v match
    case Cv.Arr(Vector(Cv.UInt(t))) if t == 2 => Right(Fetch.StartBatch)
    case Cv.Arr(Vector(Cv.UInt(t))) if t == 3 => Right(Fetch.NoBlocks)
    case Cv.Arr(Vector(Cv.UInt(t))) if t == 5 => Right(Fetch.BatchDone)
    case Cv.Arr(Vector(Cv.UInt(t), Cv.Tag(24, Cv.Bytes(b), _))) if t == 4 => Right(Fetch.Block(b))
    case other => Left(s"not a block-fetch message: $other")

  // ---- keep-alive -----------------------------------------------------

  def keepAlive(cookie: Int): Array[Byte] = out { o => o.arrayHeader(2); o.integer(0); o.integer(cookie & 0xFFFF) }
