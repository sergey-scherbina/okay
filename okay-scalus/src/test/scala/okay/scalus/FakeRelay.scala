package okay.scalus

import okay.codec.Cbor

/**
 * A relay that ANSWERS: the recorded preprod headers and blocks, served
 * by a scripted chain that replies to what the client actually asks —
 * so a rollback, which a real relay cannot be made to produce on
 * demand, is a deterministic test. The bytes are the recording's; only
 * the order of events is scripted.
 *
 * Script steps (after the intersection):
 *  - `Fwd(i)`: RollForward to recorded block i (0..4)
 *  - `Back(i)`: RollBackward to recorded block i (-1 = the intersection)
 *  - `Await`: "wait" — then the chain is quiet (reads are Idle)
 */
object FakeRelay:
  enum Step:
    case Fwd(i: Int)
    case Back(i: Int)
    case Await

  /** the recording's five headers (era, bytes) and five blocks' bytes */
  lazy val headers: Vector[(Int, Array[Byte])] = recorded._1
  lazy val bodies: Vector[Array[Byte]] = recorded._2

  private lazy val recorded: (Vector[(Int, Array[Byte])], Vector[Array[Byte]]) =
    val d = N2N.Demux()
    val msgs = Recorded.inbound.map(s => (s.protocol, d.feed(s).fold(e => sys.error(e), identity))).flatMap((p, ms) => ms.map(p -> _))
    val hs = msgs.collect { case (N2N.ChainSync, Cv.Arr(Vector(Cv.UInt(t), Cv.Arr(Vector(Cv.UInt(era), Cv.Tag(24, Cv.Bytes(b), _))), _))) if t == 2 => (era.toInt, b) }
    val bs = msgs.collect { case (N2N.BlockFetch, Cv.Arr(Vector(Cv.UInt(t), Cv.Tag(24, Cv.Bytes(b), _)))) if t == 4 => b }
    (hs, bs)

  lazy val parsed: Vector[Header] = headers.map((era, b) => Header.parse(era, b).fold(e => sys.error(e), identity))

  private def out(f: Cbor.Out => Unit): Array[Byte] = { val o = Cbor.Out(); f(o); o.toArray }
  private def point(o: Cbor.Out, i: Int): Unit =
    if i < 0 then { o.arrayHeader(2); o.integer(Recorded.intersect.slot); o.byteString(Header.unhex(Recorded.intersect.hash)) }
    else { o.arrayHeader(2); o.integer(parsed(i).slot); o.byteString(Header.unhex(parsed(i).hash)) }
  private def tip(o: Cbor.Out, i: Int): Unit =
    o.arrayHeader(2); point(o, i); o.integer(if i < 0 then Recorded.intersect.blockNo else parsed(i).blockNo)
  private def tag24(o: Cbor.Out, b: Array[Byte]): Unit = { o.header(6, 24); o.byteString(b) }

  final class Wire(script: Vector[Step]) extends okay.scalus.Wire:
    private val pending = scala.collection.mutable.Queue.empty[N2N.Segment]
    private val demux = N2N.Demux()
    private var steps = script
    private val tipAt = parsed.size - 1
    @volatile private var closed = false
    private def reply(protocol: Int, bytes: Array[Byte]): Unit =
      pending += N2N.Segment(0, true, protocol, bytes)

    def read(): okay.scalus.Wire.Read =
      val r = synchronized {
        if closed then okay.scalus.Wire.Read.Closed
        else if pending.nonEmpty then okay.scalus.Wire.Read.Got(pending.dequeue())
        else okay.scalus.Wire.Read.Idle
      }
      // a quiet chain answers after an idle interval, as a socket's read
      // timeout does — not at once, which would spin the keep-alive loop
      if r == okay.scalus.Wire.Read.Idle then Thread.sleep(20)
      r

    def write(s: N2N.Segment): Unit = synchronized {
      demux.feed(s).fold(e => sys.error(e), identity).foreach(m => answer(s.protocol, m))
    }

    def close(): Unit = closed = true

    private def index(hash: Array[Byte]): Int =
      val h = Header.hex(hash)
      if h == Recorded.intersect.hash then -1 else parsed.indexWhere(_.hash == h)

    private def answer(protocol: Int, m: Cv): Unit = (protocol, m) match
      case (N2N.Handshake, _) =>
        reply(N2N.Handshake, out { o => o.arrayHeader(3); o.integer(1); o.integer(15)
          o.arrayHeader(4); o.integer(1); o.bool(true); o.integer(0); o.bool(false) })
      case (N2N.ChainSync, Cv.Arr(Vector(Cv.UInt(t), Cv.Arr(pts)))) if t == 4 =>
        pts.headOption match
          case Some(Cv.Arr(Vector(_, Cv.Bytes(h)))) =>
            val i = index(h)
            reply(N2N.ChainSync, out { o => o.arrayHeader(3); o.integer(5); point(o, i); tip(o, tipAt) })
          case _ =>   // an empty intersection: "not found", naming the tip
            reply(N2N.ChainSync, out { o => o.arrayHeader(2); o.integer(6); tip(o, tipAt) })
      case (N2N.ChainSync, Cv.Arr(Vector(Cv.UInt(t)))) if t == 0 =>
        steps match
          case Step.Fwd(i) +: rest =>
            steps = rest
            reply(N2N.ChainSync, out { o => o.arrayHeader(3); o.integer(2)
              o.arrayHeader(2); o.integer(headers(i)._1); tag24(o, headers(i)._2); tip(o, tipAt) })
          case Step.Back(i) +: rest =>
            steps = rest
            reply(N2N.ChainSync, out { o => o.arrayHeader(3); o.integer(3); point(o, i); tip(o, tipAt) })
          case Step.Await +: rest =>
            steps = rest
            reply(N2N.ChainSync, out { o => o.arrayHeader(1); o.integer(1) })
          case _ => ()   // the script is over: the chain stays quiet
      case (N2N.BlockFetch, Cv.Arr(Vector(Cv.UInt(t), Cv.Arr(Vector(_, Cv.Bytes(a))), Cv.Arr(Vector(_, Cv.Bytes(b)))))) if t == 0 =>
        val (from, to) = (index(a), index(b))
        reply(N2N.BlockFetch, out { o => o.arrayHeader(1); o.integer(2) })
        for i <- from to to do reply(N2N.BlockFetch, out { o => o.arrayHeader(2); o.integer(4); tag24(o, bodies(i)) })
        reply(N2N.BlockFetch, out { o => o.arrayHeader(1); o.integer(5) })
      case (N2N.KeepAlive, Cv.Arr(Vector(Cv.UInt(t), Cv.UInt(c)))) if t == 0 =>
        reply(N2N.KeepAlive, out { o => o.arrayHeader(2); o.integer(1); o.integer(c.toLong) })
      case _ => ()
