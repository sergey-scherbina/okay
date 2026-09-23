package okay.scalus

import okay.codec.Cbor

/**
 * A BUSY connection must still say keep-alive (scalus-executor-fetch,
 * 2026-09-23): a preprod backfill on one connection was reset by the
 * relay at 97.5 s and 98.2 s, twice, because the session pinged only
 * when the wire was IDLE — and a connection streaming blocks is never
 * idle. The protocol also allows ONE ping in flight: the next waits
 * for the answer.
 */
class TestKeepAlive extends munit.FunSuite:
  private def out(f: Cbor.Out => Unit): Array[Byte] = { val o = Cbor.Out(); f(o); o.toArray }
  private val accept = out { o => o.arrayHeader(3); o.integer(1); o.integer(15)
    o.arrayHeader(4); o.integer(1); o.bool(true); o.integer(0); o.bool(false) }
  private val awaitReply = out { o => o.arrayHeader(1); o.integer(1) }          // chain-sync chatter
  private val batchDone = out { o => o.arrayHeader(1); o.integer(5) }           // the block-fetch answer
  private val pong = out { o => o.arrayHeader(2); o.integer(1); o.integer(1) }  // MsgKeepAliveResponse

  /** never idle: every read is a segment, and each read is 5 s of clock */
  final class Busy(var clock: Long) extends Wire:
    val written = scala.collection.mutable.ArrayBuffer.empty[N2N.Segment]
    val script = scala.collection.mutable.Queue.empty[(Int, Array[Byte])]
    def read(): Wire.Read =
      clock += 5_000_000_000L
      val (p, m) = if script.nonEmpty then script.dequeue() else (N2N.ChainSync, awaitReply)
      Wire.Read.Got(N2N.Segment(0, true, p, m))
    def write(s: N2N.Segment): Unit = written += s
    def close(): Unit = ()
    def pings: Int = written.count(_.protocol == N2N.KeepAlive)

  private def session(w: Busy): Session =
    w.script.enqueue(N2N.Handshake -> accept)
    Session.open(w, CardanoNetwork.preprod.magic, clock = () => w.clock).fold(e => fail(e), identity)

  test("a connection that is never idle still pings once the interval has passed") {
    val w = Busy(0L)
    val s = session(w)
    w.script ++= Vector.fill(10)(N2N.ChainSync -> awaitReply) :+ (N2N.BlockFetch -> batchDone)
    assert(s.receive(N2N.BlockFetch).isRight)   // 55 s of a busy wire
    assert(w.pings >= 1, s"no keep-alive in 55 s of traffic: ${w.pings}")
  }

  test("one ping in flight: the next waits for the answer, then comes") {
    val w = Busy(0L)
    val s = session(w)
    w.script ++= Vector.fill(20)(N2N.ChainSync -> awaitReply) :+ (N2N.BlockFetch -> batchDone)
    assert(s.receive(N2N.BlockFetch).isRight)   // 105 s, no answer to the ping
    assertEquals(w.pings, 1)
    w.script ++= (N2N.KeepAlive -> pong) +: Vector.fill(10)(N2N.ChainSync -> awaitReply) :+ (N2N.BlockFetch -> batchDone)
    assert(s.receive(N2N.BlockFetch).isRight)
    assertEquals(w.pings, 2)
  }
