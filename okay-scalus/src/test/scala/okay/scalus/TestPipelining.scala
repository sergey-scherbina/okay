package okay.scalus

import okay.chain.{Event, Finality}
import FakeRelay.Step.*

/**
 * Chain-sync PIPELINING (scalus-chainsync-pipelining): behind the tip,
 * the follower keeps several `RequestNext` in flight instead of one —
 * measured 2026-09-23 at ~44 ms a header, one relay round trip apiece,
 * which was half of a backfill. Counted on the wire: requests written
 * minus answers read, at its highest. The answers, their ORDER and a
 * rollback in the middle of a pipeline must read exactly as before.
 */
class TestPipelining extends munit.FunSuite:
  private val requestNext = N2N.requestNext.toSeq

  /** the fake relay, with the chain-sync requests in flight counted */
  final class Counting(script: Vector[FakeRelay.Step]) extends Wire:
    private val inner = FakeRelay.Wire(script)
    @volatile var inFlight = 0
    @volatile var most = 0
    def read(): Wire.Read =
      val r = inner.read()
      r match
        // the FINAL answer to a RequestNext: [2, header, tip] or [3, point, tip]
        // (an AwaitReply is not final, an intersection answers something else)
        case Wire.Read.Got(s) if s.protocol == N2N.ChainSync && s.payload.length > 1 &&
            (s.payload(0) & 0xFF) == 0x83 && (s.payload(1) == 2 || s.payload(1) == 3) =>
          synchronized(inFlight -= 1)
        case _ => ()
      r
    def write(s: N2N.Segment): Unit =
      if s.protocol == N2N.ChainSync && s.payload.toSeq == requestNext then
        synchronized { inFlight += 1; most = math.max(most, inFlight) }
      inner.write(s)
    def close(): Unit = inner.close()

  private def follow(w: Wire, n: Int): Vector[Event[Header]] =
    val f = CardanoFollower.headers(w, CardanoNetwork.preprod, Some(Recorded.intersect), Finality.Depth(0))
      .fold(e => fail(e), identity)
    try
      var got = Vector.empty[Event[Header]]
      while got.size < n do got ++= f.step().fold(e => fail(e), identity)
      got
    finally f.close()

  test("five blocks behind the tip: more than one request in flight, the headers in order") {
    val w = Counting(Vector(Back(-1), Fwd(0), Fwd(1), Fwd(2), Fwd(3), Fwd(4), Await))
    val got = follow(w, 5).collect { case Event.Confirmed(h) => h.hash }
    assertEquals(got, FakeRelay.parsed.map(_.hash))
    assert(w.most > 1, s"at most ${w.most} request in flight: not pipelined")
    assert(w.most <= 6, s"${w.most} in flight: more than the blocks that remain (and the opening rollback)")
  }

  test("a rollback in the middle of a pipeline reads as it did one request at a time") {
    val script = Vector(Back(-1), Fwd(0), Fwd(1), Fwd(2), Fwd(3), Fwd(4), Back(2), Fwd(3), Fwd(4), Await)
    val got = follow(Counting(script), 8).map {
      case Event.Confirmed(h) => s"+${h.blockNo}"
      case Event.RolledBack(to, _) => s"<${to.height}"
    }
    val no = FakeRelay.parsed.map(_.blockNo)
    assertEquals(got, Vector(s"+${no(0)}", s"+${no(1)}", s"+${no(2)}", s"+${no(3)}", s"+${no(4)}",
      s"<${no(2)}", s"+${no(3)}", s"+${no(4)}"))
  }
