package okay.scalus

import okay.chain.*
import FakeRelay.Step.*

/** the fake relay itself, and a rollback through the whole client */
class TestFakeRelay extends munit.FunSuite:

  test("the fake relay serves the recording: five blocks, the chain's hashes") {
    val f = CardanoFollower.open(FakeRelay.Wire(Vector(Back(-1), Fwd(0), Fwd(1), Fwd(2), Fwd(3), Fwd(4), Await)),
      CardanoNetwork.preprod, Some(Recorded.intersect), Finality.Depth(0)).fold(fail(_), identity)
    val es = f.step().fold(fail(_), identity) ++ f.step().fold(fail(_), identity)
    assertEquals(es.collect { case Event.Confirmed(b) => b.header.hash }, Recorded.hashes)
  }

  test("a rollback past confirmed blocks is RolledBack, then the chain is followed again") {
    // 0..4 confirmed at depth 0, back to block 2, then 3 and 4 again
    val f = CardanoFollower.open(FakeRelay.Wire(Vector(Back(-1), Fwd(0), Fwd(1), Fwd(2), Fwd(3), Fwd(4), Back(2), Fwd(3), Fwd(4), Await)),
      CardanoNetwork.preprod, Some(Recorded.intersect), Finality.Depth(0)).fold(fail(_), identity)
    var events = Vector.empty[Event[CardanoBlock]]
    var steps = 0
    while events.size < 8 && steps < 20 do
      events ++= f.step().fold(fail(_), identity); steps += 1
    val shape = events.map {
      case Event.Confirmed(b) => s"+${b.header.blockNo - Recorded.blockNos(0)}"
      case Event.RolledBack(to, _) => s"<${to.height - Recorded.blockNos(0)}"
    }
    assertEquals(shape, Vector("+0", "+1", "+2", "+3", "+4", "<2", "+3", "+4"))
  }
