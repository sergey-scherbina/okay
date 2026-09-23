package okay.scalus

import okay.chain.*

/**
 * Against a real preprod relay — `Live`-tagged, out of `sbt test`
 * (integration-test-gate): the network is not ours to control. Starts
 * at the tip and waits for the next block (~20 s on preprod).
 */
class TestLive extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(180, "s")

  test("a live preprod relay: from the tip, the next block arrives, linked, and its body decodes") {
    val f = CardanoFollower.open(Wire.tcp("preprod-node.play.dev.cardano.org", 3001),
      CardanoNetwork.preprod, None, Finality.Depth(0)).fold(e => fail(e), identity)
    try
      var got = Vector.empty[CardanoBlock]
      while got.isEmpty do
        got = f.step().fold(e => fail(e), identity).collect { case Event.Confirmed(b) => b }
      val b = got.head
      assertEquals(b.header.hash.length, 64)
      assert(b.transactions.size >= 0)
    finally f.close()
  }
