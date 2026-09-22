package okay.chain

/** the snippets in docs/modules/okay-chain.md, VERBATIM */
class TestDocExamplesChain extends munit.FunSuite:

  final case class Blk(height: Long, hash: String, parent: String)
  given BlockOf[Blk] with
    type Tx = Nothing
    def ref(b: Blk) = BlockRef(Point(b.height, BlockId(b.hash)), BlockId(b.parent), None)
    def txs(b: Blk) = Vector.empty

  test("docs: a push source through the Tracker") {
    // ---- snippet begins
    val events = List(
        Observed.Forward(Blk(0, "a0", "g")),
        Observed.Forward(Blk(1, "a1", "a0")),        // a0 has one block on top: Confirmed
        Observed.Backward(Point(0, BlockId("a0"))),  // a1 was never said: absorbed
        Observed.Forward(Blk(1, "b1", "a0")),
        Observed.Forward(Blk(2, "b2", "b1")))        // b1 has one on top: Confirmed
      .foldLeft[Either[Broken, (Tracker[Blk], Vector[Event[Blk]])]](
        Right((Tracker[Blk](Finality.Depth(1)), Vector.empty))) { (acc, o) =>
          acc.flatMap((t, es) => t.feed(o).map((t2, more) => (t2, es ++ more))) }
      .map(_._2)
    // Right(Vector(Confirmed(Blk(0, "a0", "g")), Confirmed(Blk(1, "b1", "a0"))))
    // ---- snippet ends
    assertEquals(events, Right(Vector(Event.Confirmed(Blk(0, "a0", "g")), Event.Confirmed(Blk(1, "b1", "a0")))))
  }

  test("docs: identifiers") {
    // ---- snippet begins
    val base = Network.parse("eip155:8453")                 // Right(Network("eip155", "8453"))
    val ada  = Asset.native(Network.cardano, 1815).toString // "cip34:1-764824073/slip44:1815"
    // ---- snippet ends
    assertEquals(base, Right(Network("eip155", "8453")))
    assertEquals(ada, "cip34:1-764824073/slip44:1815")
  }
