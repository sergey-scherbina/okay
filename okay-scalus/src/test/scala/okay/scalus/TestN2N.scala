package okay.scalus

import okay.chain.*

/** the node-to-node client against a REAL recorded preprod session */
class TestN2N extends munit.FunSuite:
  import Recorded.*

  test("mux headers round-trip byte for byte on every recorded segment") {
    for s <- segments do
      val (t, r, p, len) = N2N.Segment.header(s.header)
      assertEquals(len, s.payload.length)
      assertEquals(N2N.Segment(t, r, p, s.payload).bytes.take(8).toList, s.header.toList)
  }

  test("our requests are the probe's requests, byte for byte (handshake, intersect)") {
    assertEquals(N2N.proposeVersions(1).toList, outbound(0).payload.toList)
    assertEquals(N2N.findIntersect(Seq(Some(intersect.pt))).toList, outbound(1).payload.toList)
    assertEquals(N2N.requestNext.toList, outbound(2).payload.toList)
  }

  test("the replayed session: handshake v15 on magic 1, five headers whose hashes are the chain's") {
    val wire = Replay()
    val session = Session.open(wire, 1).fold(fail(_), identity)
    val src = ChainSyncSource(session, CardanoNetwork.preprod, Some(intersect))
    val tip = src.open().fold(fail(_), identity)
    assertEquals(tip.blockNo, 5209707L)
    // the relay opens with a rollback to the intersection; it is the
    // checkpoint, so its height is known — and the Tracker absorbs a
    // rollback to its own frontier (the last test runs them together)
    val first = src.next().fold(fail(_), identity)
    assertEquals(first, Vector(Observed.Backward(intersect.point)))
    val obs = src.next().fold(fail(_), identity)
    val blocks = obs.collect { case Observed.Forward(b) => b }
    assertEquals(blocks.map(_.header.blockNo), blockNos)
    assertEquals(blocks.map(_.header.hash), hashes)
    assertEquals(blocks.head.header.prev, Some(intersect.hash))
    assert(blocks.zip(blocks.tail).forall((a, b) => b.header.prev.contains(a.header.hash)))
    assert(obs.last match { case Observed.AtTip(t) => t.point.height == 5209707L; case _ => false }, obs.last)
    // the client asked what the probe asked, in the same order
    assertEquals(wire.written.map(_.payload.toList).toVector, outbound.take(wire.written.size).map(_.payload.toList))
    assertEquals(wire.written.size, 10)   // propose, intersect, 7 next, one range
  }

  test("bodies decode through scalus: the transactions, ids and fees Koios reports") {
    val session = Session.open(Replay(), 1).fold(fail(_), identity)
    val src = ChainSyncSource(session, CardanoNetwork.preprod, Some(intersect))
    src.open().fold(fail(_), identity): Unit
    src.next().fold(fail(_), identity): Unit
    val blocks = src.next().fold(fail(_), identity).collect { case Observed.Forward(b) => b }
    assertEquals(blocks.map(_.transactions.size), txCounts)
    val ledger = CardanoLedger(Network.cardanoPreprod)
    for (i, expected) <- txs do
      val got = blocks(i).transactions.toVector.map(tx =>
        (ledger.id(tx).value, ledger.fee(tx).get.toLong, ledger.utxo(tx).get.spent.size, ledger.utxo(tx).get.created.size))
      assertEquals(got, expected)
  }

  test("through okay-chain's Tracker: Depth(2) confirms the first three, time from the slot") {
    val session = Session.open(Replay(), 1).fold(fail(_), identity)
    val src = ChainSyncSource(session, CardanoNetwork.preprod, Some(intersect))
    src.open().fold(fail(_), identity): Unit
    val obs = src.next().fold(fail(_), identity) ++ src.next().fold(fail(_), identity)
    val t0 = Tracker[CardanoBlock](Finality.Depth(2), from = Some(intersect.point))
    val events = obs.foldLeft((t0, Vector.empty[Event[CardanoBlock]])) { case ((t, acc), o) =>
      t.feed(o).fold(b => fail(b.reason), (t2, es) => (t2, acc ++ es)) }._2
    val confirmed = events.collect { case Event.Confirmed(b) => b.header.blockNo }
    assertEquals(confirmed, blockNos.take(3))
    val first = events.collect { case Event.Confirmed(b) => b }.head
    assertEquals(summon[BlockOf[CardanoBlock]].ref(first).time, Some(scalus.cardano.ledger.SlotConfig.preprod.slotToTime(134460842L)))
  }

  test("a movement per asset per output, `from` unknown without the UTXO set") {
    val session = Session.open(Replay(), 1).fold(fail(_), identity)
    val src = ChainSyncSource(session, CardanoNetwork.preprod, Some(intersect))
    src.open().fold(fail(_), identity): Unit
    src.next().fold(fail(_), identity): Unit
    val b0 = src.next().fold(fail(_), identity).collect { case Observed.Forward(b) => b }.head
    val ledger = CardanoLedger(Network.cardanoPreprod)
    val ms = b0.transactions.toVector.flatMap(ledger.movements)
    assert(ms.nonEmpty)
    assert(ms.forall(m => m.from.isEmpty && m.to.exists(_.address.startsWith("addr_test")) && m.amount > 0), ms)
    assert(ms.exists(_.asset == ledger.ada))
  }
