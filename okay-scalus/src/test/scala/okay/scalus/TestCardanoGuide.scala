package okay.scalus

import okay.chain.*
import okay.codec.Columns
import CardanoTables.*

/**
 * The code of docs/cardano.md, VERBATIM between the snippet markers.
 * The first snippet opens a socket, so it is compiled here and run by
 * TestLive; the others run on the recorded preprod blocks.
 */
class TestCardanoGuide extends munit.FunSuite:

  def follow(): Unit =
    // ---- snippet: follow
    CardanoFollower.open(Wire.tcp("preprod-node.play.dev.cardano.org", 3001), CardanoNetwork.preprod) match
      case Left(why) => println(s"could not follow: $why")
      case Right(follower) =>
        try
          while true do
            follower.step() match
              case Left(why) => throw IllegalStateException(why)
              case Right(events) => events.foreach {
                case Event.Confirmed(block) =>
                  val t = CardanoTables.of(block)
                  println(s"block ${block.header.blockNo}: ${t.transactions.size} txs, ${t.outputs.size} outputs")
                case Event.RolledBack(to, _) =>
                  println(s"forget everything after block ${to.height}")
              }
        finally follower.close()
    // ---- snippet ends

  private lazy val blocks: Vector[CardanoBlock] =
    val session = Session.open(Recorded.Replay(), 1).fold(fail(_), identity)
    val src = ChainSyncSource(session, CardanoNetwork.preprod, Some(Recorded.intersect))
    src.open().fold(fail(_), identity): Unit
    src.next().fold(fail(_), identity): Unit
    src.next().fold(fail(_), identity).collect { case Observed.Forward(b) => b }

  test("docs: the questions a block's tables answer, with no engine") {
    val block = blocks(3)
    // ---- snippet: tables
    val t = CardanoTables.of(block)

    val withTokens = t.outputs.filter(_.assetCount > 0).map(o => s"${o.txHash.take(8)}#${o.index}")
    val toEach     = t.outputs.groupMapReduce(_.address)(_.lovelace)(_ + _)
    val minted     = t.mints.map(m => (m.policy, m.name, m.quantity))

    // an input is a reference: resolve it against outputs you have seen
    val seen     = t.outputs.map(o => (o.txHash, o.index) -> o).toMap
    val resolved = t.inputs.flatMap(i => seen.get((i.spentTx, i.spentIndex)))
    // ---- snippet ends
    assertEquals(withTokens.size, 4)
    assertEquals(toEach.values.sum, t.outputs.map(_.lovelace).sum)
    assertEquals(minted, Vector.empty)
    assertEquals(resolved, Vector.empty)   // these inputs spend outputs from earlier blocks
  }

  test("docs: a table as engine-free columns") {
    val t = CardanoTables.of(blocks(0))
    // ---- snippet: columns
    val (fields, toRow) = Columns.table[OutputRow]
    val names = fields.map(_.name)
    // Vector(slot, blockNo, blockHash, time, txHash, index, address, lovelace,
    //        assetCount, datum, scriptRef, collateralReturn)
    val rows = t.outputs.map(toRow)
    // ---- snippet ends
    assertEquals(names.take(3), Vector("slot", "blockNo", "blockHash"))
    assertEquals(rows.size, 5)
  }

  test("docs: the chain-neutral view of the same transactions") {
    val block = blocks(0)
    // ---- snippet: ledger
    val ledger = CardanoLedger(Network.cardanoPreprod)
    val moved = block.transactions.flatMap(ledger.movements)
    val ada   = moved.filter(_.asset == ledger.ada).map(_.amount).sum
    // `from` is None on Cardano: who paid needs the UTXO set (inputs ⋈ outputs)
    // ---- snippet ends
    assert(ada > 0)
    assert(moved.forall(_.from.isEmpty))
  }
