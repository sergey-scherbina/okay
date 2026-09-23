package okay.scalus

import okay.chain.*
import okay.codec.Columns

/**
 * MAINNET, `Live`-tagged: a real relay from its tip, the next few blocks
 * through the follower and every CardanoTables table — mainnet blocks
 * carry what preprod's rarely do (scripts, redeemers, certificates,
 * mints, withdrawals), so this is where decoding meets the real mix.
 */
class TestLiveMainnet extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(300, "s")

  test("mainnet: three blocks from the tip decode, explode into every table, and fold into Columns") {
    val f = CardanoFollower.open(Wire.tcp("backbone.cardano.iog.io", 3001), CardanoNetwork.mainnet,
      None, Finality.Depth(0)).fold(e => fail(e), identity)
    try
      var blocks = Vector.empty[CardanoBlock]
      while blocks.size < 3 do
        blocks ++= f.step().fold(e => fail(e), identity).collect { case Event.Confirmed(b) => b }
      val t = blocks.map(CardanoTables.of).foldLeft(CardanoTables.Tables.empty)(_ ++ _)
      println(s"mainnet: blocks ${blocks.map(_.header.blockNo)}, txs ${t.transactions.size}, inputs ${t.inputs.size}, " +
        s"outputs ${t.outputs.size}, assets ${t.assets.size}, mints ${t.mints.size}, certs ${t.certificates.size}, " +
        s"withdrawals ${t.withdrawals.size}, redeemers ${t.redeemers.size}")
      assertEquals(t.transactions.size, blocks.map(_.transactions.size).sum)
      for tx <- t.transactions do assertEquals(Header.hex(Header.blake2b256(tx.cbor)), tx.txHash)
      def rows[A: okay.codec.Schema](xs: Vector[A]) = { val (_, w) = Columns.table[A]; xs.foreach(w) }
      rows(t.transactions); rows(t.inputs); rows(t.outputs); rows(t.assets); rows(t.mints)
      rows(t.certificates); rows(t.withdrawals); rows(t.redeemers)
    finally f.close()
  }
