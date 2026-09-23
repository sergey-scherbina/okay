package okay.scalus

import okay.chain.Observed
import okay.codec.{Columns, Json}
import CardanoTables.*

/**
 * The explode of real preprod blocks, checked against what KOIOS says
 * about the same transactions (src/test/resources/n2n/koios-txs.json,
 * fetched 2026-09-23 beside the recorded session): inputs as sets
 * (the body keeps them SORTED, Koios in its own order), outputs by
 * index, assets per output.
 */
class TestCardanoTables extends munit.FunSuite:

  private lazy val blocks: Vector[CardanoBlock] =
    val session = Session.open(Recorded.Replay(), 1).fold(fail(_), identity)
    val src = ChainSyncSource(session, CardanoNetwork.preprod, Some(Recorded.intersect))
    src.open().fold(fail(_), identity): Unit
    src.next().fold(fail(_), identity): Unit
    src.next().fold(fail(_), identity).collect { case Observed.Forward(b) => b }

  private lazy val tables: Tables = blocks.map(CardanoTables.of).foldLeft(Tables.empty)(_ ++ _)

  private def field(j: Json, k: String): Json = j match
    case Json.JObj(fs) => fs.collectFirst { case (`k`, v) => v }.getOrElse(Json.JNull)
    case _ => Json.JNull
  private def str(j: Json): String = j match { case Json.JStr(s) => s; case Json.JNum(n) => n.toLong.toString; case o => fail(s"$o") }
  private def arr(j: Json): Vector[Json] = j match { case Json.JArr(v) => v; case _ => Vector.empty }

  private lazy val koios: Vector[Json] =
    val src = scala.io.Source.fromResource("n2n/koios-txs.json")
    try arr(Json.parse(src.mkString)) finally src.close()

  test("one block row per block, heights and hashes the chain's") {
    assertEquals(tables.blocks.map(_.blockNo), Recorded.blockNos)
    assertEquals(tables.blocks.map(_.blockHash), Recorded.hashes)
    assertEquals(tables.blocks.map(_.txCount), Recorded.txCounts)
    assert(tables.blocks.forall(_.era == 7), tables.blocks.map(_.era))
  }

  test("transactions: ids, block positions and fees as Koios reports; cbor hashes to the id") {
    val got = tables.transactions.map(t => (t.txHash, t.txIndex, t.fee))
    val want = koios.map(k => (str(field(k, "tx_hash")), str(field(k, "tx_block_index")).toInt, str(field(k, "fee")).toLong))
    assertEquals(got, want)
    for t <- tables.transactions do assertEquals(Header.hex(Header.blake2b256(t.cbor)), t.txHash)
    assert(tables.transactions.forall(_.valid))
  }

  test("inputs: the spent references Koios lists, as sets per transaction; no collateral or reference here") {
    for k <- koios do
      val id = str(field(k, "tx_hash"))
      val got = tables.inputs.filter(i => i.txHash == id && i.role == InputRole.Spend).map(i => (i.spentTx, i.spentIndex)).toSet
      val want = arr(field(k, "inputs")).map(i => (str(field(i, "tx_hash")), str(field(i, "tx_index")).toInt)).toSet
      assertEquals(got, want, id)
    assert(tables.inputs.forall(i => i.role == InputRole.Spend && i.spent))
  }

  test("outputs: index, address, lovelace, asset count and datum presence, per Koios") {
    for k <- koios do
      val id = str(field(k, "tx_hash"))
      val got = tables.outputs.filter(_.txHash == id).map(o => (o.index, o.address, o.lovelace, o.assetCount, o.datum.isDefined))
      val want = arr(field(k, "outputs")).map(o =>
        (str(field(o, "tx_index")).toInt, str(field(o, "address")), str(field(o, "value")).toLong,
          arr(field(o, "assets")).size, field(o, "datum_hash") != Json.JNull))
      assertEquals(got, want, id)
  }

  test("assets: one row per asset per output, policy, name and quantity per Koios") {
    for k <- koios do
      val id = str(field(k, "tx_hash"))
      val got = tables.assets.filter(_.txHash == id).map(a => (a.index, a.policy, a.name, a.quantity)).toSet
      val want = arr(field(k, "outputs")).flatMap(o => arr(field(o, "assets")).map(a =>
        (str(field(o, "tx_index")).toInt, str(field(a, "policy")), str(field(a, "name")), str(field(a, "quantity")).toLong))).toSet
      assertEquals(got, want, id)
  }

  test("every table is a Columns table, engine-free: inline datums read as their tabular shape") {
    def check[A: okay.codec.Schema](rows: Vector[A]): Unit =
      val (fields, toRow) = Columns.table[A]
      rows.foreach(r => assertEquals(toRow(r).values.size, fields.size))
    check(tables.blocks); check(tables.transactions); check(tables.inputs); check(tables.outputs)
    check(tables.assets); check(tables.mints); check(tables.certificates); check(tables.withdrawals)
    check(tables.redeemers)
    val datum = Columns.fields[OutputRow].find(_.name == "datum").get.tpe.toString
    assert(datum.contains("kind") && datum.contains("Inline") && datum.contains("cbor"), datum)
  }
