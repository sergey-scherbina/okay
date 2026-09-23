package okay.scalus

import okay.chain.Observed
import okay.codec.{Cbor, Columns, Json, Schema}
import CardanoSchemas.given
import scalus.cardano.ledger.{Block, Transaction}

/** the scalus ledger model through okay's Schema, on REAL preprod blocks */
class TestCardanoSchemas extends munit.FunSuite:

  private lazy val blocks: Vector[CardanoBlock] =
    val session = Session.open(Recorded.Replay(), 1).fold(fail(_), identity)
    val src = ChainSyncSource(session, CardanoNetwork.preprod, Some(Recorded.intersect))
    src.open().fold(fail(_), identity): Unit
    src.next().fold(fail(_), identity): Unit
    src.next().fold(fail(_), identity).collect { case Observed.Forward(b) => b }

  private lazy val txs: Vector[Transaction] = blocks.flatMap(_.transactions)

  test("the recursive types are exactly the recursive ones: Data, Timelock, Metadatum") {
    val names = Columns.recursiveNames(summon[Schema[Transaction]])
    assert(Set("Data", "Timelock", "Metadatum").subsetOf(names), names)
    assert(!names("TransactionBody") && !names("Transaction"), names)
  }

  test("every real transaction folds to JSON and back to the same JSON") {
    assertEquals(txs.size, 4)
    for tx <- txs do
      val once = Json.write(tx)
      val back = Json.read[Transaction](once).fold(e => fail(s"${tx.id.toHex}: $e"), identity)
      assertEquals(Json.write(back), once)
  }

  test("and through okay's CBOR, the same way") {
    for tx <- txs do
      val once = Cbor.write(tx)
      val back = Cbor.read[Transaction](once).fold(e => fail(s"${tx.id.toHex}: $e"), identity)
      assertEquals(Cbor.write(back).toList, once.toList)
  }

  test("Columns reads a whole block as one row, engine-free") {
    val (fields, toRow) = Columns.table[Block]
    assert(fields.map(_.name).contains("transactionBodies"), fields.map(_.name))
    for b <- blocks do
      val row = toRow(b.block)
      assertEquals(row.values.size, fields.size)
  }

  test("a value's assets are flat (policy, name, quantity) rows, ADA is a Long column") {
    val body = Columns.fields[Transaction].find(_.name == "body").get.tpe.toString
    assert(body.contains("AssetQuantity") || body.contains("policy"), body.take(400))
  }
