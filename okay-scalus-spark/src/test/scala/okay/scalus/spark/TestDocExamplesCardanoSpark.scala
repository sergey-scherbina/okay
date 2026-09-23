package okay.scalus.spark

import org.apache.spark.sql.SparkSession

/**
 * The Spark snippet of docs/cardano.md, VERBATIM. `load()` and `sql` on a
 * streaming DataFrame are ANALYSED, not run — so a wrong column, table
 * or JSON path fails here, without a connection to the relay the snippet
 * names.
 */
class TestDocExamplesCardanoSpark extends munit.FunSuite:
  override def munitIgnore: Boolean = Runtime.version().feature() == 24
  lazy val spark = SparkSession.builder().master("local[1]").appName("doc")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  test("docs: a live outputs stream, queried with SQL over sums and datums") {
    // ---- snippet: spark
    val outputs = spark.readStream.format("cardano")
      .option("relay", "preprod-node.play.dev.cardano.org:3001")
      .option("network", "preprod")
      .option("table", "outputs")
      .option("confirmations", "15")
      .load()
    outputs.createOrReplaceTempView("outputs")

    val withDatums = spark.sql("""
      SELECT txHash, index, address, lovelace,
             variant_get(datum.Inline.data.json, '$.Constr.constr', 'string') AS constructor
      FROM outputs
      WHERE datum.kind = 'Inline'""")
    // withDatums.writeStream.format("console").start() — one line per inline datum, as blocks confirm
    // ---- snippet ends
    assert(outputs.isStreaming)
    assertEquals(withDatums.schema.fieldNames.toList, List("txHash", "index", "address", "lovelace", "constructor"))
  }

  test("docs: rollbacks as rows (mode = events)") {
    // ---- snippet: events
    val events = spark.readStream.format("cardano")
      .option("relay", "preprod-node.play.dev.cardano.org:3001")
      .option("network", "preprod")
      .option("table", "outputs")
      .option("mode", "events")
      .option("confirmations", "0")          // every block as it arrives
      .option("journal", "/var/lib/cardano-journal")
      .load()
    // event = 'applied'     → row is set: an output of a block just applied
    // event = 'rolled_back' → rollbackTo is set: delete rows above rollbackTo.blockNo
    val applied    = events.where("event = 'applied'").select("seq", "row.txHash", "row.index", "row.lovelace")
    val rollbacks  = events.where("event = 'rolled_back'").select("seq", "rollbackTo.blockNo")
    // ---- snippet ends
    assertEquals(applied.schema.fieldNames.toList, List("seq", "txHash", "index", "lovelace"))
    assertEquals(rollbacks.schema.fieldNames.toList, List("seq", "blockNo"))
  }
