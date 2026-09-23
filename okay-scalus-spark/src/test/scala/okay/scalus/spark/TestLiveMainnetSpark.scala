package okay.scalus.spark

import org.apache.spark.sql.SparkSession

/**
 * `format("cardano")` against MAINNET, `Live`-tagged. `confirmations`
 * is 2, not 0: the first run, at 0, met a REAL mainnet tip fork within
 * four minutes (block 13977847 taken back, 2026-09-23) and confirmed
 * mode failed the query exactly as it says it will — short forks at the
 * tip are routine on mainnet, which is why the default is 15.
 */
class TestLiveMainnetSpark extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(300, "s")
  override def munitIgnore: Boolean = Runtime.version().feature() == 24

  lazy val spark = SparkSession.builder().master("local[2]").appName("okay-cardano-mainnet")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  private def read(table: String) = spark.read.format("cardano")
    .option("relay", "backbone.cardano.iog.io:3001").option("network", "mainnet")
    .option("start", "tip").option("confirmations", "2").option("blocks", "2")
    .option("table", table).load()

  test("mainnet: the outputs of two confirmed blocks, with SQL over a sum column") {
    val outputs = read("outputs").cache()
    val nOut = outputs.count()
    val nTx = outputs.select("txHash").distinct().count()
    outputs.createOrReplaceTempView("mainnet_outputs")
    val kinds = spark.sql("SELECT coalesce(datum.kind, 'none') AS k, count(*) FROM mainnet_outputs GROUP BY 1").collect()
      .map(r => r.getString(0) -> r.getLong(1)).toMap
    println(s"mainnet spark: $nOut outputs of $nTx transactions, datums $kinds")
    assert(nTx > 0 && nOut > 0)
    assertEquals(kinds.values.sum, nOut)
  }
