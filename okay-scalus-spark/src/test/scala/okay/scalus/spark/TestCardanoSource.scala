package okay.scalus.spark

import _root_.okay.scalus.{CardanoTables, Recorded, Relays, Wire, N2N}
import _root_.okay.scalus.CardanoTables.Tables
import org.apache.spark.sql.SparkSession

/**
 * `format("cardano")` over the RECORDED preprod session (okay-scalus's
 * test resources), so the DataFrame is checked against CardanoTables
 * itself — which is checked against Koios in okay-scalus.
 */
class TestCardanoSource extends munit.FunSuite:
  override def munitIgnore: Boolean = Runtime.version().feature() == 24   // see okay-spark's TestSparkInterop

  /** the recording, then a QUIET chain (idle, as a relay at its tip is)
   * until closed — so a stream waits instead of failing */
  final class Quiet extends Wire:
    private var in = Recorded.inbound
    @volatile private var closed = false
    def read(): Wire.Read =
      if closed then Wire.Read.Closed
      else in match
        case s +: rest => in = rest; Wire.Read.Got(s)
        case _ => Thread.sleep(50); Wire.Read.Idle
    def write(s: N2N.Segment): Unit = ()
    def close(): Unit = closed = true

  Relays.register("recorded", () => Quiet())
  private val start = s"${Recorded.intersect.slot}:${Recorded.intersect.hash}:${Recorded.intersect.blockNo}"

  lazy val spark = SparkSession.builder().master("local[2]").appName("okay-cardano")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  private def read(table: String, blocks: Int = 5) = spark.read.format("cardano")
    .option("relay", "registered:recorded").option("network", "preprod")
    .option("start", start).option("confirmations", "0").option("blocks", blocks.toString)
    .option("table", table).load()

  /** the same five blocks, exploded directly */
  private lazy val direct: Tables =
    val session = _root_.okay.scalus.Session.open(Recorded.Replay(), 1).fold(e => fail(e), identity)
    val src = _root_.okay.scalus.ChainSyncSource(session, _root_.okay.scalus.CardanoNetwork.preprod, Some(Recorded.intersect))
    src.open().fold(e => fail(e), identity): Unit
    src.next().fold(e => fail(e), identity): Unit
    src.next().fold(e => fail(e), identity).collect { case _root_.okay.chain.Observed.Forward(b) => CardanoTables.of(b) }
      .foldLeft(Tables.empty)(_ ++ _)

  /**
   * The SETUP is paid before the tests, not inside the first one
   * (scalus-cardano-source-timeout, 2026-09-23): the first test used to
   * start the Spark session and replay the recorded chain (`direct`) under
   * its own 30 s budget, and timed out in a full gate at load 41–76 while
   * passing 4/4 alone. Measured on this box at load 22: the session 1.2 s,
   * the first query 1.7 s, the others under 0.6 s. The budget below is
   * ~70x the slowest measured test — room for a loaded box, not a cover
   * for a hang.
   */
  override def beforeAll(): Unit =
    spark: Unit
    direct: Unit
  override def munitTimeout = scala.concurrent.duration.Duration(120, "s")

  test("batch: the outputs table is CardanoTables' outputs, row for row") {
    val df = read("outputs")
    val got = df.select("txHash", "index", "address", "lovelace").collect()
      .map(r => (r.getString(0), r.getInt(1), r.getString(2), r.getLong(3))).toSet
    val want = direct.outputs.map(o => (o.txHash, o.index, o.address, o.lovelace)).toSet
    assertEquals(got, want)
    assertEquals(got.size, 11)
  }

  test("batch: SQL over a sum column — the outputs whose datum is inline") {
    read("outputs").createOrReplaceTempView("outputs")
    val n = spark.sql("SELECT count(*) FROM outputs WHERE datum.kind = 'Inline'").collect().head.getLong(0)
    assertEquals(n, direct.outputs.count(_.datum.exists(_.isInstanceOf[scalus.cardano.ledger.DatumOption.Inline])).toLong)
    assert(n > 0)
    // the JSON path the guide uses reads every real inline datum's constructor
    val ctors = spark.sql("SELECT variant_get(datum.Inline.data.json, '$.Constr.constr', 'string') FROM outputs WHERE datum.kind = 'Inline'")
      .collect().map(_.getString(0)).toList
    assertEquals(ctors.size.toLong, n)
    assert(ctors.forall(c => c != null && c.forall(_.isDigit)), ctors)
  }

  test("an unknown table is refused with the names that exist") {
    val e = intercept[IllegalArgumentException](read("nope").schema)
    assert(e.getMessage.contains("outputs"), e.getMessage)
  }

  test("stream: confirmations=2 reads the first three blocks' transactions, and waits on a quiet chain") {
    val q = spark.readStream.format("cardano")
      .option("relay", "registered:recorded").option("network", "preprod")
      .option("start", start).option("confirmations", "2").option("table", "transactions").load()
      .writeStream.format("memory").queryName("cardano_txs").start()
    try
      q.processAllAvailable()
      val got = spark.sql("SELECT blockNo, txHash FROM cardano_txs").collect().map(r => (r.getLong(0), r.getString(1))).toSet
      val want = direct.transactions.filter(_.blockNo <= Recorded.blockNos(2)).map(t => (t.blockNo, t.txHash)).toSet
      assertEquals(got, want)
      assertEquals(got.size, 2)
    finally q.stop()
  }
