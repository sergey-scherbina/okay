package okay.scalus.spark

import org.apache.spark.sql.SparkSession

/**
 * specs/scalus.md stage 5, MEASURED: a preprod backfill of the same
 * confirmed blocks, `fetch = driver` against `fetch = executor`, on one
 * local cluster. Arms ALTERNATE (driver, executor, driver, executor), so
 * the box's load drifts across both rather than onto one; each arm
 * counts every output, which forces every block to be fetched and
 * decoded. `Live`: a public relay, the network, a real chain.
 */
class TestLiveExecutorFetch extends munit.FunSuite:
  override def munitIgnore: Boolean = Runtime.version().feature() == 24
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")

  // preprod block 4 800 000 (Koios, 2026-09-23), ~400 000 blocks behind the tip
  private val start = "125249736:7feb537a313fbccd2ea2edf0ec8ce539a2b97231d0f5af471ae51d7a88c1a143:4800000"
  private val blocks = sys.env.getOrElse("OKAY_BACKFILL_BLOCKS", "1000")

  lazy val spark = SparkSession.builder().master("local[4]").appName("okay-cardano-backfill")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def beforeAll(): Unit = spark: Unit
  override def afterAll(): Unit = spark.stop()

  /** one arm: its output count and time — or the failure, which is a
   * finding about that arm (a public relay resets a long connection), not
   * a reason to lose the other arms' numbers */
  private def arm(fetch: String): Either[String, (Long, Long)] =
    val t0 = System.nanoTime()
    val r = scala.util.Try(spark.read.format("cardano")
      .option("relay", "preprod-node.play.dev.cardano.org:3001").option("network", "preprod")
      .option("start", start).option("confirmations", "0").option("blocks", blocks)
      .option("fetch", fetch).option("table", "outputs").load().count())
    val ms = (System.nanoTime() - t0) / 1_000_000
    val out = r.toEither.left.map(e => s"${e.getClass.getSimpleName}: ${e.getMessage}".take(200)).map(n => (n, ms))
    println(s"BACKFILL fetch=$fetch blocks=$blocks ${out.fold(e => s"FAILED after ${ms} ms: $e", (n, t) => s"outputs=$n ms=$t")}")
    out

  test("backfill: driver vs executor, alternated, same rows") {
    val runs = Seq("driver", "executor", "driver", "executor").map(arm)
    val counts = runs.collect { case Right((n, _)) => n }.distinct
    assert(counts.size <= 1, s"arms disagree on the rows: $counts")
    assert(runs.exists(_.isRight), "no arm finished")
  }

  test("the driver's share: following the same HEADERS alone, no bodies") {
    val c = CardanoSource.checkpoint(start)
    val t0 = System.nanoTime()
    val f = _root_.okay.scalus.CardanoFollower.headers(_root_.okay.scalus.Wire.tcp("preprod-node.play.dev.cardano.org", 3001),
      _root_.okay.scalus.CardanoNetwork.preprod, c, _root_.okay.chain.Finality.Depth(0)).fold(e => fail(e), identity)
    try
      var n = 0
      while n < blocks.toInt do
        n += f.step().fold(e => fail(e), identity).count { case _root_.okay.chain.Event.Confirmed(_) => true; case _ => false }
      println(s"BACKFILL headers-only blocks=$n ms=${(System.nanoTime() - t0) / 1_000_000}")
    finally f.close()
  }
