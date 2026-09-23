package okay.scalus.spark

import _root_.okay.scalus.{FakeRelay, N2N, Recorded, Relays, Wire}
import FakeRelay.Step.*
import org.apache.spark.sql.SparkSession

/**
 * `fetch = executor` (specs/scalus.md stage 5) against the FAKE RELAY:
 * every connection is a fresh scripted relay, so the driver's follower
 * and each executor's range fetch are separate sessions, as on a
 * cluster. The DataFrame must equal `fetch = driver`'s row for row —
 * the same confirmed blocks, fetched by someone else.
 */
class TestExecutorFetch extends munit.FunSuite:
  override def munitIgnore: Boolean = Runtime.version().feature() == 24

  private val chain = Vector(Fwd(0), Fwd(1), Fwd(2), Fwd(3), Fwd(4), Await)
  Relays.register("fake-chain", () => FakeRelay.Wire(chain))

  /** a relay that follows the chain but has lost every block: chain-sync
   * as the fake chain, `NoBlocks` to any range request. The driver of a
   * `fetch = executor` read never block-fetches, so only the executors
   * meet the loss — decided by what is ASKED, not by connection order */
  final class Forgetful extends Wire:
    private val inner = FakeRelay.Wire(chain)
    private val extra = scala.collection.mutable.Queue.empty[N2N.Segment]
    def read(): Wire.Read = synchronized(if extra.nonEmpty then Some(extra.dequeue()) else None) match
      case Some(s) => Wire.Read.Got(s)
      case None => inner.read()
    def write(s: N2N.Segment): Unit =
      if s.protocol == N2N.BlockFetch then
        // [3] — MsgNoBlocks
        synchronized(extra.enqueue(N2N.Segment(0, true, N2N.BlockFetch, Array(0x81.toByte, 0x03)))): Unit
      else inner.write(s)
    def close(): Unit = inner.close()
  Relays.register("forgetful", () => Forgetful())

  private val start = s"${Recorded.intersect.slot}:${Recorded.intersect.hash}:${Recorded.intersect.blockNo}"

  lazy val spark = SparkSession.builder().master("local[2]").appName("okay-cardano-executor-fetch")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def beforeAll(): Unit = spark: Unit
  override def afterAll(): Unit = spark.stop()
  // measured like TestCardanoSource's (scalus-cardano-source-timeout): the
  // first query pays Spark's code generation, ~2 s on a quiet box
  override def munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private def read(table: String, fetch: String, relay: String = "fake-chain", per: Int = 2) =
    spark.read.format("cardano")
      .option("relay", s"registered:$relay").option("network", "preprod")
      .option("start", start).option("confirmations", "0").option("blocks", "5")
      .option("fetch", fetch).option("blocksPerPartition", per.toString)
      .option("table", table).load()

  private def outputs(fetch: String, per: Int = 2): Set[(String, Int, String, Long)] =
    read("outputs", fetch, per = per).select("txHash", "index", "address", "lovelace").collect()
      .map(r => (r.getString(0), r.getInt(1), r.getString(2), r.getLong(3))).toSet

  test("batch: fetch=executor reads the same rows as fetch=driver, whatever the partitioning") {
    val byDriver = outputs("driver")
    assertEquals(byDriver.size, 11)
    assertEquals(outputs("executor"), byDriver)
    assertEquals(outputs("executor", per = 1), byDriver)   // five partitions, five sessions
    assertEquals(outputs("executor", per = 5), byDriver)   // one range for all
  }

  /** a row as VALUES: a binary column prints as `[B@…` (identity), so
   * bytes become hex, through structs and arrays */
  private def value(v: Any): Any = v match
    case b: Array[Byte] => b.map(x => f"${x & 0xFF}%02x").mkString
    case r: org.apache.spark.sql.Row => r.toSeq.map(value)
    case xs: scala.collection.Seq[?] => xs.map(value)
    case other => other

  test("batch: every table, not just outputs") {
    for t <- Seq("blocks", "transactions", "inputs", "assets", "redeemers") do
      val d = read(t, "driver").collect().map(value).toSet
      assert(d.nonEmpty || t == "redeemers" || t == "assets", t)
      assertEquals(read(t, "executor").collect().map(value).toSet, d, t)
  }

  test("stream: fetch=executor, confirmations=2 — the first three blocks, as with the driver") {
    def stream(fetch: String, name: String): Set[(Long, String)] =
      val q = spark.readStream.format("cardano")
        .option("relay", "registered:fake-chain").option("network", "preprod")
        .option("start", start).option("confirmations", "2").option("fetch", fetch)
        .option("table", "transactions").load()
        .writeStream.format("memory").queryName(name).start()
      try
        q.processAllAvailable()
        spark.sql(s"SELECT blockNo, txHash FROM $name").collect().map(r => (r.getLong(0), r.getString(1))).toSet
      finally q.stop()
    val byDriver = stream("driver", "by_driver")
    assertEquals(byDriver.size, 2)
    assertEquals(stream("executor", "by_executor"), byDriver)
  }

  test("a relay that lost the range fails the task naming the blocks and the relay") {
    val e = intercept[Exception](read("outputs", "executor", relay = "forgetful").collect())
    def all(t: Throwable): List[String] = if t == null then Nil else Option(t.getMessage).toList ++ all(t.getCause)
    val msg = all(e).mkString(" | ")
    assert(msg.contains("from registered:forgetful") && msg.contains("no longer has blocks"), msg)
  }

  test("an unknown fetch is refused with the choices") {
    val e = intercept[IllegalArgumentException](read("outputs", "somewhere").collect())
    assert(e.getMessage.contains("driver, executor"), e.getMessage)
  }
