package okay.scalus.spark

import _root_.okay.scalus.{FakeRelay, Recorded}
import FakeRelay.Step.*
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.util.CaseInsensitiveStringMap
import scala.jdk.CollectionConverters.*

/**
 * `mode = events` against the FAKE RELAY (the recorded blocks, a
 * scripted chain that rolls back): rollbacks arrive as rows, offsets are
 * journal sequences, and a range reads the same records twice.
 */
class TestCardanoEvents extends munit.FunSuite:
  override def munitIgnore: Boolean = Runtime.version().feature() == 24

  // five blocks at depth 0, back to block 2, then 3 and 4 again
  private val script = Vector(Back(-1), Fwd(0), Fwd(1), Fwd(2), Fwd(3), Fwd(4), Back(2), Fwd(3), Fwd(4), Await)
  Relays.register("rolls-back", () => FakeRelay.Wire(script))
  private val start = s"${Recorded.intersect.slot}:${Recorded.intersect.hash}:${Recorded.intersect.blockNo}"
  private val base = Recorded.blockNos(0)

  lazy val spark = SparkSession.builder().master("local[2]").appName("okay-cardano-events")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  private def tmp(p: String) = java.nio.file.Files.createTempDirectory(p)

  test("a rollback is a row: applied 0..4, rolled_back to 2, applied 3 and 4 again — in journal order") {
    val q = spark.readStream.format("cardano")
      .option("relay", "registered:rolls-back").option("network", "preprod").option("start", start)
      .option("confirmations", "0").option("mode", "events").option("table", "blocks")
      .option("journal", tmp("cardano-journal").toString)
      .load()
      .writeStream.format("memory").queryName("cardano_events")
      .option("checkpointLocation", tmp("cardano-ckpt").toString).start()
    try
      // the whole script lands in the journal; wait for it, then drain
      var n = 0L
      val deadline = System.nanoTime() + 60_000_000_000L
      while n < 8 && System.nanoTime() < deadline do
        q.processAllAvailable()
        n = spark.sql("SELECT count(*) FROM cardano_events").collect().head.getLong(0)
      val rows = spark.sql("SELECT seq, event, rollbackTo.blockNo, row.blockNo FROM cardano_events ORDER BY seq").collect().toList
      val shape = rows.map(r =>
        if r.getString(1) == "applied" then s"+${r.getLong(3) - base}" else s"<${r.getLong(2) - base}")
      assertEquals(shape, List("+0", "+1", "+2", "+3", "+4", "<2", "+3", "+4"))
      assertEquals(rows.map(_.getLong(0)), (0L to 7L).toList)
    finally q.stop()
  }

  test("a range is read from the journal: the same records twice, whatever the chain did since") {
    val dir = tmp("cardano-journal-replay")
    val opts = CaseInsensitiveStringMap(Map("relay" -> "registered:rolls-back", "network" -> "preprod",
      "start" -> start, "confirmations" -> "0", "mode" -> "events", "table" -> "transactions").asJava)
    val kind = CardanoSource.table(opts)
    val s = EventsStream(opts, kind, dir)
    try
      val deadline = System.nanoTime() + 30_000_000_000L
      var end = SeqOffset(-1)
      while end.seq < 7 && System.nanoTime() < deadline do
        end = s.latestOffset(SeqOffset(-1), s.getDefaultReadLimit).asInstanceOf[SeqOffset]
        Thread.sleep(50)
      // by CONTENT: a partition holds byte arrays, whose toString is identity
      def content(ps: Vector[org.apache.spark.sql.connector.read.InputPartition]) =
        ps.flatMap { case EventsPartition(es) => es; case _ => Vector.empty }.map {
          case (seq, Journaled.Applied(c)) => (seq, "applied", c.block.header.blockNo, _root_.okay.scalus.Header.hex(_root_.okay.scalus.Header.blake2b256(c.body)))
          case (seq, Journaled.RolledBack(no, h)) => (seq, "rolled_back", no, h)
        }
      val first = content(s.planInputPartitions(SeqOffset(-1), end).toVector)
      val again = content(s.planInputPartitions(SeqOffset(-1), end).toVector)
      assertEquals(again, first)
      assertEquals(first.map(_._1), (0L to 7L).toVector)
      assertEquals(first.map(_._2).count(_ == "rolled_back"), 1)
    finally s.stop()
  }

  test("a restarted stream resumes after the last block STILL STANDING (the journal replayed, rollback applied)") {
    val t = Journal.topic(tmp("cardano-journal-resume"))
    val blocks = FakeRelay.parsed.zip(FakeRelay.bodies).map((h, b) => Carried(h.era, h.bytes, b, None))
    for i <- 0 to 4 do Journal.append(t, Journaled.Applied(blocks(i))): Unit
    Journal.append(t, Journaled.RolledBack(FakeRelay.parsed(2).blockNo, FakeRelay.parsed(2).hash)): Unit
    val at = Journal.resume(t).get
    assertEquals((at.blockNo, at.hash), (FakeRelay.parsed(2).blockNo, FakeRelay.parsed(2).hash))
  }
