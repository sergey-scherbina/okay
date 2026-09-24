package okay.cluster

import okay.{Aggregator, Pane}
import okay.codec.{Codecs, Schema}
import okay.given
import okay.persist.{Ack, MemoryStore, Policy, Topic}

/**
 * EXACTLY-ONCE FROM LOG TO LOG (specs/dataflow.md, stage 11).
 *
 * `TestStaged` closed stage 9's commit window with a two-phase writer,
 * and proved the LOGIC: a successor asked to redo an epoch the writer
 * had already applied drops it. What it could not prove is that the
 * knowledge SURVIVES — its `applied` counter is a field of an object
 * in this JVM, so the simulated death never took it away, and a real
 * coordinator's death takes everything.
 *
 * So: the writer's store is a TOPIC, and the dedup state lives IN THE
 * OUTPUT rather than beside it. A resume builds a FRESH writer with no
 * memory at all — which is what a new process is — and `recovered`
 * learns what already landed by reading the topic's tail. The writer's
 * atomicity is the log's append, which is the one thing a log gives
 * that a cell does not.
 */
object PaneLog {
  given Schema[(Long, Int, Long)] = Schema.derived
  val codec = Codecs.cbor(summon[Schema[(Long, Int, Long)]])
  /** ONE EPOCH, ONE RECORD — see `Writer.move` for why the batch and
   * not the pane is the unit that crosses */
  val batch = Codecs.cbor(Schema.SVector(() => summon[Schema[(Long, Int, Long)]]))

  /**
   * THE OUTPUT TOPIC, REMADE PER SCENARIO — and it is a `Topic` and
   * not a `MemoryStore` topic, so the same battery runs against a
   * REAL log. `okay-kafka`'s `TestDataflowKafka` passes a
   * `KafkaStore` one and asserts the same things (stage 11's last
   * box); nothing else here changes, which is the point of the seam.
   */
  /** kill the writer at this epoch — the death stage 9's other cases
   * do not cover, since they kill the COORDINATOR. Two points, which
   * is all there are: before its one append and after it. */
  @volatile var dieInside: Option[Int] = None
  @volatile var dieAfter: Option[Int] = None

  @volatile private var out: Topic | Null = null
  def topic: Topic = out.nn
  def fresh(): Unit = fresh(MemoryStore().topic("panes", 1, Policy(compact = false)))
  def fresh(t: Topic): Unit = out = t

  /** every record in the output, with the epoch that wrote it */
  def rows: Vector[(Int, (Long, Int), Long)] =
    var from = topic.begin(0)
    val acc = Vector.newBuilder[(Int, (Long, Int), Long)]
    var going = true
    while going do
      topic.read(0, from, 256) match
        case Topic.Read.TooEarly(b) => from = b
        case Topic.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            rs.foreach: r =>
              from = r.offset + 1
              val epoch = new String(r.key, "UTF-8").toInt
              batch.decode(r.value).fold(e => sys.error(e), identity).foreach: (start, key, value) =>
                acc += ((epoch, (start, key), value))
    acc.result()

  /** how many RECORDS the output holds — one per epoch, which is what
   * makes `recover()`'s rule sound */
  def records: Int =
    var from = topic.begin(0)
    var n = 0
    var going = true
    while going do
      topic.read(0, from, 256) match
        case Topic.Read.TooEarly(b) => from = b
        case Topic.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else { n += rs.length; from = rs.last.offset + 1 }
    n

  /** the answer the output holds, as a map — and it is a map ONLY if
   * nothing was written twice, which `once` checks separately */
  def answer: Map[(Long, Int), Long] = rows.map(r => r._2 -> r._3).toMap

  /**
   * A WRITER WITH NO MEMORY OF ITS OWN — the whole point. Built fresh
   * per process, it learns what already landed from the topic, so a
   * death that takes the process takes nothing it needed.
   */
  final class Writer extends Serializable:
    /** the highest epoch already in the output, read once at recovery */
    private var landed: Int = 0
    private var appends = 0L
    private var dropped = 0L

    def move(epoch: Int, panes: Vector[Pane[Int, Long]]): Unit = synchronized:
      if epoch <= landed then dropped += 1
      else
        // ONE APPEND PER EPOCH, and that is the whole of the fix
        // (specs/dataflow.md, stage 9's last box). The first version
        // appended one record per PANE and said "the log's append is
        // the atomicity a two-phase writer needs" — which was true of
        // each record and false of the batch. A writer that died
        // three panes into an epoch left an epoch that LOOKED
        // complete to `recover()`, whose rule is "the highest epoch
        // in the output"; the successor dropped the re-move as a
        // duplicate and 93 panes of 3 204 were never written, with
        // the run reporting all 3 204. Measured, not feared.
        //
        // A batch in ONE record cannot be half-written: the append
        // either happened or it did not, which is exactly the
        // atomicity the comment always claimed.
        //
        // WHAT THIS COSTS, said rather than discovered: an epoch must
        // FIT in one record. Here the largest is about 64 KB (3 204
        // panes over eleven epochs), well under Kafka's 1 MB default;
        // a job whose epoch does not fit needs either chunking with a
        // completion marker per epoch — and then a reader that
        // ignores an unmarked tail — or a transactional writer, which
        // the Kafka interop has and `TestKafkaEos` exercises. Neither
        // is built, because nothing here has an epoch that big.
        // THE ONLY TWO PLACES THIS WRITER CAN DIE, and that is the
        // fix: before the append, or after it. There is no third,
        // because the epoch is ONE record.
        if dieInside.contains(epoch) then
          dieInside = None
          throw IllegalStateException(s"the writer died before appending epoch $epoch")
        topic.append(0, epoch.toString.getBytes("UTF-8"),
          batch.encode(panes.map(p => (p.start, p.key, p.value))), Ack.Durable): Unit
        appends += 1
        if dieAfter.contains(epoch) then
          dieAfter = None
          throw IllegalStateException(s"the writer died after appending epoch $epoch")
        landed = epoch

    /** what a new process does before it starts: read the tail and
     * find out what the last one managed to write */
    def recover(): Unit = synchronized:
      landed = rows.foldLeft(0)((m, r) => math.max(m, r._1))

    def appended: Long = synchronized(appends)
    def skipped: Long = synchronized(dropped)
}

/** the tumbling job of `TestStaged`, writing its panes to a topic */
object PaneLogJob extends Job[Feed, Long] {
  import Feeds.*
  @volatile var writer: PaneLog.Writer = PaneLog.Writer()
  type A = Ev
  def name: String = "test.panelog"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def answer: Schema[Long] = summon[Schema[Long]]
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
  def sink(f: Feed): Wire[Ev, Long] =
    Wire.tumblingStaged(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(
      (epoch, panes) => writer.move(epoch, panes))
}

/**
 * THE BATTERY, over whatever `Topic` the suite supplies — so the
 * memory run and the Kafka run assert the same things rather than two
 * things that look alike (specs/dataflow.md, stage 11).
 */
trait StagingTopicSuite extends munit.FunSuite {
  import Feeds.*

  /** a fresh, empty output topic for one scenario */
  def output(): Topic

  TestJobs.install()
  Jobs.register(PaneLogJob)
  val feed: Feed = Feed(20000, Late - 1)

  val collect: Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]] =
    Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]](
      Vector.empty)((b, p) => b :+ ((p.start, p.key) -> p.value))((a, b) => a ++ b)(identity)

  lazy val panes: Map[(Long, Int), Long] =
    Flows.fan(PaneLogJob.flow(feed, 8),
      Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(collect)).runWith.value.toMap

  /** `TestStaged`'s coordinator, dying on either side of the commit */
  final class Dying(at: Int, afterSaving: Boolean) extends Checkpoint:
    val kept = Checkpoint.Memory()
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      if epoch == at && !afterSaving then throw Dying.Died(epoch)
      kept.save(epoch, bytes)
      if epoch == at then throw Dying.Died(epoch)
    def latest: Option[(Int, Array[Byte])] = kept.latest

  object Dying:
    final case class Died(epoch: Int) extends RuntimeException(s"died at epoch $epoch")

  /** start a process: a writer with NO memory, which recovers from the
   * output before it is given any work */
  def newProcess(): PaneLog.Writer =
    val w = PaneLog.Writer()
    w.recover()
    PaneLogJob.writer = w
    w

  def once(): Unit =
    val keys = PaneLog.rows.map(_._2)
    assertEquals(keys.distinct.length, keys.length,
      s"a pane is in the output twice: ${keys.diff(keys.distinct).distinct.take(5)}")

  test("a quiet run writes every pane to the log, once") {
    PaneLog.fresh(output())
    val w = newProcess()
    val got = Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512).runWith
    once()
    assertEquals(PaneLog.answer, panes)
    assertEquals(got.value, panes.size.toLong)
    assertEquals(w.skipped, 0L, "an epoch was moved twice on a quiet run")
  }

  test("EXACTLY-ONCE: a death between the append and the journal commit") {
    var recovered = 0L
    for at <- Vector(2, 4, 6, 8) do
      PaneLog.fresh(output())
      val first = newProcess()
      val j = Dying(at, afterSaving = false)
      val _ = intercept[Dying.Died](
        Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512, j).runWith)
      assertEquals(j.latest.map(_._1), Some(at - 1), s"the run never reached epoch $at")
      val wrote = first.appended

      // THE PROCESS IS GONE. A new one keeps nothing of it — not the
      // counter, not the high-water mark — and learns what landed by
      // reading the output
      val second = newProcess()
      val got = Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512, j.kept).runWith

      once()
      assertEquals(PaneLog.answer, panes, s"died before committing epoch $at")
      assertEquals(got.value, panes.size.toLong, s"died before committing epoch $at")
      recovered += second.skipped
      assert(wrote > 0, s"the first process wrote nothing before dying at $at")
    // and the mechanism was EXERCISED rather than merely available:
    // some successor WAS asked to redo an epoch its predecessor had
    // already appended, and knew so from the log alone
    assert(recovered > 0,
      "four deaths inside the window and no successor ever recognised a landed epoch — " +
        "this test is not exercising the thing it exists for")
  }

  test("AN EPOCH IS ONE RECORD, so a writer cannot leave half of one") {
    // specs/dataflow.md stage 9's last box, and the whole of the fix.
    // `recover()` learns what landed from the output, by the HIGHEST
    // EPOCH in it — a rule that is only sound if an epoch is in the
    // output entirely or not at all. The first version of this writer
    // appended one record per PANE and said "the log's append is the
    // atomicity a two-phase writer needs", which was true of each
    // record and false of the batch: measured, a writer dying three
    // panes into epoch 4 left an epoch that LOOKED complete, the
    // successor dropped the re-move as a duplicate, and 93 panes of
    // 3 204 were never written while the run reported all 3 204.
    //
    // One append per epoch removes the state rather than detecting
    // it. This is the assertion that keeps it that way.
    PaneLog.fresh(output())
    val _ = newProcess()
    val got = Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512).runWith
    assertEquals(got.value, panes.size.toLong)
    val epochs = PaneLog.rows.map(_._1).distinct
    assertEquals(PaneLog.records, epochs.length,
      s"${PaneLog.records} records for ${epochs.length} epochs: an epoch is no longer one record, " +
        "so a death inside `move` can leave half of one")
  }

  test("THE WRITER ITSELF DIES, and the successor is right either side of the append") {
    // the death stage 9's other cases do not cover: they kill the
    // COORDINATOR, and this kills the writer. Both points are
    // exercised because they are the only two there are.
    for (before, at) <- Vector((true, 4), (false, 4)) do
      PaneLog.fresh(output())
      val first = newProcess()
      if before then PaneLog.dieInside = Some(at) else PaneLog.dieAfter = Some(at)
      val died = try
        val _ = Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512).runWith
        false
      catch case _: Throwable => true
      assert(died, s"the writer never died (before=$before) — nothing is being exercised")
      val _ = first

      // a NEW process, no memory of its own, recovering from the output
      val second = newProcess()
      val got = Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512).runWith
      once()
      assertEquals(PaneLog.answer, panes, s"a writer death before=$before lost or doubled panes")
      assertEquals(got.value, panes.size.toLong)
      val _ = second
  }

  test("a death AFTER the commit: the successor starts past it and appends nothing again") {
    for at <- Vector(2, 4, 6) do
      PaneLog.fresh(output())
      val _ = newProcess()
      val j = Dying(at, afterSaving = true)
      val _ = intercept[Dying.Died](
        Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512, j).runWith)
      val second = newProcess()
      val got = Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512, j.kept).runWith
      once()
      assertEquals(PaneLog.answer, panes, s"died after committing epoch $at")
      assertEquals(got.value, panes.size.toLong)
      val _ = second
  }

  test("THE DEDUP STATE IS THE OUTPUT: a fresh writer over a filled log knows the epoch") {
    // the property `TestStaged` could not have, stated alone: build a
    // writer that has never run, over a log somebody else filled, and
    // it refuses the epochs already there
    PaneLog.fresh(output())
    val _ = newProcess()
    val _ = Cluster.stream(PaneLogJob, feed, 4, Vector(Cluster.local), 512).runWith
    val before = PaneLog.rows.length

    val stranger = PaneLog.Writer()
    stranger.recover()
    stranger.move(1, Vector(Pane(0L, Size, 1, 1L)))
    assertEquals(PaneLog.rows.length, before, "a landed epoch was appended again")
    assertEquals(stranger.skipped, 1L)
    // and an epoch ABOVE the high-water is taken
    val high = PaneLog.rows.map(_._1).max
    stranger.move(high + 1, Vector(Pane(0L, Size, 99, 7L)))
    assertEquals(PaneLog.rows.length, before + 1)
  }
}

/** the battery on a memory log — the one that runs in every gate */
class TestStagingTopic extends StagingTopicSuite {
  def output(): Topic = MemoryStore().topic("panes", 1, Policy(compact = false))
}
