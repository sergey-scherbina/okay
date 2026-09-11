package okay.cluster

import okay.codec.Schema
import okay.given
import okay.persist.{Ack, Configs, Election, MemoryStore, Policy}
import okay.{Aggregator, Pane}
import scala.collection.mutable

/**
 * A STORE OF ITS OWN, for the same reason `GhostStore` is not
 * `TestOnce`'s: two suites in one JVM may run at the same time, and a
 * shared map would make each one's assertions depend on the other's
 * timing.
 */
object GhostStore {
  private val rows = mutable.HashMap.empty[(Long, Int), Long]
  private var offers = 0L
  def write(p: Pane[Int, Long]): Unit = synchronized {
    offers += 1
    rows.update((p.start, p.key), p.value)
  }
  def reset(): Unit = synchronized { rows.clear(); offers = 0L }
  def snapshot: Map[(Long, Int), Long] = synchronized(rows.toMap)
  def offered: Long = synchronized(offers)
}

object GhostJob extends Job[Feed, Long] {
  import Feeds.*
  type A = Ev
  def name: String = "test.write.ghost"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
  def sink(f: Feed): Wire[Ev, Long] =
    Wire.tumblingTo(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(GhostStore.write)
}

/**
 * THE JOURNAL, ON THE REAL LOG (specs/dataflow.md, stage 8).
 *
 * `Checkpoint` is two methods over bytes on purpose: okay-cluster's
 * compile graph stays at okay-codec and the STORE is the caller's.
 * This is the proof that the seam is the right shape — okay-persist's
 * compacted keyed topic already answers "the latest value under a
 * name", which is what a coordinator's journal is, and the adapter
 * below is eight lines.
 *
 * okay-persist is a TEST-scope dependency for the same reason
 * okay-persist itself takes okay-tls in test scope: showing that a
 * seam binds is not the same as making every user carry the binding.
 */
class TestPersisted extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  val feed: Feed = Feed(20000, Late - 1)

  final case class Saved(epoch: Int, state: Array[Byte]) derives Schema

  /** a coordinator's journal, as a compacted keyed topic */
  final class Logged(val configs: Configs, name: String) extends Checkpoint:
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      configs.put(name, Saved(epoch, bytes), Ack.Durable): Unit
    def latest: Option[(Int, Array[Byte])] =
      configs.latest[Saved](name).map { (_, got) =>
        val s = got.fold(why => throw IllegalStateException(s"the journal: $why"), identity)
        (s.epoch, s.state)
      }

  def journal(): Logged = Logged(Configs(MemoryStore()), "coordinator")

  lazy val batch: Run[((Sum, Sum), Sum)] =
    Flows.fan(FanJob.flow(feed, 8), FanJob.sink(feed)).runWith

  Jobs.register(GhostJob)

  val collect: Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]] =
    Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]](
      Vector.empty)((b, p) => b :+ ((p.start, p.key) -> p.value))((a, b) => a ++ b)(identity)

  lazy val panes: Map[(Long, Int), Long] =
    Flows.fan(GhostJob.flow(feed, 8),
      Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(collect)).runWith.value.toMap

  test("a stream journalled into okay-persist answers what the batch run answers") {
    val j = journal()
    val got = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 512, j).runWith
    assertEquals(got.value, batch.value)
    assertEquals(got.dropped, batch.dropped)
    assert(j.latest.isDefined, "nothing reached the log")
  }

  test("A COORDINATOR DIES AND THE LOG BRINGS THE NEXT ONE UP TO DATE") {
    val j = journal()
    val dying: Checkpoint = new Checkpoint:
      def save(epoch: Int, bytes: Array[Byte]): Unit =
        j.save(epoch, bytes)
        if epoch == 4 then throw RuntimeException("the coordinator died at epoch 4")
      def latest: Option[(Int, Array[Byte])] = j.latest

    val e = intercept[RuntimeException](
      Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 512, dying).runWith)
    assert(e.getMessage.contains("died at epoch 4"), e.getMessage)
    assertEquals(j.latest.map(_._1), Some(4), "epoch 4 never reached the log")

    // a NEW coordinator, given nothing but the log
    val got = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 512, j).runWith
    assertEquals(got.value, batch.value)
    assertEquals(got.dropped, batch.dropped)
  }

  test("the log keeps the HISTORY of the fold, so a run can be read back") {
    // not a property of the engine — of the store it was handed, and
    // worth pinning because it is what a LOG buys over a cell: every
    // epoch's state is still there, oldest first, until compaction
    // reclaims it
    val j = journal()
    val _ = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 512, j).runWith
    val history = j.configs.history[Saved]("coordinator")
    assert(history.length > 2, s"only ${history.length} epochs in the log")
    val epochs = history.map(_._2.fold(why => fail(why), _.epoch))
    assertEquals(epochs, epochs.sorted, "the log is not in epoch order")
    assertEquals(epochs.last, j.latest.get._1)
  }

  /**
   * LEADERSHIP ON THE REAL ELECTION (specs/dataflow.md, stage 10).
   *
   * `Lease` is three methods for the same reason `Checkpoint` is two:
   * so the engine can be given a real one without depending on it.
   * okay-persist's `Election` answers all three as they stand —
   * `tryTakeover` returns the epoch, which IS the fencing term;
   * `leader` says who holds it; `heartbeat` renews the lease, which
   * is exactly what a leader should be doing once per epoch anyway.
   */
  final class Elected(e: Election, partition: Int = 0) extends Lease:
    def take(): Option[Long] = e.tryTakeover(partition)
    def held(term: Long): Boolean =
      e.heartbeat()
      e.leader(partition).contains((term, e.node))
    override def release(term: Long): Unit = ()   // a lease expires; nothing to say

  def control(store: okay.persist.Store): okay.persist.Topic =
    store.topic("__election", 1, Policy(compact = false))

  test("a real election picks the coordinator, and the run answers the batch answer") {
    val e = Election(control(MemoryStore()), node = "a")
    val got = Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512, journal(), Elected(e)).runWith
    assertEquals(got.map(_.value), Some(batch.value))
  }

  test("TWO NODES, ONE SEAT: the second is told no, and takes over when the lease lapses") {
    val topic = control(MemoryStore())
    var now = 1000L
    val clock = () => now
    val a = Election(topic, node = "a", leaseMillis = 100, skewMillis = 10, clock = clock)
    val b = Election(topic, node = "b", leaseMillis = 100, skewMillis = 10, clock = clock)
    val log = journal()

    // a leads and dies at epoch 3
    val dying: Checkpoint = new Checkpoint:
      def save(epoch: Int, bytes: Array[Byte]): Unit =
        log.save(epoch, bytes)
        if epoch == 3 then throw RuntimeException("node a died at epoch 3")
      def latest: Option[(Int, Array[Byte])] = log.latest

    val died = intercept[RuntimeException](
      Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512, dying, Elected(a)).runWith)
    assert(died.getMessage.contains("node a died"), died.getMessage)
    assertEquals(log.latest.map(_._1), Some(3))

    // b cannot take the seat while a's lease is live
    assertEquals(Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512, log, Elected(b))
      .runWith, None, "b took a live lease")

    // the lease lapses (nobody is heartbeating a any more) and b leads
    now += 1000L
    val got = Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512, log, Elected(b)).runWith
    assertEquals(got.map(_.value), Some(batch.value))
    assertEquals(got.map(_.dropped), Some(batch.dropped))
  }

  // -----------------------------------------------------------------
  // dataflow-durable — what a journal that keeps its history buys
  // -----------------------------------------------------------------

  val foldedCodec = okay.codec.Codecs.cbor(summon[Schema[Folded]])
  def decode(bytes: Array[Byte]): Folded =
    foldedCodec.decode(bytes).fold(why => fail(why), identity)

  /**
   * The same log, read two ways.
   *
   * `defended` takes the record with the highest (term, epoch) out of
   * everything the log still holds; the other takes the last thing
   * written, which is what a single CELL would give. A stale commit
   * from a deposed leader is a record like any other, so the
   * difference between the two readers is the whole of what the
   * history buys.
   */
  final class Journal(val configs: Configs, name: String, defended: Boolean) extends Checkpoint:
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      configs.put(name, Saved(epoch, bytes), Ack.Durable): Unit
    def records: Vector[Saved] =
      configs.history[Saved](name).map(_._2.fold(why => fail(why), identity))
    def latest: Option[(Int, Array[Byte])] =
      val all = records
      if !defended then all.lastOption.map(s => (s.epoch, s.state))
      else Checkpoint.newest(all.map(s => decode(s.state))).map(f => (f.epoch, foldedCodec.encode(f)))

  test("newest takes the later TERM, and the later epoch inside a term") {
    def rec(term: Long, epoch: Int) =
      Folded(epoch, Vector.empty, 0L, 0L, Array.emptyByteArray, 7L, term)
    assertEquals(Checkpoint.newest(Vector.empty).map(_.epoch), None)
    // a later epoch under an OLDER term does not win: that is exactly
    // the ghost, which is further along its own dead branch
    assertEquals(Checkpoint.newest(Vector(rec(1, 9), rec(2, 3))).map(_.term), Some(2L))
    assertEquals(Checkpoint.newest(Vector(rec(2, 3), rec(2, 5), rec(1, 9))).map(_.epoch), Some(5))
  }

  test("a resume of a FINISHED run re-answers the recorded value, and writes nothing again") {
    // stage 8 claimed this and it was not true: the fresh sessions a
    // resumed run opens replay the whole source, DISCARD the panes
    // their catch-up closes and hand over only what is still open at
    // the end — so the tail panes were retired a second time out of
    // one partition's half and overwritten with a partial value, 29
    // of 3 204. A finished run records that it is finished now, and a
    // resume answers from the state instead of asking anybody.
    GhostStore.reset()
    val j = Journal(Configs(MemoryStore()), "over", defended = true)
    val first = Cluster.stream(GhostJob, feed, 4, Vector(Cluster.local), 512, j, term = 1L).runWith
    assertEquals(GhostStore.snapshot, panes)
    val offers = GhostStore.offered

    val again = Cluster.stream(GhostJob, feed, 4, Vector(Cluster.local), 512, j, term = 2L).runWith
    assertEquals(again.value, first.value, "a resume after the end answered something else")
    assertEquals(again.dropped, first.dropped)
    assertEquals(GhostStore.snapshot, panes, "a resume after the end rewrote panes, and wrongly")
    assertEquals(GhostStore.offered, offers, "a resume after the end offered panes again")
  }

  test("A GHOST THAT GOT PAST THE FENCE is shadowed by the history, not read back") {
    // the fence is a check before a write, so a leader deposed
    // between the two can land ONE stale commit. This is that commit,
    // written by hand because faking a lease would only prove the
    // fence works — and the point is what happens when it does not.
    GhostStore.reset()
    val store = MemoryStore()
    val defended = Journal(Configs(store), "coordinator", defended = true)
    val naive = Journal(defended.configs, "coordinator", defended = false)

    // leader A, term 1, dies at epoch 3
    val dying: Checkpoint = new Checkpoint:
      def save(epoch: Int, bytes: Array[Byte]): Unit =
        defended.save(epoch, bytes)
        if epoch == 3 then throw RuntimeException("A died at epoch 3")
      def latest: Option[(Int, Array[Byte])] = defended.latest
    val _ = intercept[RuntimeException](
      Cluster.stream(GhostJob, feed, 4, Vector(Cluster.local), 512, dying, term = 1L).runWith)
    val stale = defended.records.last                 // A's last state, term 1

    // leader B, term 2, finishes the job
    val b = Cluster.stream(GhostJob, feed, 4, Vector(Cluster.local), 512, defended, term = 2L).runWith
    assertEquals(b.value, panes.size.toLong)
    val offeredByB = GhostStore.offered

    // THE GHOST: A wakes and writes its stale state again
    defended.save(stale.epoch, stale.state)
    assertEquals(decode(defended.records.last.state).term, 1L, "the ghost's record is not the last one")

    // a third leader resumes. The DEFENDED reader takes B's newest
    // record and finds the stream over; the naive one takes the
    // ghost's and replays everything after epoch 3.
    val third = Cluster.stream(GhostJob, feed, 4, Vector(Cluster.local), 512, defended, term = 3L).runWith
    assertEquals(GhostStore.snapshot, panes, "the defended resume changed the rows")
    assertEquals(GhostStore.offered, offeredByB,
      "the defended resume re-offered panes, so it resumed from the ghost after all")
    assertEquals(third.value, b.value,
      "the defended resume answered something other than the run it resumed")

    // and the naive reader, on the same log, does resume from the
    // ghost. The store is NOT reset: the rows B wrote stay, and what
    // this run does is write some of them again.
    val beforeGhost = GhostStore.offered
    val fromGhost = Cluster.stream(GhostJob, feed, 4, Vector(Cluster.local), 512, naive, term = 4L).runWith
    assert(fromGhost.value > 0L,
      "the naive reader found nothing to redo — it did not resume from the ghost")
    assert(GhostStore.offered > beforeGhost,
      "resuming from the ghost cost no work at all, so nothing was redone")
    // AND THE ROWS ARE STILL RIGHT, which is the honest half of this
    // entry: a stale resume costs WORK, not correctness — as long as
    // the source replays and the writer is keyed by (window, key),
    // which is stage 6c's whole argument. The fence and the history
    // are what keep a run from paying that work, not what keep it
    // from being wrong.
    assertEquals(GhostStore.snapshot, panes,
      "resuming from the ghost changed the rows, not just the work")
  }
}
