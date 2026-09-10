package okay.cluster

import okay.codec.Schema
import okay.given
import okay.persist.{Ack, Configs, MemoryStore}

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
}
