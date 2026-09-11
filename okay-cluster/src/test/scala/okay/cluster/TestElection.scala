package okay.cluster

import okay.given
import java.util.concurrent.atomic.AtomicLong

/**
 * WHO STARTS THE SUCCESSOR, AND WHAT STOPS THE PREDECESSOR
 * (specs/dataflow.md, stage 10).
 *
 * Stage 8 made a successor possible: the journal holds the fold, and
 * a second `Cluster.stream` over it picks the run up. Nobody started
 * one. And there was a second hole, the dangerous one — two
 * coordinators over one journal is worse than none, because a paused
 * leader that wakes believing it still leads commits over its
 * successor's state and the next resume reads whichever landed last.
 *
 * The tests below are in that order: taking the seat, handing it on,
 * and refusing the ghost.
 */
class TestElection extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  val feed: Feed = Feed(20000, Late - 1)

  lazy val batch: Run[((Sum, Sum), Sum)] =
    Flows.fan(FanJob.flow(feed, 8), FanJob.sink(feed)).runWith

  /**
   * A LEASE IN MEMORY: one seat, a term that only goes up.
   *
   * `depose()` is what a real lease's EXPIRY does — the holder is not
   * told, it simply stops being the leader, which is the only case
   * worth testing.
   */
  final class Seat extends Lease:
    private val term = AtomicLong(0)
    private var holder: Long = 0L
    private var free: Boolean = true
    def take(): Option[Long] = synchronized {
      if !free then None
      else
        free = false
        holder = term.incrementAndGet()
        Some(holder)
    }
    def held(t: Long): Boolean = synchronized(!free && t == holder)
    override def release(t: Long): Unit = synchronized { if t == holder then free = true }
    /** the lease lapsed: the seat is free and the holder does not know */
    def depose(): Unit = synchronized { free = true }

  /** a coordinator that dies at a chosen epoch (TestResume's) */
  final class Dying(at: Int, kept: Checkpoint) extends Checkpoint:
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      kept.save(epoch, bytes)
      if epoch == at then throw Dying.Died(epoch)
    def latest: Option[(Int, Array[Byte])] = kept.latest

  object Dying:
    final case class Died(epoch: Int) extends RuntimeException(s"died at epoch $epoch")

  test("the only candidate leads, and the answer is the batch answer") {
    val seat = Seat()
    val got = Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512,
      Checkpoint.Memory(), seat).runWith
    assertEquals(got.map(_.value), Some(batch.value))
    assertEquals(got.map(_.dropped), Some(batch.dropped))
    // and the seat is free again, so the next attempt is not blocked
    // by a run that finished
    assert(seat.take().isDefined, "the seat was never given up")
  }

  test("a second candidate is told NO rather than running beside the first") {
    val seat = Seat()
    val first = seat.take()
    assert(first.isDefined)
    val got = Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512,
      Checkpoint.Memory(), seat).runWith
    assertEquals(got, None, "two coordinators ran over one journal")
  }

  test("`Lease.solitary` is the default shape: no election, no cost") {
    val got = Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512,
      Checkpoint.none, Lease.solitary).runWith
    assertEquals(got.map(_.value), Some(batch.value))
  }

  test("A LEADER DIES AND THE NEXT CANDIDATE FINISHES THE JOB") {
    val journal = Checkpoint.Memory()
    val seat = Seat()
    // the first candidate takes the seat and dies at epoch 3
    val e = intercept[Dying.Died](
      Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512,
        Dying(3, journal), seat).runWith)
    assertEquals(e.epoch, 3)
    assertEquals(journal.latest.map(_._1), Some(3))

    // its lease lapses — nothing tells it, the seat simply frees
    seat.depose()

    val got = Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512,
      journal, seat).runWith
    assertEquals(got.map(_.value), Some(batch.value))
    assertEquals(got.map(_.dropped), Some(batch.dropped))
  }

  test("THE GHOST IS REFUSED: a deposed coordinator does not commit over its successor") {
    val journal = Checkpoint.Memory()
    val seat = Seat()

    // the first leader is deposed BETWEEN its epochs — the case that
    // matters, and the one a test of a vacant seat would never reach
    var deposed = false
    val trap: Checkpoint = new Checkpoint:
      def save(epoch: Int, bytes: Array[Byte]): Unit =
        journal.save(epoch, bytes)
        if epoch == 2 && !deposed then
          deposed = true
          seat.depose()
          val stolen = seat.take()          // the successor takes the seat
          assert(stolen.isDefined, "the successor could not take the seat")
      def latest: Option[(Int, Array[Byte])] = journal.latest

    val ghost = intercept[Checkpoint.Deposed](
      Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512, trap, seat).runWith)
    assert(ghost.getMessage.contains("no longer holds the lease"), ghost.getMessage)
    // it stopped at the epoch AFTER the one it lost the seat on: the
    // fence is checked before every commit
    assertEquals(ghost.epoch, 3)
    assertEquals(journal.latest.map(_._1), Some(2),
      "the deposed coordinator committed after losing the seat")
  }

  test("and the successor of a refused ghost still answers the batch answer") {
    val journal = Checkpoint.Memory()
    val seat = Seat()
    var deposed = false
    val trap: Checkpoint = new Checkpoint:
      def save(epoch: Int, bytes: Array[Byte]): Unit =
        journal.save(epoch, bytes)
        if epoch == 2 && !deposed then { deposed = true; seat.depose() }
      def latest: Option[(Int, Array[Byte])] = journal.latest

    val _ = intercept[Checkpoint.Deposed](
      Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512, trap, seat).runWith)

    val got = Cluster.leading(FanJob, feed, 4, Vector(Cluster.local), 512, journal, seat).runWith
    assertEquals(got.map(_.value), Some(batch.value))
    assertEquals(got.map(_.dropped), Some(batch.dropped))
  }
}
