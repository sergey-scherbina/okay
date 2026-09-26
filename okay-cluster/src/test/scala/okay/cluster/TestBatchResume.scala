package okay.cluster

import okay.given
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/**
 * A BATCH RUN SURVIVES ITS COORDINATOR (specs/dataflow.md, stage 8's
 * list; batch-coordinator-resume).
 *
 * Stage 8 made a STREAM's coordinator replaceable and left the batch
 * `Cluster.run` to be restarted, which is right for a job measured in
 * seconds and wrong for one whose partitions are an hour each. Here the
 * run writes down each partition's partial as it arrives, and a second
 * run over the same journal asks only for the ones it does not hold.
 *
 * "Only" is asserted by COUNTING `Run` requests per partition, not by
 * the answer — a successor that recomputed everything would get the
 * answer right too.
 */
class TestBatchResume extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  val feed: Feed = Feed(20000, Late - 1)
  val parts = 8

  lazy val batch: Run[((Sum, Sum), Sum)] =
    Flows.fan(FanJob.flow(feed, parts), FanJob.sink(feed)).runWith

  /** a worker that writes down which partitions it was asked to RUN */
  def counting(asked: ConcurrentLinkedQueue[Int]): Cluster.Serve = req =>
    req match
      case Req.Run(_, _, i, _, _, _) => asked.add(i): Unit
      case _ => ()
    Cluster.local(req)

  /**
   * A COORDINATOR THAT DIES once `at` partials have arrived, and
   * writes nothing after: the saves already made are what a successor
   * finds, and the partitions still in flight reach nobody — which is
   * what a dead process's in-flight replies do.
   *
   * IN LOCKSTEP, because saves coalesce: left to run freely, a burst of
   * arrivals is one save, and "died after half" would mean anything
   * from nothing to everything depending on the box's load (the first
   * cut of this fixture died by save COUNT and went red under a busy
   * gate for exactly that reason). `serve` lets one `Run` through at a
   * time and the journal lets the next one through only once the
   * previous partial is written, so the record grows one partial per
   * save and the death lands at exactly `at`.
   */
  final class Dying(at: Int, kept: Checkpoint) extends Checkpoint:
    private val turn = java.util.concurrent.Semaphore(1)
    private var held = 0
    private var dead = false
    def serve(base: Cluster.Serve): Cluster.Serve = req =>
      req match
        case _: Req.Run => turn.acquire(); base(req)
        case _ => base(req)
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      val n = Partials.read(bytes).fold(0)(_.held.length)
      val die = synchronized {
        if dead then true
        else if n >= at then { dead = true; true }
        else
          kept.save(epoch, bytes)
          if n > held then { held = n; turn.release() }
          false
      }
      // a dead coordinator lets every waiting reply through, to die
      if die then { turn.release(at * 4); throw Dying.Died(at) }
    def latest: Option[(Int, Array[Byte])] = kept.latest

  object Dying:
    final case class Died(at: Int) extends RuntimeException(s"the coordinator died at its partial $at")

  def held(journal: Checkpoint): Set[Int] =
    Cluster.heldPartitions(journal).getOrElse(Set.empty)

  test("killed after half the partitions, a successor finishes with the batch answer and runs only the rest") {
    val journal = Checkpoint.Memory()
    val first = ConcurrentLinkedQueue[Int]()
    val dying = Dying(parts / 2, journal)
    val workers1 = Vector.fill(4)(dying.serve(counting(first)))
    val died = intercept[Throwable](
      Cluster.run(FanJob, feed, parts, workers1, journal = dying).runWith)
    assert(Iterator.iterate(died)(_.getCause).takeWhile(_ != null).exists(_.isInstanceOf[Dying.Died]),
      s"the run died of something else: $died")

    val kept = held(journal)
    assertEquals(kept.size, parts / 2 - 1, "the death did not land where the fixture put it")

    val second = ConcurrentLinkedQueue[Int]()
    val workers = Vector.fill(4)(counting(second))
    val got = Cluster.run(FanJob, feed, parts, workers, journal = journal).runWith
    assertEquals(got.value, batch.value)
    assertEquals(got.dropped, batch.dropped)
    assertEquals(got.merged, batch.merged)
    assertEquals(second.asScala.toVector.sorted, (0 until parts).filterNot(kept).toVector,
      s"the successor recomputed a partition the journal held ($kept)")
  }

  test("the pre-pass is journalled: a resumed run asks for no extents") {
    val journal = Checkpoint.Memory()
    val dying = Dying(2, journal)
    val _ = intercept[Throwable](Cluster.run(FanJob, feed, parts, Vector.fill(4)(dying.serve(Cluster.local)),
      journal = dying).runWith)
    val extents = ConcurrentLinkedQueue[Int]()
    val watching: Cluster.Serve = req =>
      req match
        case Req.Extent(_, _, i, _, _) => extents.add(i): Unit
        case _ => ()
      Cluster.local(req)
    val got = Cluster.run(FanJob, feed, parts, Vector.fill(4)(watching), journal = journal).runWith
    assertEquals(got.value, batch.value)
    assertEquals(extents.size, 0, "the resumed run re-asked the pre-pass")
  }

  test("a finished record starts a fresh run, and the journal says finished") {
    val journal = Checkpoint.Memory()
    val a = Cluster.run(FanJob, feed, parts, Vector(Cluster.local), journal = journal).runWith
    assertEquals(a.value, batch.value)
    assertEquals(Cluster.heldPartitions(journal), None, "a finished run still reads as resumable")
    val asked = ConcurrentLinkedQueue[Int]()
    val b = Cluster.run(FanJob, feed, parts, Vector(counting(asked)), journal = journal).runWith
    assertEquals(b.value, batch.value)
    assertEquals(asked.size, parts, "a finished run was resumed rather than started again")
  }

  test("a journal holding another run is refused by name, and left as it was") {
    val journal = Checkpoint.Memory()
    val dying = Dying(2, journal)
    val _ = intercept[Throwable](Cluster.run(FanJob, feed, parts, Vector.fill(4)(dying.serve(Cluster.local)),
      journal = dying).runWith)
    val before = journal.latest.map(_._2.toVector)
    val wider = intercept[IllegalStateException](
      Cluster.run(FanJob, feed, parts * 2, Vector(Cluster.local), journal = journal).runWith)
    assert(wider.getMessage.contains("another run"), wider.getMessage)
    val other = intercept[IllegalStateException](
      Cluster.run(FanJob, Feed(10000, Late - 1), parts, Vector(Cluster.local), journal = journal).runWith)
    assert(other.getMessage.contains("another run"), other.getMessage)
    assertEquals(journal.latest.map(_._2.toVector), before, "a refused journal was written")
  }

  test("a stream's journal is not a batch run's") {
    val journal = Checkpoint.Memory()
    val _ = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 4096, journal).runWith
    val refused = intercept[IllegalStateException](
      Cluster.run(FanJob, feed, parts, Vector(Cluster.local), journal = journal).runWith)
    assert(refused.getMessage.contains("another run"), refused.getMessage)
  }

  test("runLeading: one seat, and the answer") {
    var free = true
    val seat = new Lease:
      def take(): Option[Long] = synchronized { if free then { free = false; Some(7L) } else None }
      def held(t: Long): Boolean = synchronized(!free && t == 7L)
      override def release(t: Long): Unit = synchronized { free = true }
    val got = Cluster.runLeading(FanJob, feed, parts, Vector(Cluster.local), Checkpoint.Memory(), seat).runWith
    assertEquals(got.map(_.value), Some(batch.value))
    assert(seat.take().isDefined, "the seat was never given up")
    assertEquals(Cluster.runLeading(FanJob, feed, parts, Vector(Cluster.local), Checkpoint.Memory(), seat).runWith,
      None, "a second coordinator ran while the seat was held")
  }
}
