package okay.cluster

import okay.{Aggregator, Chunks, given}
import okay.codec.Schema
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

/** a source whose partitions hold something: every open and close is
 * recorded, and so is the position each partition was opened at */
object OpenedJob extends Job[Int, Long] {
  val opens = ConcurrentLinkedQueue[(Int, Long)]()
  val closes = AtomicInteger(0)
  type A = Long
  def name: String = "test.opened"
  def params: Schema[Int] = summon[Schema[Int]]
  def answer: Schema[Long] = summon[Schema[Long]]
  def flow(n: Int, parts: Int): Flow[Long] =
    Flow.opened(parts) { (i, start, scope) =>
      opens.add((i, start)): Unit
      scope.onEnd(() => closes.incrementAndGet(): Unit)
      Chunks.fromIterator(Iterator.range(start.toInt, n).map(k => (i * n + k).toLong), 100)
    }
  def sink(n: Int): Wire[Long, Long] = Wire.fold(Aggregator.sum[Long])
}

/**
 * `Flow.opened` (specs/dataflow.md, stage 16): a partition that holds a
 * resource opens it AT ITS POSITION and closes it with the partition.
 */
/**
 * AN IN-PROCESS WORKER THAT DIES LIKE A PROCESS: after `after` advances
 * every request throws, and the sessions it opened die with it. Without
 * that second half a survivor in the same JVM finds the dead worker's
 * session in `Sessions` and carries on reading it — nothing reopens,
 * and a test of the reopen tests nothing (it did, the first time).
 */
final class Dying(after: Int) extends Cluster.Serve:
  private val advances = AtomicInteger(0)
  private val mine = ConcurrentLinkedQueue[Long]()
  @volatile var dead = false
  def apply(req: Req): Resp =
    if !dead && req.isInstanceOf[Req.Advance] && advances.incrementAndGet() > after then
      dead = true
      mine.asScala.foreach(Sessions.drop)
    if dead then throw java.io.IOException("this worker is gone")
    req match
      case o: Req.Open => mine.add(o.session): Unit
      case _ => ()
    Cluster.local(req)

class TestFlowOpened extends munit.FunSuite {
  Jobs.register(OpenedJob)

  def expected(n: Int, parts: Int): Long = (0 until parts * n).map(_.toLong).sum

  test("every partition opens at zero and closes at its end") {
    OpenedJob.opens.clear(); OpenedJob.closes.set(0)
    val got = Cluster.run(OpenedJob, 1000, 4, Vector.fill(2)(Cluster.local)).runWith
    assertEquals(got.value, expected(1000, 4))
    assertEquals(OpenedJob.opens.asScala.toSet, (0 until 4).map(i => (i, 0L)).toSet)
    assertEquals(OpenedJob.closes.get, 4)
  }

  test("a replacement worker opens its partition AT THE POSITION, and the answer is unchanged") {
    OpenedJob.opens.clear(); OpenedJob.closes.set(0)
    val dying = Dying(3)
    val got = Cluster.stream(OpenedJob, 1000, 2, Vector(dying, Cluster.local), 100).runWith
    assertEquals(got.value, expected(1000, 2))
    assert(OpenedJob.opens.asScala.exists(_._2 > 0L), s"nobody opened past zero: ${OpenedJob.opens}")
  }
}
