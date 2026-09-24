package okay.pool

import okay.*
import okay.given
import okay.cluster.{Flow, Job, Jobs, Served, Wire}
import okay.codec.{Json, Schema}
import okay.resilience.{Discovery, Endpoint}

/** the smallest STRIPED, fold-sink job — rescalable, the same shape
 * `StripeKeyedJob`/box 13 already prove correct at the engine level;
 * this suite proves okay-pool actually FOLLOWS the resize end to end */
object RescaleJob extends Job[Long, Long]:
  type A = Long
  def name = "pool.test.rescale"
  def params: Schema[Long] = summon[Schema[Long]]
  def answer: Schema[Long] = summon[Schema[Long]]
  override def rescalable: Boolean = true
  def flow(n: Long, parts: Int): Flow[Long] = Flow.striped(Vector.range(0L, n), parts)
  def sink(n: Long): Wire[Long, Long] = Wire.fold(Aggregator.count[Long])

/**
 * PEERS RE-RESOLVED AT EVERY EPOCH BOUNDARY, END TO END
 * (specs/cluster-pool.md, stage 5). `TestClusterElastic` (okay-cluster)
 * already proves `Cluster.stream`'s own `resolve`/`Rescale` mechanics
 * with a synthetic resolve; this proves `Pool.nudge` actually catches
 * `Cluster.Rescale`, rewrites the run's stored `parts`, and restarts
 * immediately — no external poll needed for the run to follow a real
 * peer-count change.
 */
class TestPoolElastic extends munit.FunSuite:
  Jobs.register(RescaleJob)

  /** empty on the FIRST resolve (the submission itself), a second real
   * member on every one after — simulating the pool growing by one
   * between the first epoch and the second */
  private final class GrowingDiscovery(port: Int) extends Discovery:
    private val calls = java.util.concurrent.atomic.AtomicInteger(0)
    def resolve(service: String): Vector[Endpoint] ! Async =
      pure(if calls.getAndIncrement() == 0 then Vector.empty else Vector(Endpoint("127.0.0.1", port)))

  test("a rescalable run whose peer count grows mid-run is followed to the new width, and still finishes correctly") {
    val peer = java.net.ServerSocket(0)
    okay.Threads.spawn("t")(() => Served.serve(peer, Pool.fingerprinted("")))
    try
      val discovery = GrowingDiscovery(peer.getLocalPort)
      val store = SharedStore()
      val conf = PoolConf(service = "growing")
      val id = "rescale-1"
      val res = Pool.submit(RescaleJob.name, Json.JNum(5000), 0, 50, id, conf, discovery, store(_)).runWith
      assert(res.isRight, res.toString)
      val done = TestSupport.waitDone(id, conf, discovery, store(_))
      assertEquals(done.value, "5000")
      val (metaCk, _) = store(s"$id.meta")
      val m = metaCk.latest.flatMap((_, b) => RunMeta.decode(b).toOption)
      assertEquals(m.map(_.parts), Some(2), s"the run's stored parts never followed the peer count: $m")
    finally peer.close()
  }

