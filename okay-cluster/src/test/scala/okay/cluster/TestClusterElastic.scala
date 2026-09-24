package okay.cluster

import okay.*
import okay.given

/**
 * PEERS RE-RESOLVED AT EVERY EPOCH BOUNDARY (specs/cluster-pool.md,
 * stage 5) — `Cluster.stream`'s own `resolve`/`onRefusedRescale`
 * seam, exercised directly with a synthetic `resolve` rather than a
 * real `Discovery`. `StripeKeyedJob` (rescalable) and `WindowJob`
 * (NOT — it slices rather than stripes, and never overrides
 * `rescalable`) already exist for exactly this pair of properties.
 */
class TestClusterElastic extends munit.FunSuite {
  TestJobs.install()
  Jobs.register(StripeKeyedJob)

  private val feed = Feed(2000, 0)

  /** a `Serve` that counts its own dispatches, so a test can tell
   * WHICH vector's entry actually served a partition */
  private def counting(): (Cluster.Serve, () => Int) =
    var n = 0
    ((req => { n += 1; Cluster.local(req) }), () => n)

  test("a rescalable job whose peer COUNT changes mid-stream throws Cluster.Rescale, carrying the new peers") {
    var calls = 0
    val resolve = () =>
      calls += 1
      pure[Async, Vector[Cluster.Serve]](if calls >= 2 then Vector(Cluster.local, Cluster.local) else Vector(Cluster.local))
    val ex = intercept[Cluster.Rescale](
      Cluster.stream(StripeKeyedJob, feed, 4, Vector(Cluster.local), 200, resolve = Some(resolve)).runWith)
    assertEquals(ex.peers.length, 2)
  }

  test("a NON-rescalable job's peer-count change is refused BY NAME and the run finishes on the ORIGINAL workers") {
    var calls = 0
    var refusals = Vector.empty[(Int, Int)]
    val resolve = () =>
      calls += 1
      pure[Async, Vector[Cluster.Serve]](if calls >= 2 then Vector(Cluster.local, Cluster.local) else Vector(Cluster.local))
    val got = Cluster.stream(WindowJob, feed, 4, Vector(Cluster.local), 200,
      resolve = Some(resolve), onRefusedRescale = (have, want) => refusals :+= (have, want)).runWith
    assert(refusals.nonEmpty, "onRefusedRescale never fired")
    assertEquals(refusals.head, (2, 1))
    // the run reached its ordinary end despite the refused resize
    assertEquals(got.partitions, 4)
  }

  test("SAME peer count, a DIFFERENT identity: the fresh vector is dispatched to, not the stale one") {
    val (w0, _) = counting()
    val (w1, c1) = counting()
    var calls = 0
    val resolve = () =>
      calls += 1
      pure[Async, Vector[Cluster.Serve]](if calls >= 2 then Vector(w1) else Vector(w0))
    val _ = Cluster.stream(StripeKeyedJob, feed, 4, Vector(w0), 200, resolve = Some(resolve)).runWith
    assert(c1() > 0, "the replacement worker (w1, same count as w0) was never dispatched to")
  }

  test("resolve defaulting to None behaves exactly as no resolve at all -- every caller before this stage") {
    val a = Cluster.stream(StripeKeyedJob, feed, 4, Vector(Cluster.local), 200).runWith
    val b = Cluster.stream(StripeKeyedJob, feed, 4, Vector(Cluster.local), 200, resolve = None).runWith
    assertEquals(a.value, b.value)
  }
}
