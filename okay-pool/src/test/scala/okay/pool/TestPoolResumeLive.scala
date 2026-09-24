package okay.pool

import okay.*
import okay.given
import okay.cluster.Folded
import okay.codec.{Codecs, Json}
import okay.resilience.Discovery

/**
 * "the member that accepted a submission is killed before it
 * finishes; a `GET` against a different member resumes it, no second
 * submission" (specs/cluster-pool.md, stage 1) — proven against a
 * REAL running fiber cancelled mid-epoch, which is a race against
 * this box's own speed rather than an assertion, hence `Live`
 * (AGENTS.md: "no flaky tests in the default gate"), the same reason
 * `TestFederation`'s two-process suite is.
 */
class TestPoolResumeLive extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  CountJobs.install()

  test("a member that accepted a submission dies mid-run; a DIFFERENT statusOf call resumes it") {
    val store = SharedStore()
    val id = "count-resume"
    val conf = PoolConf()
    val noPeers: Discovery = Discovery.static(Map.empty)
    val (checkpoint, lease) = store(id)
    // a large n and a small take: many epochs, so a poll every 5ms
    // reliably lands before the run would finish on its own
    val m = RunMeta(CountJob.name, Json.JNum(20000000), 3, 2000)
    val (metaCk, _) = store(s"$id.meta")
    metaCk.save(0, RunMeta.encode(m))
    // the FIRST attempt -- spawned directly, standing in for the member
    // that accepted the original POST and then died before finishing
    val peers = Pool.workers(conf, noPeers).runWith
    val prog = CountJob.lead(m.params, m.parts, peers, Pool.effectiveTake(m.take), checkpoint, lease).toOption.get
    val fiber = Async.spawn(prog)
    // let at least one epoch land, then kill it -- best-effort
    // cancellation between operations, exactly as a real process death
    // leaves the journal: at a committed epoch, never mid-write
    var waited = 0
    while checkpoint.latest.isEmpty && waited < 2000 do { Thread.sleep(5); waited += 1 }
    fiber.cancel()
    assert(checkpoint.latest.isDefined, "at least one epoch should have committed before the kill")
    assert(!checkpoint.latest.exists((_, bytes) => decodedDone(bytes)),
      "the run finished before it could be killed -- widen the workload or shrink `take`")
    // a DIFFERENT statusOf call -- standing in for a different member --
    // resumes it to completion with no second submission
    val done = TestSupport.waitDone(id, conf, noPeers, store(_))
    assertEquals(done.value, "20000000")
  }

  private def decodedDone(bytes: Array[Byte]): Boolean =
    Codecs.cbor(Folded.given_Schema_Folded).decode(bytes).toOption.exists(_.done)
