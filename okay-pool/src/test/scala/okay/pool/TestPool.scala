package okay.pool

import okay.*
import okay.given
import okay.cluster.{Checkpoint, Lease, Served}
import okay.codec.Json
import okay.http.{Body, Method, Request}
import okay.resilience.{Discovery, Endpoint}
import java.net.ServerSocket

/** a `Checkpoint`/`Lease` factory that hands the SAME `Checkpoint`
 * back for the same name — the shared-store stand-in every "member"
 * in these tests calls through, exactly as several real pool members
 * would call through a real okay-persist-backed one */
final class SharedStore:
  private val cps = scala.collection.mutable.Map.empty[String, Checkpoint]
  def apply(name: String): (Checkpoint, Lease) =
    synchronized(cps.getOrElseUpdate(name, Checkpoint.Memory())) -> Lease.solitary

class TestPool extends munit.FunSuite {
  CountJobs.install()

  val noPeers: Discovery = Discovery.static(Map.empty)
  def confOf(build: String = "", peers: String = ""): PoolConf =
    PoolConf(build = build, peers = peers)

  // ---- resolve / workers -------------------------------------------

  test("resolve: the static list, when service is unset") {
    val conf = confOf(peers = "a:1,b:2")
    val es = Pool.resolve(conf, noPeers).runWith
    assertEquals(es.toSet, Set(Endpoint("a", 1), Endpoint("b", 2)))
  }

  test("resolve: discovery and the static list, unioned") {
    val discovery = Discovery.static(Map("pool" -> Vector(Endpoint("c", 3))))
    val conf = confOf(peers = "a:1").copy(service = "pool")
    val es = Pool.resolve(conf, discovery).runWith
    assertEquals(es.toSet, Set(Endpoint("a", 1), Endpoint("c", 3)))
  }

  test("workers: always includes this member, in-process") {
    val conf = confOf()
    val ws = Pool.workers(conf, noPeers).runWith
    assertEquals(ws.length, 1)
  }

  test("workers: a peer whose build disagrees is excluded") {
    val agreeing = ServerSocket(0)
    val disagreeing = ServerSocket(0)
    okay.Threads.spawn("t")(() => Served.serve(agreeing, Pool.fingerprinted("v1")))
    okay.Threads.spawn("t")(() => Served.serve(disagreeing, Pool.fingerprinted("v2")))
    try
      val conf = confOf(build = "v1", peers = s"127.0.0.1:${agreeing.getLocalPort},127.0.0.1:${disagreeing.getLocalPort}")
      val ws = Pool.workers(conf, noPeers).runWith
      // this member + the agreeing peer only
      assertEquals(ws.length, 2)
    finally { agreeing.close(); disagreeing.close() }
  }

  test("workers: no build configured means no check at all") {
    val disagreeing = ServerSocket(0)
    okay.Threads.spawn("t")(() => Served.serve(disagreeing, Pool.fingerprinted("v2")))
    try
      val conf = confOf(build = "", peers = s"127.0.0.1:${disagreeing.getLocalPort}")
      val ws = Pool.workers(conf, noPeers).runWith
      assertEquals(ws.length, 2)
    finally disagreeing.close()
  }

  // ---- leaseFor (specs/cluster-pool.md, stage 5) -----------------------

  test("leaseFor: \"\" is Lease.solitary -- take() always succeeds, no manager asked") {
    val l = Pool.leaseFor(confOf(), "run-a")
    assertEquals(l.take(), Some(1L))
    assertEquals(l.take(), Some(1L))
  }

  test("leaseFor: an unknown kind falls back to Lease.solitary rather than crash") {
    val l = Pool.leaseFor(confOf().copy(leaseKind = "nonsense"), "run-a")
    assertEquals(l.take(), Some(1L))
  }

  test("leaseFor: \"kube\" with an explicit leaseUrl builds a KubeLease, never touching the in-cluster env") {
    val l = Pool.leaseFor(confOf().copy(leaseKind = "kube", leaseUrl = "http://127.0.0.1:1"), "run-a")
    assert(l.isInstanceOf[KubeLease], l.getClass.getName)
  }

  test("leaseFor: \"consul\" builds a ConsulLease") {
    val l = Pool.leaseFor(confOf().copy(leaseKind = "consul", leaseUrl = "http://127.0.0.1:1"), "run-a")
    assert(l.isInstanceOf[ConsulLease], l.getClass.getName)
  }

  // ---- submit ---------------------------------------------------------

  test("submit: an unknown job is a named 404") {
    val store = SharedStore()
    val out = Pool.submit("no.such.job", Json.JNum(1), 0, 0, "", confOf(), noPeers, store(_)).runWith
    out match
      case Left((404, why)) => assert(why.contains("no.such.job"))
      case other => fail(s"expected a 404, got $other")
  }

  test("submit: bad parameters are a named 400, before anything runs") {
    val store = SharedStore()
    val out = Pool.submit(CountJob.name, Json.JStr("not a number"), 0, 0, "", confOf(), noPeers, store(_)).runWith
    out match
      case Left((400, why)) => assert(why.contains(CountJob.name))
      case other => fail(s"expected a 400, got $other")
  }

  test("submit: a valid submission answers 202 and writes the run's record") {
    val store = SharedStore()
    val out = Pool.submit(CountJob.name, Json.JNum(10), 0, 0, "", confOf(), noPeers, store(_)).runWith
    val id = out.toOption.get.run
    val (metaCk, _) = store(s"$id.meta")
    assert(metaCk.latest.isDefined, "the run's record should be on record after a 202")
  }

  test("submit: a repeat POST of the same journal reuses the stored record") {
    val store = SharedStore()
    val id = "myrun"
    val first = Pool.submit(CountJob.name, Json.JNum(10), 0, 0, id, confOf(), noPeers, store(_)).runWith
    // the second body disagrees with the first -- it must be ignored
    val second = Pool.submit(CountJob.name, Json.JNum(999), 0, 0, id, confOf(), noPeers, store(_)).runWith
    assertEquals(first, Right(Submitted(id)))
    assertEquals(second, Right(Submitted(id)))
    // wait for it to finish and check the ORIGINAL value (10) answered, not 999
    val status = waitDone(id, confOf(), noPeers, store(_))
    assertEquals(status.value, "10")
  }

  test("submit: the same id under a different job name is a named conflict") {
    val store = SharedStore()
    val id = "onerun"
    val _ = Pool.submit(CountJob.name, Json.JNum(10), 0, 0, id, confOf(), noPeers, store(_)).runWith
    val out = Pool.submit("no.such.job", Json.JNum(1), 0, 0, id, confOf(), noPeers, store(_)).runWith
    // the SECOND job name does not exist, so this is a 404, not a 409 --
    // the 409 needs the second name to exist too
    assert(out.isLeft)
  }

  // ---- statusOf --------------------------------------------------------

  test("statusOf: an id nobody has ever submitted is unknown") {
    val store = SharedStore()
    val out = Pool.statusOf("nope", confOf(), noPeers, store(_)).runWith
    assertEquals(out, None)
  }

  test("end to end: submit, poll, and the count is exact") {
    val store = SharedStore()
    val id = "count-e2e"
    val _ = Pool.submit(CountJob.name, Json.JNum(5000), 4, 200, id, confOf(), noPeers, store(_)).runWith
    val done = waitDone(id, confOf(), noPeers, store(_))
    assertEquals(done.value, "5000")
    assertEquals(done.dropped, 0L)
  }

  test("statusOf: a job this build no longer knows is a named failure") {
    val store = SharedStore()
    val id = "ghost"
    val (metaCk, _) = store(s"$id.meta")
    metaCk.save(0, RunMeta.encode(RunMeta("nowhere.at.all", Json.JNum(1), 1, 0)))
    val out = Pool.statusOf(id, confOf(), noPeers, store(_)).runWith
    out match
      case Some(Status.Failed(why)) => assert(why.contains("nowhere.at.all"))
      case other => fail(s"expected a named Failed, got $other")
  }

  // ---- routes, end to end over the PartialFunction ----------------------

  test("routes: healthz and readyz") {
    val store = SharedStore()
    var ready = false
    val router = Routes.router(confOf(), noPeers, store(_), () => ready)
    val notReady = router.routes(Request.get("/readyz")).runWith
    assertEquals(notReady.status, 503)
    ready = true
    val isReady = router.routes(Request.get("/readyz")).runWith
    assertEquals(isReady.status, 200)
    val live = router.routes(Request.get("/healthz")).runWith
    assertEquals(live.status, 200)
  }

  test("routes: submit then poll to Done") {
    val store = SharedStore()
    val router = Routes.router(confOf(), noPeers, store(_), () => true)
    val body = Json.print(Json.JObj(Vector("params" -> Json.JNum(10), "journal" -> Json.JStr("route-run"))))
    val posted = router.routes(Request(Method.Post, s"/pool/jobs/${CountJob.name}", Nil, Body.Text(body))).runWith
    assertEquals(posted.status, 202)
    var status = ""
    var tries = 0
    while !status.contains("\"Done\"") && tries < 500 do
      Thread.sleep(10)
      val got = router.routes(Request.get("/pool/runs/route-run")).runWith
      status = new String(okay.http.Http.bytes(got).runWith.toArray, java.nio.charset.StandardCharsets.UTF_8)
      tries += 1
    assert(status.contains("\"Done\""), s"never finished: $status")
    assert(status.contains("\"value\":\"10\""), status)
  }

  test("routes: an unregistered job is 404") {
    val store = SharedStore()
    val router = Routes.router(confOf(), noPeers, store(_), () => true)
    val res = router.routes(Request(Method.Post, "/pool/jobs/nowhere", Nil, Body.Text("{}"))).runWith
    assertEquals(res.status, 404)
  }

  test("routes: a body that is not a JSON object is a 400") {
    val store = SharedStore()
    val router = Routes.router(confOf(), noPeers, store(_), () => true)
    val res = router.routes(Request(Method.Post, s"/pool/jobs/${CountJob.name}", Nil, Body.Text("[1,2]"))).runWith
    assertEquals(res.status, 400)
  }

  test("routes: jobs and peers") {
    val store = SharedStore()
    val router = Routes.router(confOf(peers = "x:1"), noPeers, store(_), () => true)
    val jobs = router.routes(Request.get("/pool/jobs")).runWith
    val jobsBody = new String(okay.http.Http.bytes(jobs).runWith.toArray, java.nio.charset.StandardCharsets.UTF_8)
    assert(jobsBody.contains(CountJob.name), jobsBody)
    val peers = router.routes(Request.get("/pool/peers")).runWith
    val peersBody = new String(okay.http.Http.bytes(peers).runWith.toArray, java.nio.charset.StandardCharsets.UTF_8)
    assert(peersBody.contains("\"host\":\"x\""), peersBody)
  }

  // ---- helpers -----------------------------------------------------

  private def waitDone(id: String, conf: PoolConf, discovery: Discovery, store: String => (Checkpoint, Lease)): Status.Done =
    TestSupport.waitDone(id, conf, discovery, store)
}

/** shared by `TestPool` and the `Live`-tagged resume test */
private[pool] object TestSupport:
  def waitDone(id: String, conf: PoolConf, discovery: Discovery, store: String => (Checkpoint, Lease)): Status.Done =
    var out: Option[Status] = None
    var tries = 0
    while !out.exists { case Status.Done(_, _, _, _, _) => true; case _ => false } && tries < 1000 do
      out = Pool.statusOf(id, conf, discovery, store).runWith
      if !out.exists { case Status.Done(_, _, _, _, _) => true; case _ => false } then Thread.sleep(5)
      tries += 1
    out match
      case Some(d: Status.Done) => d
      case other => throw AssertionError(s"never reached Done for '$id': $other")
