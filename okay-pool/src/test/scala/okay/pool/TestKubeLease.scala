package okay.pool

/**
 * A REAL `coordination.k8s.io/v1 Lease`, OVER A REAL API SERVER
 * (specs/cluster-pool.md, stage 5) — `kubectl proxy` is the same
 * authenticated, TLS-free local road a real in-cluster call replaces
 * with a ServiceAccount token and the cluster's own CA
 * (`KubeLease.inCluster`); the wire on the API server's side is
 * identical either way, which is what makes this an honest test
 * rather than a mock of one.
 *
 * `Live`-tagged because it shells out (`kubectl`) AND needs a real
 * Kubernetes API server; skipped where either is absent. It earned
 * its keep on the FIRST run: `Instant.now().toString`'s own format
 * was refused with a 400 naming `metav1.MicroTime`'s exact layout —
 * see `KubeLease.microTime`'s doc comment.
 */
object TestKubeLease:
  /**
   * THE PROBE `munitIgnore` RUNS STARTS NOTHING (kube-lease-proxy-leak,
   * 2026-09-25). It used to be `proxy.isEmpty`, which FORCED the lazy
   * val below: munit evaluates `munitIgnore` for every run, Live or
   * not, and `afterAll` never runs for a suite whose tests were all
   * filtered out — so every default gate on the box started one
   * `kubectl proxy` and left it running (99 orphans found, the oldest
   * 19 hours). This looks at the file system only: `kubectl` on the
   * PATH and a kubeconfig where kubectl would look. Whether an API
   * server actually answers is found out by the first test, which
   * `assume`s the proxy came up (a skip, not a failure).
   */
  def configured: Boolean =
    val sep = java.io.File.pathSeparator
    val kubectl = sys.env.getOrElse("PATH", "").split(sep).exists(d =>
      d.nonEmpty && java.nio.file.Files.isExecutable(java.nio.file.Path.of(d, "kubectl")))
    val config = sys.env.get("KUBECONFIG") match
      case Some(list) => list.split(sep).exists(p => p.nonEmpty && java.nio.file.Files.exists(java.nio.file.Path.of(p)))
      case None => java.nio.file.Files.exists(java.nio.file.Path.of(sys.props("user.home"), ".kube", "config"))
    kubectl && config

  /** set by the initializer below, so `stop()` never forces it */
  @volatile private var started = false

  /** started ONLY from a test body (`base`), never from a probe */
  private lazy val proxy: Option[(Process, Int)] =
    started = true
    try
      val port = freePort()
      val p = ProcessBuilder("kubectl", "proxy", s"--port=$port").redirectErrorStream(true).start()
      val up = (1 to 100).exists { _ =>
        Thread.sleep(50)
        try { val s = java.net.Socket("127.0.0.1", port); s.close(); true }
        catch case _: Exception => false
      }
      if up then Some((p, port)) else { p.destroyForcibly(): Unit; None }
    catch case _: Exception => None

  /** what `afterAll` calls: stops the proxy if, and only if, a test started it */
  def stop(): Unit = if started then proxy.foreach((p, _) => p.destroyForcibly(): Unit)

  /** the proxy's base URL for a test, or a SKIP when it did not come up */
  def base(using munit.Location): String =
    munit.Assertions.assume(proxy.isDefined, "kubectl proxy did not come up: no reachable API server")
    s"http://127.0.0.1:${proxy.get._2}"

  private def freePort(): Int =
    val s = java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()

/** the one branch that needs no network at all: refusing cleanly
 * when this process is plainly not running in a pod */
class TestKubeLeaseInCluster extends munit.FunSuite:
  test("inCluster: KUBERNETES_SERVICE_HOST unset is a named refusal, not a crash") {
    val out = KubeLease.inCluster("x", "holder", "ns")
    assert(out.isLeft)
    assert(out.left.toOption.get.contains("KUBERNETES_SERVICE_HOST"), out.toString)
  }

class TestKubeLease extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !TestKubeLease.configured

  override def afterAll(): Unit = TestKubeLease.stop()

  private def base: String = TestKubeLease.base
  private def freshName(): String = s"okay-test-${java.util.UUID.randomUUID()}"
  private def cleanup(name: String): Unit =
    try KubeLease(base, "default", name, "cleanup", None, "").release(0) catch case _: Exception => ()

  test("take() creates the Lease object when it is absent, and answers a fencing term") {
    val name = freshName()
    try assert(KubeLease(base, "default", name, "holder-a", None, "").take().isDefined)
    finally cleanup(name)
  }

  test("a SECOND holder cannot take an unexpired lease the first one holds") {
    val name = freshName()
    try
      assert(KubeLease(base, "default", name, "holder-a", None, "").take().isDefined)
      assertEquals(KubeLease(base, "default", name, "holder-b", None, "").take(), None)
    finally cleanup(name)
  }

  test("held() renews, and keeps answering true while nobody else has written") {
    val name = freshName()
    try
      val a = KubeLease(base, "default", name, "holder-a", None, "")
      val term = a.take().get
      assert(a.held(term))
      assert(a.held(term))
    finally cleanup(name)
  }

  test("release() removes the object, so the next take() creates it fresh") {
    val name = freshName()
    val a = KubeLease(base, "default", name, "holder-a", None, "")
    a.release(a.take().get)
    val term2 = KubeLease(base, "default", name, "holder-b", None, "").take()
    assert(term2.isDefined, term2.toString)
    cleanup(name)
  }

  test("an EXPIRED lease lets a new holder take it over") {
    val name = freshName()
    KubeLease(base, "default", name, "holder-a", None, "", leaseDurationSeconds = 1).take(): Unit
    Thread.sleep(1500)
    val term = KubeLease(base, "default", name, "holder-b", None, "", leaseDurationSeconds = 15).take()
    assert(term.isDefined, term.toString)
    cleanup(name)
  }

  test("the fencing term only rises: a takeover's term is greater than the original holder's") {
    val name = freshName()
    val first = KubeLease(base, "default", name, "holder-a", None, "", leaseDurationSeconds = 1).take().get
    Thread.sleep(1500)
    val second = KubeLease(base, "default", name, "holder-b", None, "", leaseDurationSeconds = 15).take().get
    assert(second > first, s"$second was not greater than $first")
    cleanup(name)
  }

  /** an outside party overwriting the Lease object directly, the way
   * a SECOND `KubeLease` (a real competitor) would once it takes over
   * -- raw, deliberately not going through `KubeLease` itself, so this
   * test does not just check the class against its own bookkeeping */
  private def stealExternally(name: String, resourceVersion: String): Unit =
    val body = s"""{"apiVersion":"coordination.k8s.io/v1","kind":"Lease",
      |"metadata":{"name":"$name","namespace":"default","resourceVersion":"$resourceVersion"},
      |"spec":{"holderIdentity":"a-stranger","leaseDurationSeconds":30,
      |"renewTime":"${KubeLease.microTime.format(java.time.Instant.now())}"}}""".stripMargin
    val req = java.net.http.HttpRequest.newBuilder(
      java.net.URI.create(s"$base/apis/coordination.k8s.io/v1/namespaces/default/leases/$name"))
      .header("content-type", "application/json")
      .PUT(java.net.http.HttpRequest.BodyPublishers.ofString(body)).build()
    val client = java.net.http.HttpClient.newBuilder().version(java.net.http.HttpClient.Version.HTTP_1_1).build()
    val res = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString())
    assertEquals(res.statusCode(), 200, res.body())

  test("a member DEPOSED by an outside taker throws Checkpoint.Deposed at its own next commit " +
    "(specs/cluster-pool.md, the exact Behavior box)") {
    import okay.cluster.Checkpoint
    val name = freshName()
    try
      val lease = KubeLease(base, "default", name, "holder-a", None, "", leaseDurationSeconds = 30)
      val term = lease.take().get
      // read the resourceVersion `lease` itself now holds, so the
      // external write is a real CAS-winning takeover, not a guess
      val current = {
        val req = java.net.http.HttpRequest.newBuilder(
          java.net.URI.create(s"$base/apis/coordination.k8s.io/v1/namespaces/default/leases/$name")).GET().build()
        val client = java.net.http.HttpClient.newBuilder().version(java.net.http.HttpClient.Version.HTTP_1_1).build()
        val body = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString()).body()
        okay.codec.Json.parse(body) match
          case okay.codec.Json.JObj(fs) =>
            fs.collectFirst { case ("metadata", okay.codec.Json.JObj(ms)) =>
              ms.collectFirst { case ("resourceVersion", okay.codec.Json.JStr(s)) => s }.get }.get
          case other => fail(s"unexpected Lease JSON shape: $other")
      }
      stealExternally(name, current)
      val checkpoint = Checkpoint.fenced(term, lease, Checkpoint.Memory())
      intercept[Checkpoint.Deposed](checkpoint.save(1, Array.emptyByteArray))
    finally cleanup(name)
  }
