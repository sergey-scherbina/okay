package okay.pool

import okay.cluster.Lease
import okay.codec.Json
import okay.codec.Json.{JNum, JObj, JStr}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.KeyStore
import java.security.cert.CertificateFactory
import java.time.Instant
import javax.net.ssl.{SSLContext, TrustManagerFactory}

/**
 * A `coordination.k8s.io/v1 Lease` OBJECT AS THE SEAT
 * (specs/cluster-pool.md, stage 5) — the ONLY place this engine ever
 * speaks to a manager's API, and it is exactly the three methods
 * `okay.cluster.Lease` already asks for, over one REST resource
 * Kubernetes ships for precisely this purpose (the same object
 * `kube-scheduler`'s own leader election uses).
 *
 * PLAIN, SYNCHRONOUS `java.net.http.HttpClient` — never `okay.http`'s
 * `Async`-typed client — because `Lease`'s own trait is plain
 * synchronous Scala (`def take(): Option[Long]`, no effect type at
 * all) and `held` is documented as "called once per epoch, before the
 * commit": a real, blocking network round trip already IS the
 * intended cost, not something to hide behind a fiber.
 *
 * THE FENCING TOKEN IS THE OBJECT'S OWN `resourceVersion`, which
 * Kubernetes (etcd underneath) guarantees only RISES on every write to
 * the object — exactly what `Checkpoint.newest` needs ("later TERM
 * wins"). `held`'s renewal PUT carries the resourceVersion this
 * instance last saw as a precondition baked into the body; the API
 * server answers 409 the moment anyone else has written since, which
 * IS the mutual exclusion — no separate holder-identity check is
 * needed on that path, because a stale write cannot land at all.
 *
 * `take()` refuses an unexpired lease outright, EVEN ONE THIS SAME
 * `holder` string already names: a restarted process with a reused
 * pod name gets no special path back in, on purpose — the process
 * that is still actually running holds a resourceVersion this one
 * has never seen, and a resourceVersion-blind "it's probably still
 * me" is exactly the split-brain a fencing token exists to rule out.
 */
final class KubeLease(base: String, namespace: String, name: String,
                      holder: String, token: Option[String], caFile: String,
                      leaseDurationSeconds: Int = 15) extends Lease:

  private val collectionUrl = s"$base/apis/coordination.k8s.io/v1/namespaces/$namespace/leases"
  private val objectUrl = s"$collectionUrl/$name"
  private val client: HttpClient = KubeLease.clientFor(caFile)
  @volatile private var resourceVersion: String = ""

  private def send(method: String, url: String, body: Option[String]): HttpResponse[String] =
    val b = HttpRequest.newBuilder(URI.create(url))
      .header("content-type", "application/json")
      .header("accept", "application/json")
    token.foreach(t => b.header("authorization", s"Bearer $t"))
    val pub = body.fold(HttpRequest.BodyPublishers.noBody())(HttpRequest.BodyPublishers.ofString(_, UTF_8))
    method match
      case "GET" => b.GET()
      case "POST" => b.POST(pub)
      case "PUT" => b.PUT(pub)
      case "DELETE" => b.DELETE()
    client.send(b.build(), HttpResponse.BodyHandlers.ofString(UTF_8))

  /**
   * `metav1.MicroTime`, EXACTLY — Kubernetes's own Go layout is
   * `"2006-01-02T15:04:05.000000Z07:00"`, six fractional digits
   * always present, never `Instant.toString`'s own (which OMITS the
   * fraction entirely when nanos happen to be zero, and carries up to
   * nine digits otherwise). Measured against a real kind cluster: a
   * bare `Instant.now().toString` is refused with a 400 naming this
   * exact layout string back.
   */
  private def now(): String =
    KubeLease.microTime.format(Instant.now())

  private def path(j: Json, ks: String*): Option[Json] =
    ks.foldLeft(Option(j)) { (acc, k) =>
      acc.collect { case JObj(fs) => fs }.flatMap(_.collectFirst { case (`k`, v) => v })
    }

  private def str(j: Option[Json]): Option[String] = j.collect { case JStr(s) => s }
  private def num(j: Option[Json]): Option[Double] = j.collect { case JNum(n) => n }

  private def leaseBody(acquireTime: Option[String], resourceVersionOf: String): String =
    val meta = JObj(Vector("name" -> JStr(name), "namespace" -> JStr(namespace)) ++
      (if resourceVersionOf.nonEmpty then Vector("resourceVersion" -> JStr(resourceVersionOf)) else Vector.empty))
    val spec = JObj(Vector(
      "holderIdentity" -> JStr(holder),
      "leaseDurationSeconds" -> JNum(leaseDurationSeconds.toDouble),
      "renewTime" -> JStr(now())) ++
      acquireTime.map(t => "acquireTime" -> JStr(t)).toVector)
    Json.print(JObj(Vector(
      "apiVersion" -> JStr("coordination.k8s.io/v1"), "kind" -> JStr("Lease"),
      "metadata" -> meta, "spec" -> spec)))

  private def expired(spec: Json): Boolean =
    (for
      rt <- str(path(spec, "renewTime")).map(Instant.parse)
      secs <- num(path(spec, "leaseDurationSeconds"))
    yield rt.plusSeconds(secs.toLong).isBefore(Instant.now())).getOrElse(true)

  def take(): Option[Long] =
    send("GET", objectUrl, None).statusCode() match
      case 404 =>
        val res = send("POST", collectionUrl, Some(leaseBody(Some(now()), "")))
        if res.statusCode() != 201 then None
        else
          val rv = str(path(Json.parse(res.body()), "metadata", "resourceVersion")).getOrElse("0")
          resourceVersion = rv
          rv.toLongOption
      case 200 =>
        val got = Json.parse(send("GET", objectUrl, None).body())
        if !expired(path(got, "spec").getOrElse(JObj(Vector.empty))) then None
        else
          val rv = str(path(got, "metadata", "resourceVersion")).getOrElse("")
          val res = send("PUT", objectUrl, Some(leaseBody(Some(now()), rv)))
          if res.statusCode() != 200 then None
          else
            val rv2 = str(path(Json.parse(res.body()), "metadata", "resourceVersion")).getOrElse(rv)
            resourceVersion = rv2
            rv2.toLongOption
      case _ => None

  def held(term: Long): Boolean =
    val res = send("PUT", objectUrl, Some(leaseBody(None, resourceVersion)))
    if res.statusCode() != 200 then false
    else
      resourceVersion = str(path(Json.parse(res.body()), "metadata", "resourceVersion")).getOrElse(resourceVersion)
      true

  override def release(term: Long): Unit =
    try send("DELETE", objectUrl, None): Unit
    catch case _: Exception => ()   // best-effort: the lease also just expires

object KubeLease:
  /** six fractional digits, always, UTC, `Z` — `metav1.MicroTime`'s
   * own layout, not `Instant.toString`'s variable one */
  val microTime: java.time.format.DateTimeFormatter =
    java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSS'Z'")
      .withZone(java.time.ZoneOffset.UTC)

  private val svcRoot = "/var/run/secrets/kubernetes.io/serviceaccount"

  /** the in-cluster defaults every pod already carries: the API
   * server's own DNS name, this pod's ServiceAccount token, its own
   * namespace, its own CA -- what a `leaseUrl`/`leaseToken` of "" mean */
  def inCluster(name: String, holder: String, namespace: String = "",
               leaseDurationSeconds: Int = 15): Either[String, KubeLease] =
    val host = Option(System.getenv("KUBERNETES_SERVICE_HOST")).filter(_.nonEmpty)
    val port = Option(System.getenv("KUBERNETES_SERVICE_PORT")).filter(_.nonEmpty).getOrElse("443")
    host match
      case None => Left("KUBERNETES_SERVICE_HOST is unset -- this is not running in a pod; " +
        "pass leaseUrl/leaseToken/leaseNamespace explicitly instead")
      case Some(h) =>
        val ns = if namespace.nonEmpty then namespace
          else readFile(s"$svcRoot/namespace").getOrElse("default")
        val tok = readFile(s"$svcRoot/token")
        Right(KubeLease(s"https://$h:$port", ns, name, holder, tok, s"$svcRoot/ca.crt", leaseDurationSeconds))

  private def readFile(path: String): Option[String] =
    try Some(java.nio.file.Files.readString(java.nio.file.Paths.get(path)).trim)
    catch case _: Exception => None

  /** a plain client for "http://..." (a `kubectl proxy`, the test
   * road) or one trusting `caFile` for "https://..." — never the
   * platform default trust store, which does not know a cluster's own
   * CA */
  private def clientFor(caFile: String): HttpClient =
    // FORCED TO HTTP/1.1 (measured against a real `kubectl proxy`):
    // the JDK client's default HTTP/2-with-upgrade attempt reads a
    // 201 Created answered mid-upgrade as "invalid upgrade response"
    // and fails the whole request. `kubectl proxy` speaks HTTP/1.1
    // only, and so, in practice, does every API server this seam is
    // ever asked to reach.
    val b = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1)
    if caFile.nonEmpty then
      val cf = CertificateFactory.getInstance("X.509")
      val in = java.nio.file.Files.newInputStream(java.nio.file.Paths.get(caFile))
      val cert = try cf.generateCertificate(in) finally in.close()
      val ks = KeyStore.getInstance(KeyStore.getDefaultType)
      ks.load(null, null)
      ks.setCertificateEntry("ca", cert)
      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(ks)
      val ctx = SSLContext.getInstance("TLS")
      ctx.init(null, tmf.getTrustManagers, null)
      b.sslContext(ctx): Unit
    b.build()
