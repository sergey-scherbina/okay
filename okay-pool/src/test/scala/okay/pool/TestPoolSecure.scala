package okay.pool

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Body, Method, Request}
import okay.resilience.Discovery
import okay.security.given

/**
 * specs/cluster-pool.md, stage 4: the capability at the submission
 * door, and the pure "is this pool secured at all" predicate. The
 * mTLS handshake itself needs a real certificate, so it is
 * `TestPoolSecureLive`; everything here needs neither a subprocess
 * nor a socket -- the capability's root key travels as a `file:`
 * reference (a temp file), the same seam `okay-tls`'s own suite
 * already trusts for its key, rather than mutating the real process
 * environment for a test.
 */
class TestPoolSecure extends munit.FunSuite {
  CountJobs.install()

  private val noPeers: Discovery = Discovery.static(Map.empty)
  private val rootKeyText = "the-pool's-shared-root-key"
  private val rootKey = rootKeyText.getBytes(java.nio.charset.StandardCharsets.UTF_8)

  private def keyFile(): okay.conf.Secret =
    val p = java.nio.file.Files.createTempFile("okaypool-cap-key", ".txt")
    java.nio.file.Files.writeString(p, rootKeyText): Unit
    okay.conf.Secret(s"file:$p")

  // ---- secured() -------------------------------------------------------

  test("secured: neither TLS nor a capability nor the override -- refused") {
    assert(!Pool.secured(PoolConf()))
  }

  test("secured: a capability key alone is enough") {
    assert(Pool.secured(PoolConf(capabilityKey = okay.conf.Secret("env:X"))))
  }

  test("secured: a TLS cert alone is enough") {
    assert(Pool.secured(PoolConf(tlsCert = "/some/cert.pem")))
  }

  test("secured: the explicit override is enough, on purpose") {
    assert(Pool.secured(PoolConf(insecure = true)))
  }

  // ---- the capability at the submission door ---------------------------

  test("no capabilityKey configured: the door is open, as it always was") {
    val store = SharedStore()
    val body = Json.print(Json.JObj(Vector("params" -> Json.JNum(10))))
    val res = Routes.router(PoolConf(), noPeers, store(_), () => true)
      .routes(Request(Method.Post, s"/pool/jobs/${CountJob.name}", Nil, Body.Text(body))).runWith
    assertEquals(res.status, 202)
  }

  test("capabilityKey configured, no Authorization header: 401, and the job name never leaks") {
    val store = SharedStore()
    val body = Json.print(Json.JObj(Vector("params" -> Json.JNum(10))))
    val conf = PoolConf(capabilityKey = keyFile())
    val router = Routes.router(conf, noPeers, store(_), () => true)
    val res = router.routes(Request(Method.Post, "/pool/jobs/no.such.job", Nil, Body.Text(body))).runWith
    assertEquals(res.status, 401)
    val text = new String(okay.http.Http.bytes(res).runWith.toArray, java.nio.charset.StandardCharsets.UTF_8)
    assert(!text.contains("no.such.job"), text)
  }

  test("capabilityKey configured, a VALID capability: the submission goes through") {
    val store = SharedStore()
    val cap = okay.security.Capability.issue(rootKey, "test-submitter")
    val body = Json.print(Json.JObj(Vector("params" -> Json.JNum(10))))
    val conf = PoolConf(capabilityKey = keyFile())
    val router = Routes.router(conf, noPeers, store(_), () => true)
    val req = Request(Method.Post, s"/pool/jobs/${CountJob.name}",
      Vector("authorization" -> s"Bearer ${cap.encoded}"), Body.Text(body))
    val res = router.routes(req).runWith
    assertEquals(res.status, 202)
  }

  test("capabilityKey configured, a capability signed with the WRONG key: 401") {
    val store = SharedStore()
    val wrong = "not-the-pool's-key".getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val cap = okay.security.Capability.issue(wrong, "test-submitter")
    val body = Json.print(Json.JObj(Vector("params" -> Json.JNum(10))))
    val conf = PoolConf(capabilityKey = keyFile())
    val router = Routes.router(conf, noPeers, store(_), () => true)
    val req = Request(Method.Post, s"/pool/jobs/${CountJob.name}",
      Vector("authorization" -> s"Bearer ${cap.encoded}"), Body.Text(body))
    val res = router.routes(req).runWith
    assertEquals(res.status, 401)
  }

  test("capabilityKey configured, a garbage token: 401, not an exception") {
    val store = SharedStore()
    val body = Json.print(Json.JObj(Vector("params" -> Json.JNum(10))))
    val conf = PoolConf(capabilityKey = keyFile())
    val router = Routes.router(conf, noPeers, store(_), () => true)
    val req = Request(Method.Post, s"/pool/jobs/${CountJob.name}",
      Vector("authorization" -> "Bearer not-a-real-token"), Body.Text(body))
    val res = router.routes(req).runWith
    assertEquals(res.status, 401)
  }
}
