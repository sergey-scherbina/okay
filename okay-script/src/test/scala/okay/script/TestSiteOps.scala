package okay.script

import okay.http.Request
import java.nio.file.Files

/**
 * A deployment cannot name a probe path this Site does not serve
 * (specs/optics-outside.md, stage 4).
 *
 * Nothing could state that before. `ScriptDeploy` wrote `/healthz`
 * as a literal and `Site` served one as a different literal; rename
 * either and Kubernetes would have found out first, by restarting the
 * pod. The paths come from `Site.Ops` now, and these two tests are
 * what hold the ends together.
 */
class TestSiteOps extends munit.FunSuite {

  test("Site.Ops.paths is exactly what opsRouter dispatches") {
    val root = Files.createTempDirectory("okay-script-ops-")
    Files.writeString(root.resolve("index.md"), "hello\n"): Unit
    val site = Site(root)
    try assertEquals(site.opsRouter.describe.map(_._2).toSet, Site.Ops.paths)
    finally site.close()
  }

  test("every probe path the deployment names is one the Site serves") {
    val health = ScriptDeploy.system.services.head.health
    assert(Site.Ops.paths.contains(health.livenessPath), health.livenessPath)
    assert(Site.Ops.paths.contains(health.readinessPath), health.readinessPath)
  }

  test("an ops route is not defeated by a query string") {
    val root = Files.createTempDirectory("okay-script-ops-q-")
    Files.writeString(root.resolve("index.md"), "hello\n"): Unit
    val site = Site(root)
    try
      assert(site.opsRoutes.isDefinedAt(Request.get("/healthz")))
      assert(site.opsRoutes.isDefinedAt(Request.get("/healthz?probe=1")))
      assert(!site.opsRoutes.isDefinedAt(Request.get("/readyz")))
    finally site.close()
  }
}
