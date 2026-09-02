package okay.demo

import okay.deploy.Deploy

/** the committed okay-demo/deploy IS the rendered DemoDeploy.spec —
 * a hand edit or a stale regeneration fails here, by file name */
class TestDemoDeploy extends munit.FunSuite:
  test("okay-demo/deploy does not drift from DemoDeploy.spec") {
    assertEquals(Deploy.drift(DemoDeploy.spec, Deploy.repoRoot()), Vector.empty)
  }
