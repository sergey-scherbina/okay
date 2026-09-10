package okay.ops

import okay.persist.MemoryStore

/**
 * The ops surface describes itself, and the description is the one
 * the router dispatches (specs/optics-outside.md, stage 4).
 *
 * This is the law the stage exists for. A deployment names a probe
 * path and has no `Store` to build a router with, so the paths live
 * as values apart from the router — and the moment they do, they can
 * drift from it. They cannot now: this test is what makes `Ops.paths`
 * an assertion about the code rather than a comment about it.
 */
class TestOpsSurface extends munit.FunSuite {

  test("paths is exactly what the router dispatches") {
    val store = MemoryStore()
    assertEquals(Ops.router(store).describe.map(_._2).toSet, Ops.paths)
  }

  test("the surface is the four documented endpoints, and they are GET") {
    assertEquals(Ops.paths, Set("/healthz", "/readyz", "/stats", "/metrics"))
    val store = MemoryStore()
    assertEquals(Ops.router(store).describe.map(_._1).toSet, Set(okay.http.Method.Get))
    assertEquals(Ops.router(store).describe.length, Ops.paths.size)
  }

  test("a probe path is a value a deployment can read without a store") {
    // the whole point: no MemoryStore in sight
    assertEquals(Ops.healthz.describe, "/healthz")
    assertEquals(Ops.readyz.describe, "/readyz")
  }
}
