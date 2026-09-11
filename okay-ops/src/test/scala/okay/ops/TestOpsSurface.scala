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

  /** the repo root, without a dependency on okay-deploy for three lines */
  private def repoRoot: java.nio.file.Path =
    Iterator.iterate(java.nio.file.Path.of(".").toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .find(p => java.nio.file.Files.exists(p.resolve("build.sbt")))
      .getOrElse(fail("no build.sbt above the working directory"))

  test("the documented surface is the rendered one (no drift)") {
    // the first consumer that makes DESCRIBE load-bearing: the block in
    // the module's documentation is generated from the entries that
    // dispatch, and this asserts the committed file still matches. An
    // endpoint cannot be served and undocumented, or documented and
    // unserved — which is exactly the drift stage 4 found between Ops
    // and Site, caught here by a test rather than by a reader
    val doc = java.nio.file.Files.readString(repoRoot.resolve("docs/modules/okay-ops.md"))
    val start = doc.indexOf("<!-- generated: Ops.router(store).markdown -->")
    val end = doc.indexOf("<!-- /generated -->")
    assert(start >= 0 && end > start, "the generated block is missing from docs/modules/okay-ops.md")
    val committed = doc.substring(doc.indexOf("\n", start) + 1, end).trim
    assertEquals(committed, Ops.router(MemoryStore()).markdown)
  }
}
