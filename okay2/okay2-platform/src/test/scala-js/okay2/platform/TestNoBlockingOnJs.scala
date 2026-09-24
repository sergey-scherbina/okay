package okay2.platform

/** Scala.js has NO `CanBlock` (okay2-cross): the platform installs
 * `PlatformDefaults`, not `BlockingDefaults`, so every blocking door is
 * a compile error with `CanBlock`'s own message, and the callback doors
 * compile — a refusal is the only thing that can prove the absence */
class TestNoBlockingOnJs extends munit.FunSuite {

  test("a blocking join and runWith do not compile on JS; joinAsync and runAsync do") {
    val join = compileErrors("""
      import okay2._, okay2.async._, okay2.platform._
      Async.spawn(Async(1)).join()""")
    assert(join.contains("no CanBlock capability in scope"), join)
    val run = compileErrors("""
      import okay2._, okay2.async._, okay2.platform._
      Effects.runFree(Async(1))""")
    assert(run.nonEmpty, "a blocking run compiled on JS")
    assertEquals(compileErrors("""
      import okay2._, okay2.async._, okay2.platform._
      Async.runAsync(Async.spawn(Async(1)).joinAsync)"""), "")
  }
}
