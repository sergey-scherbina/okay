package okay

import java.util.concurrent.atomic.AtomicBoolean

/**
 * A join's cancel scope stays REACHABLE while the join runs
 * (merge-shared-scope-gc-release). A scope has a collector door, the
 * backstop for an ABANDONED program: when the scope is unreachable, its
 * release runs. A join that enters its scope and never names it again
 * leaves nothing holding it on a plain `runWith` (no drive, no fiber
 * handler), so a collection in the middle of the run releases it, and
 * the release closes the channel under producers still sending.
 * `Source.zip` lost pairs that way (source-zip-lost-pairs); every join
 * that opens a scope is held to the same law here.
 */
class TestMergeScopeReachable extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  /** `rounds` runs of `run` beside a thread that collects without pause:
   * enough rounds for the consumer's loop to be compiled (the interpreter
   * keeps a dead local that holds the scope; compiled code does not) */
  private def underCollection(name: String, rounds: Int)(run: () => (Int, Int)): Unit =
    val stop = AtomicBoolean(false)
    val gc = Thread.ofPlatform().daemon().start(() => while !stop.get do { System.gc(); Thread.sleep(1) })
    try
      for r <- 1 to rounds do
        val before = Source.mergeReleases.get
        val (got, want) = run()
        val released = Source.mergeReleases.get - before
        if got != want || released != 0 then note(s"$name round $r: $got of $want, $released release(s)")
        assertEquals(got, want, s"$name round $r: the join ended early")
        assertEquals(released, 0L, s"$name round $r: a join that ran to its end was released mid-run")
    finally
      stop.set(true)
      gc.join()

  private val n = 200L

  test("Merge.Shared: a collection mid-run does not release the merge") {
    given Merge = Merge.Shared
    underCollection("Shared", 3000): () =>
      ((Source.range(0, n) merge Source.range(n, 2 * n)).runCollect.runWith.size, 2 * n.toInt)
  }

  test("Merge.Ready: a collection mid-run does not release the merge") {
    given Merge = Merge.Ready
    underCollection("Ready", 3000): () =>
      ((Source.range(0, n) merge Source.range(n, 2 * n)).runCollect.runWith.size, 2 * n.toInt)
  }
}
