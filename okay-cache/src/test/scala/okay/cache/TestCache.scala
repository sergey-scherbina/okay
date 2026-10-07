package okay.cache

import okay.{!, +:, Async, AsyncCont, Op, Pure}
/**
 * The cross-platform half of the contract (specs/cache.md,
 * Behavior): budgets, invalidation, LRU eviction, negative caching,
 * stats — plain state, provable on every platform. The concurrent
 * single-flight battery is JVM-only (TestCacheFlight).
 */
class TestCache extends munit.FunSuite {

  var now = 0L
  def clock(): Long = now

  def mk(regime: Regime, max: Int = 100): Cache[String, String] =
    Cache.memory(regime, max, () => clock())

  /** cross-platform runner: these programs are Run-only (plus the
   * flight await, which completes during registration when the
   * loader is synchronous), so the drive finishes inline — no
   * CanBlock, which is what lets this suite run on JS */
  def run[A](prog: A ! (Async +: Pure)): A =
    AsyncCont.runAsync(prog).value match
      case Some(t) => t.get
      case None => fail("the test program did not complete synchronously")
  def runOp[A](op: Op[Async, A]): A = run(op.at[Async +: Pure])

  test("a budget expires: within N the value serves, after N it is a miss and reloads") {
    now = 0
    val c = mk(Regime.Budget(1000))
    var loaded = 0
    def load(k: String): String ! (Async +: Pure) = AsyncCont.async { loaded += 1; s"v-$k" }.at

    assertEquals(run(c.getOrLoad("a")(load)), "v-a")
    assertEquals(loaded, 1)
    now = 1000  // the edge of the budget: still fresh
    assertEquals(run(c.getOrLoad("a")(load)), "v-a")
    assertEquals(loaded, 1)
    now = 1001  // past it: a miss, and the loader runs again
    assertEquals(run(c.getOrLoad("a")(load)), "v-a")
    assertEquals(loaded, 2)
  }

  test("Invalidated regime never expires by time; invalidate removes, put replaces") {
    now = 0
    val c = mk(Regime.Invalidated)
    runOp(c.put("a", "one"))
    now = Long.MaxValue / 2
    assertEquals(runOp(c.get("a")), Some("one"))
    runOp(c.put("a", "two"))
    assertEquals(runOp(c.get("a")), Some("two"))
    runOp(c.invalidate("a"))
    assertEquals(runOp(c.get("a")), None)
    var loaded = 0
    assertEquals(run(c.getOrLoad("a") { _ => AsyncCont.async { loaded += 1; "three" }.at }), "three")
    assertEquals(loaded, 1, "the read after invalidate did not reload")
  }

  test("eviction: at maxEntries the least-recently-USED entry leaves") {
    val c = mk(Regime.Invalidated, max = 3)
    runOp(c.put("a", "1")); runOp(c.put("b", "2")); runOp(c.put("c", "3"))
    // touch a: b becomes the least recently used
    assertEquals(runOp(c.get("a")), Some("1"))
    runOp(c.put("d", "4"))
    assertEquals(runOp(c.get("b")), None, "the LRU entry survived the bound")
    assertEquals(runOp(c.get("a")), Some("1"))
    assertEquals(runOp(c.get("c")), Some("3"))
    assertEquals(runOp(c.get("d")), Some("4"))
    assertEquals(c.stats.evictions, 1L)
    assertEquals(c.stats.size, 3)
  }

  test("negative caching: None is a value under the same budget, a hit while fresh") {
    now = 0
    val c = Cache.memory[String, Option[String]](Regime.Budget(500), 10, () => clock())
    var loads = 0
    def look(@scala.annotation.unused k: String): Option[String] ! (Async +: Pure) = AsyncCont.async { loads += 1; None }.at

    assertEquals(run(c.getOrLoad("missing")(look)), None)
    assertEquals(run(c.getOrLoad("missing")(look)), None)
    assertEquals(loads, 1, "the absent answer was not cached")
    assert(c.stats.hits >= 1L)
    now = 501
    assertEquals(run(c.getOrLoad("missing")(look)), None)
    assertEquals(loads, 2, "the negative entry ignored its budget")
  }

  test("stats match the scenario: hits, misses, loads") {
    val c = mk(Regime.Invalidated)
    def load(k: String): String ! (Async +: Pure) = AsyncCont.async { k }.at
    run(c.getOrLoad("x")(load)): Unit // miss + load
    run(c.getOrLoad("x")(load)): Unit // hit
    run(c.getOrLoad("y")(load)): Unit // miss + load
    assertEquals(runOp(c.get("z")), None) // miss
    val s = c.stats
    assertEquals(s.loads, 2L)
    assertEquals(s.hits, 1L)
    assertEquals(s.misses, 3L)
    assertEquals(s.size, 2)
  }

  test("a failing loader propagates and does not poison the key") {
    val c = mk(Regime.Invalidated)
    intercept[RuntimeException](
      run(c.getOrLoad("k")(_ => AsyncCont.async[String](throw RuntimeException("boom")).at))): Unit
    // the claim is released: the next load succeeds
    assertEquals(run(c.getOrLoad("k")(_ => AsyncCont.async("fine").at)), "fine")
  }

  test("construction demands a bound") {
    intercept[IllegalArgumentException](Cache.memory[String, String](Regime.Invalidated, 0))
  }
}
