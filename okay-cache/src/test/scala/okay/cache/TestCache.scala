package okay.cache

import okay.{!, Async}
import okay.given

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
  def run[A](prog: A ! Async): A =
    Async.runAsync(prog).value match
      case Some(t) => t.get
      case None => fail("the test program did not complete synchronously")

  test("a budget expires: within N the value serves, after N it is a miss and reloads") {
    now = 0
    val c = mk(Regime.Budget(1000))
    var loaded = 0
    def load(k: String): String ! Async = okay.async { loaded += 1; s"v-$k" }

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
    run(c.put("a", "one"))
    now = Long.MaxValue / 2
    assertEquals(run(c.get("a")), Some("one"))
    run(c.put("a", "two"))
    assertEquals(run(c.get("a")), Some("two"))
    run(c.invalidate("a"))
    assertEquals(run(c.get("a")), None)
    var loaded = 0
    assertEquals(run(c.getOrLoad("a") { k => okay.async { loaded += 1; "three" } }), "three")
    assertEquals(loaded, 1, "the read after invalidate did not reload")
  }

  test("eviction: at maxEntries the least-recently-USED entry leaves") {
    val c = mk(Regime.Invalidated, max = 3)
    run(c.put("a", "1")); run(c.put("b", "2")); run(c.put("c", "3"))
    // touch a: b becomes the least recently used
    assertEquals(run(c.get("a")), Some("1"))
    run(c.put("d", "4"))
    assertEquals(run(c.get("b")), None, "the LRU entry survived the bound")
    assertEquals(run(c.get("a")), Some("1"))
    assertEquals(run(c.get("c")), Some("3"))
    assertEquals(run(c.get("d")), Some("4"))
    assertEquals(c.stats.evictions, 1L)
    assertEquals(c.stats.size, 3)
  }

  test("negative caching: None is a value under the same budget, a hit while fresh") {
    now = 0
    val c = Cache.memory[String, Option[String]](Regime.Budget(500), 10, () => clock())
    var loads = 0
    def look(k: String): Option[String] ! Async = okay.async { loads += 1; None }

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
    var n = 0
    def load(k: String): String ! Async = okay.async { n += 1; k }
    run(c.getOrLoad("x")(load))   // miss + load
    run(c.getOrLoad("x")(load))   // hit
    run(c.getOrLoad("y")(load))   // miss + load
    assertEquals(run(c.get("z")), None) // miss
    val s = c.stats
    assertEquals(s.loads, 2L)
    assertEquals(s.hits, 1L)
    assertEquals(s.misses, 3L)
    assertEquals(s.size, 2)
  }

  test("a failing loader propagates and does not poison the key") {
    val c = mk(Regime.Invalidated)
    intercept[RuntimeException](
      run(c.getOrLoad("k")(_ => okay.async[String](throw RuntimeException("boom")))))
    // the claim is released: the next load succeeds
    assertEquals(run(c.getOrLoad("k")(_ => okay.async("fine"))), "fine")
  }

  test("construction demands a bound") {
    intercept[IllegalArgumentException](Cache.memory[String, String](Regime.Invalidated, 0))
  }
}
