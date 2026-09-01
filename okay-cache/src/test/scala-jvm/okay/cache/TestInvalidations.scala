package okay.cache

import okay.{!, Async}
import okay.given
import okay.persist.{MemoryStore, Policy}

/**
 * Cross-node regime 2: node A's write invalidates node B's cache
 * through the TOPIC, and — the trade that justifies a topic over
 * pub/sub — a node that was DOWN replays and converges.
 */
class TestInvalidations extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  test("A's write reaches B: drain, then B's next read reloads") {
    val topic = MemoryStore().topic("__invalidations", 1, Policy())
    var truth = Map("okay" -> 100L)
    def load(k: String): Long ! Async = okay.async(truth(k))

    val a = Cache.memory[String, Long](Regime.Invalidated, 64)
    val b = Cache.memory[String, Long](Regime.Invalidated, 64)
    assertEquals(run(a.getOrLoad("okay")(load)), 100L)
    assertEquals(run(b.getOrLoad("okay")(load)), 100L)

    // node A commits, invalidates ITSELF, and appends the event
    truth = Map("okay" -> 150L)
    val _ = run(WriteThrough.write(a, "okay")(okay.async(())))
    Invalidations.append(topic, "okay")

    // B has not drained yet: the honest window, cross-node edition
    assertEquals(run(b.getOrLoad("okay")(load)), 100L)
    val next = run(Invalidations.drain(topic, b, identity, 0))
    assertEquals(run(b.getOrLoad("okay")(load)), 150L)
    assertEquals(run(a.getOrLoad("okay")(load)), 150L)

    // nothing new: the offset stands still
    assertEquals(run(Invalidations.drain(topic, b, identity, next)), next)
  }

  test("a node that was down replays the topic and converges") {
    val topic = MemoryStore().topic("__invalidations", 1, Policy())
    var truth = Map("k1" -> 1L, "k2" -> 2L)
    def load(k: String): Long ! Async = okay.async(truth(k))

    val down = Cache.memory[String, Long](Regime.Invalidated, 64)
    assertEquals(run(down.getOrLoad("k1")(load)), 1L)
    assertEquals(run(down.getOrLoad("k2")(load)), 2L)

    // the world moves while the node is down
    truth = Map("k1" -> 10L, "k2" -> 20L)
    Invalidations.append(topic, "k1")
    Invalidations.append(topic, "k2")

    // it comes back knowing only offset 0 — replay converges it
    val next = run(Invalidations.drain(topic, down, identity, 0))
    assertEquals(next, 2L)
    assertEquals(run(down.getOrLoad("k1")(load)), 10L)
    assertEquals(run(down.getOrLoad("k2")(load)), 20L)
  }
}
