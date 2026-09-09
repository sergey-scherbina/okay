package okay.ops

import okay.persist.{MemoryStore, Offsets, Policy}

/** Prometheus text, pinned (specs/ops.md): a pure mapping of
 * Store.Stats, tested as a golden string like Otlp.body's JSON. */
class TestProm extends munit.FunSuite:

  test("Store.Stats with two topics, several partitions, renders stable Prometheus text") {
    val store = MemoryStore()
    val a = store.topic("alpha", 2)
    val _ = a.append(0, Array(1), Array(1, 2, 3), okay.persist.Ack.Durable)
    val _ = a.append(0, Array(1), Array(1, 2, 3), okay.persist.Ack.Durable)
    val _ = a.append(1, Array(2), Array(9), okay.persist.Ack.Durable)
    val _ = store.topic("beta", 1)
    val out = Prom.render(store.stats)
    assert(out.contains("# HELP okay_persist_partition_begin"))
    assert(out.contains("# TYPE okay_persist_partition_begin gauge"))
    assert(out.contains("""okay_persist_partition_end{topic="alpha",partition="0"} 2"""))
    assert(out.contains("""okay_persist_partition_end{topic="alpha",partition="1"} 1"""))
    assert(out.contains("""okay_persist_partition_end{topic="beta",partition="0"} 0"""))
    assert(out.contains("""okay_persist_partition_bytes{topic="alpha",partition="0"}"""))
    assert(out.contains("""okay_persist_partition_segments{topic="beta",partition="0"} 0"""))
    assert(out.endsWith("\n"))
  }

  test("no lagOf given: no consumer_lag metric appears at all") {
    val store = MemoryStore()
    val _ = store.topic("t", 1)
    assert(!Prom.render(store.stats).contains("okay_persist_consumer_lag"))
  }

  test("lagOf reports end-minus-committed per group, named and quoted safely") {
    val store = MemoryStore()
    val t = store.topic("t\"weird", 1, Policy.default)
    val _ = t.append(0, Array(1), Array(1), okay.persist.Ack.Durable)
    val _ = t.append(0, Array(1), Array(2), okay.persist.Ack.Durable)
    val _ = t.append(0, Array(1), Array(3), okay.persist.Ack.Durable)
    val offs = Offsets(store)
    offs.commit("workers", "t\"weird", 0, 1L, okay.persist.Ack.Durable)
    val out = Prom.render(store.stats, Vector(("workers", offs, Vector(t))))
    assert(out.contains("""okay_persist_consumer_lag{group="workers",topic="t\"weird"} 2"""), out)
  }

  test("guards: breaker, bulkhead and limiter stats render as rows named by the piece") {
    import okay.resilience.{Breaker, Bulkhead, Limiter}
    val b = Breaker("pay", failures = 1, openMillis = 10)
    val h = Bulkhead("pay", permits = 8, queue = 2)
    val l = Limiter("in\"bound", ratePerSecond = 5, burst = 5)
    val out = Prom.guards(Vector(b, h, l))
    assert(out.contains("# TYPE okay_breaker_state gauge"))
    assert(out.contains("""okay_breaker_state{name="pay"} 0"""), out)
    assert(out.contains("""okay_breaker_calls_total{name="pay"} 0"""))
    assert(out.contains("""okay_bulkhead_permits{name="pay"} 8"""))
    assert(out.contains("""okay_bulkhead_in_flight{name="pay"} 0"""))
    assert(out.contains("""okay_limiter_keys{name="in\"bound"} 0"""), out)
    assert(out.contains("# TYPE okay_limiter_rejected_total counter"))
    assert(out.endsWith("\n"))
    assertEquals(Prom.guards(Vector.empty), "")
  }

  test("pools and sagas: Pool.Stats and Saga.Status render as named rows; empty inputs render nothing") {
    val st = okay.sql.Pool.Stats(size = 4, idle = 1, busy = 2, waiting = 3, created = 5L, closed = false)
    val out = Prom.pools(Vector(("pg", () => st)))
    assert(out.contains("okay_pool_size{name=\"pg\"} 4"), out)
    assert(out.contains("okay_pool_idle{name=\"pg\"} 1"), out)
    assert(out.contains("okay_pool_busy{name=\"pg\"} 2"), out)
    assert(out.contains("okay_pool_waiting{name=\"pg\"} 3"), out)
    assert(out.contains("okay_pool_created_total{name=\"pg\"} 5"), out)
    assertEquals(Prom.pools(Vector.empty), "")
    val s = okay.persist.Saga.Status("o1", "compensating", Vector("reserve", "charge"), Vector("charge"), None, Some("ship refused"))
    val sg = Prom.sagas(Vector(() => s))
    assert(sg.contains("okay_saga_phase{id=\"o1\",phase=\"compensating\"} 1"), sg)
    assert(sg.contains("okay_saga_steps_done{id=\"o1\"} 2"), sg)
    assert(sg.contains("okay_saga_steps_undone{id=\"o1\"} 1"), sg)
    assertEquals(Prom.sagas(Vector.empty), "")
  }
