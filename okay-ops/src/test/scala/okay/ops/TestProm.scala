package okay.ops

import okay.persist.{MemoryStore, Offsets, Policy}

/** Prometheus text, pinned (specs/ops.md): a pure mapping of
 * Store.Stats, tested as a golden string like Otlp.body's JSON. */
class TestProm extends munit.FunSuite:

  test("Store.Stats with two topics, several partitions, renders stable Prometheus text") {
    val store = MemoryStore()
    val a = store.topic("alpha", 2)
    a.append(0, Array(1), Array(1, 2, 3), okay.persist.Ack.Durable)
    a.append(0, Array(1), Array(1, 2, 3), okay.persist.Ack.Durable)
    a.append(1, Array(2), Array(9), okay.persist.Ack.Durable)
    store.topic("beta", 1)
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
    store.topic("t", 1)
    assert(!Prom.render(store.stats).contains("okay_persist_consumer_lag"))
  }

  test("lagOf reports end-minus-committed per group, named and quoted safely") {
    val store = MemoryStore()
    val t = store.topic("t\"weird", 1, Policy.default)
    t.append(0, Array(1), Array(1), okay.persist.Ack.Durable)
    t.append(0, Array(1), Array(2), okay.persist.Ack.Durable)
    t.append(0, Array(1), Array(3), okay.persist.Ack.Durable)
    val offs = Offsets(store)
    offs.commit("workers", "t\"weird", 0, 1L, okay.persist.Ack.Durable)
    val out = Prom.render(store.stats, Vector(("workers", offs, Vector(t))))
    assert(out.contains("""okay_persist_consumer_lag{group="workers",topic="t\"weird"} 2"""), out)
  }
