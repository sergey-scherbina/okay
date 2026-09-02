package okay.ops

import okay.persist.MemoryStore

class TestHealth extends munit.FunSuite:

  test("an open store is live and ready") {
    val store = MemoryStore()
    store.topic("t", 1)
    assertEquals(Health.of(store), Health(live = true, ready = true))
  }

  test("a store whose stats call throws is neither live nor ready, named") {
    val boom = new okay.persist.Store:
      def topic(name: String, partitions: Int, policy: okay.persist.Policy): okay.persist.Topic = ???
      def topics: Vector[String] = Vector.empty
      def stats: okay.persist.Store.Stats = throw RuntimeException("disk is gone")
    val h = Health.of(boom)
    assertEquals(h, Health(live = false, ready = false, reason = Some("disk is gone")))
  }
