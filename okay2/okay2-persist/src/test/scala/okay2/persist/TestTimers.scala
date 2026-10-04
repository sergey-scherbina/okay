package okay2.persist

import munit.FunSuite

/**
 * DURABLE DEADLINES (okay-persist's TestTimers; workflow-timers): the
 * half of a sleeping workflow that lives OUTSIDE its journal.
 */
class TestTimers extends FunSuite {

  test("armed, due, and the newest arm wins") {
    val t = Timers.over(new MemoryStore)
    t.arm("a", 100L)
    t.arm("b", 300L)
    t.arm("a", 500L)          // moved: a snooze, or a re-computed deadline

    assertEquals(t.armed, Map("a" -> 500L, "b" -> 300L))
    assertEquals(t.due(200L), Nil, "a moved deadline still fired")
    assertEquals(t.due(300L), List("b"))
    assertEquals(t.due(500L), List("a", "b"))
  }

  test("a disarmed timer is gone, and staying gone survives a re-read") {
    val store = new MemoryStore
    val t = Timers.over(store)
    t.arm("a", 100L)
    t.arm("b", 100L)
    t.disarm("a")

    assertEquals(t.due(1000L), List("b"))
    // a NEW reader over the same topic agrees — the tombstone is a record
    assertEquals(Timers.over(store).due(1000L), List("b"))
  }

  test("re-arming after a disarm brings it back") {
    val t = Timers.over(new MemoryStore)
    t.arm("a", 100L)
    t.disarm("a")
    t.arm("a", 200L)
    assertEquals(t.due(1000L), List("a"))
  }

  test("the deadlines are DURABLE: another process reads the same set") {
    val store = new MemoryStore
    Timers.over(store).arm("later", 9000L)
    Timers.over(store).arm("soon", 10L)

    // ---- the process dies; a third one reads the topic
    val fresh = Timers.over(store)
    assertEquals(fresh.armed, Map("later" -> 9000L, "soon" -> 10L))
    assertEquals(fresh.due(100L), List("soon"))
  }

  test("losing the whole timer topic loses no correctness, only wake-ups") {
    // a deadline is operational data ABOUT a run, never part of it
    val store = new MemoryStore
    val t = Timers.over(store)
    t.arm("w-1", 10L)
    assertEquals(t.due(100L), List("w-1"))

    val lost = Timers.over(new MemoryStore)
    assertEquals(lost.armed, Map.empty[String, Long])
    // and re-arming is all it takes to get the wake-up back
    lost.arm("w-1", 10L)
    assertEquals(lost.due(100L), List("w-1"))
  }
}
