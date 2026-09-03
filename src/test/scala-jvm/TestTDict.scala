package okay

/** specs/stm.md, okay-stm-collections — JVM-only: real concurrent
 * threads racing the same key, not just a single-threaded proof. */
class TestTDict extends munit.FunSuite {

  test("computeIfAbsent: 64 threads racing one missing key all observe the SAME winning value") {
    // NOT "creates exactly once": mk inherits TRef.modify's "f may
    // run more than once" rule under CAS contention (found here) —
    // what's actually guaranteed is a single, consistent STORED
    // value, never a torn or duplicated one
    val d = TDict.empty[String, Int]
    val seen = java.util.concurrent.ConcurrentHashMap.newKeySet[Int]()
    val threads = (0 until 64).map(i => Thread.ofVirtual().start { () =>
      seen.add(d.computeIfAbsent("k") { i }): Unit
    })
    threads.foreach(_.join())
    assertEquals(seen.size, 1, s"every racer must observe the SAME winning value: $seen")
    assertEquals(d.get("k"), Some(seen.iterator().next()))
  }

  test("updateAt: 100 threads each adding their own element lose none") {
    val d = TDict.empty[String, Set[Int]]
    val threads = (0 until 100).map(i => Thread.ofVirtual().start { () =>
      d.updateAt("k")(_.getOrElse(Set.empty) + i): Unit
    })
    threads.foreach(_.join())
    assertEquals(d.get("k"), Some((0 until 100).toSet), "no concurrent update lost")
  }

  test("TList.append: 200 threads, every element survives, none duplicated") {
    val l = TList.empty[Int]
    val threads = (0 until 200).map(i => Thread.ofVirtual().start { () => l.append(i) })
    threads.foreach(_.join())
    assertEquals(l.snapshot.toSet, (0 until 200).toSet)
    assertEquals(l.size, 200, "no append lost or duplicated")
  }
}
