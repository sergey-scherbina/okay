package okay

/**
 * specs/stm.md, okay-stm-collections — cross (JVM/JS/Native), no
 * platform-specific code: TDict/TList are built on TRef alone.
 */
class TestTDictCross extends munit.FunSuite {

  test("get/put/remove: the plain shape") {
    val d = TDict.empty[String, Int]
    assertEquals(d.get("a"), None)
    d.put("a", 1)
    assertEquals(d.get("a"), Some(1))
    assertEquals(d.snapshot, Map("a" -> 1))
    d.remove("a")
    assertEquals(d.get("a"), None)
  }

  test("computeIfAbsent creates once, answers the same value on a second call") {
    val d = TDict.empty[String, Int]
    var creations = 0
    val v1 = d.computeIfAbsent("k") { creations += 1; 42 }
    val v2 = d.computeIfAbsent("k") { creations += 1; 99 }
    assertEquals(v1, 42)
    assertEquals(v2, 42, "the SECOND call must answer the EXISTING value, not create again")
    assertEquals(creations, 1)
  }

  test("updateAt folds over the current value (or None), atomically") {
    val d = TDict.empty[String, Set[String]]
    val v1 = d.updateAt("k")(_.getOrElse(Set.empty) + "a")
    val v2 = d.updateAt("k")(_.getOrElse(Set.empty) + "b")
    assertEquals(v1, Set("a"))
    assertEquals(v2, Set("a", "b"))
    assertEquals(d.get("k"), Some(Set("a", "b")))
  }

  test("clear empties the dict; size/isEmpty agree with snapshot") {
    val d = TDict.empty[String, Int]
    d.put("a", 1); d.put("b", 2)
    assertEquals(d.size, 2)
    assert(!d.isEmpty)
    d.clear()
    assertEquals(d.size, 0)
    assert(d.isEmpty)
    assertEquals(d.snapshot, Map.empty[String, Int])
  }

  test("TList: append in order, snapshot is a stable Vector") {
    val l = TList.empty[Int]
    assert(l.isEmpty)
    l.append(1); l.append(2); l.append(3)
    assertEquals(l.snapshot, Vector(1, 2, 3))
    assertEquals(l.size, 3)
    l.clear()
    assert(l.isEmpty)
  }
}
