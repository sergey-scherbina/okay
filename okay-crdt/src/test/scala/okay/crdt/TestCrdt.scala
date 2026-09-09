package okay.crdt

import okay.{Hlc, Uid}

/**
 * What each type MEANS, beyond obeying the laws. The laws say
 * replicas converge; these say they converge on the right answer.
 */
class TestCrdt extends munit.FunSuite {

  private val a = NodeId("alice")
  private val b = NodeId("bob")

  test("GCounter: two replicas counting apart agree, and redelivery costs nothing") {
    val g = summon[Crdt[GCounter]]
    val ra = GCounter.empty.inc(a).inc(a)     // alice counted twice
    val rb = GCounter.empty.inc(b, 3)         // bob counted three
    val merged = g.merge(ra, rb)
    assertEquals(merged.value, 5L)
    // the same message twice, and in the other order: still five
    assertEquals(g.merge(merged, rb).value, 5L)
    assertEquals(g.merge(rb, ra).value, 5L)
  }

  test("GCounter refuses to go down, because that is what grow-only means") {
    val _ = intercept[IllegalArgumentException](GCounter.empty.inc(a, -1))
  }

  test("PNCounter: down and up, and still convergent") {
    val g = summon[Crdt[PNCounter]]
    val ra = PNCounter.empty.inc(a, 10).dec(a, 3)
    val rb = PNCounter.empty.dec(b, 2)
    assertEquals(g.merge(ra, rb).value, 5L)
    assertEquals(g.merge(rb, ra).value, 5L)
  }

  test("LwwRegister: a write that SAW another wins, whatever the wall clocks say") {
    // bob's machine is an hour behind alice's — the case a plain
    // timestamp gets wrong and loses the newer write
    val ca = Hlc.at(() => 1_700_000_000_000L)
    val cb = Hlc.at(() => 1_700_000_000_000L - 3_600_000L)
    val g = summon[Crdt[LwwRegister[String]]]

    val first = LwwRegister.write("alice's", ca, a)
    val reply = LwwRegister.after("bob's, after seeing alice's", first, cb, b)

    assertEquals(g.merge(first, reply).value, "bob's, after seeing alice's")
    assertEquals(g.merge(reply, first).value, "bob's, after seeing alice's",
      "the merge must not depend on the order it is asked in")
  }

  test("LwwRegister: concurrent writes are decided the SAME way on every replica") {
    // neither saw the other, and their clocks agree to the millisecond:
    // the node id decides, and it must decide identically everywhere
    val ca = Hlc.at(() => 1_700_000_000_000L)
    val cb = Hlc.at(() => 1_700_000_000_000L)
    val g = summon[Crdt[LwwRegister[String]]]
    val x = LwwRegister.write("from alice", ca, a)
    val y = LwwRegister.write("from bob", cb, b)
    assertEquals(g.merge(x, y), g.merge(y, x))
    assertEquals(g.merge(x, y).by.name, "bob", "the higher node id takes it")
  }

  test("OrSet: add, remove, add again — and the element is back") {
    // the bug a remove-set cannot avoid, and the reason tags exist
    val gen = Uid.at(() => 1_700_000_000_000L)
    var s = OrSet.empty[String]
    s = s.add("x", gen.next())
    assert(s.contains("x"))
    s = s.remove("x")
    assert(!s.contains("x"))
    s = s.add("x", gen.next())
    assert(s.contains("x"), "a fresh add carries a tag the removal never saw")
  }

  test("OrSet: a concurrent add survives a remove that never saw it") {
    val gen = Uid.at(() => 1_700_000_000_000L)
    val g = summon[Crdt[OrSet[String]]]
    val start = OrSet.empty[String].add("x", gen.next())

    val removed = start.remove("x")                 // alice removes what she saw
    val added = start.add("x", gen.next())          // bob adds again, concurrently

    val merged = g.merge(removed, added)
    assert(merged.contains("x"),
      "add-wins: bob's tag was never observed by alice's removal")
    assertEquals(g.merge(added, removed).value, merged.value)
  }

  test("OrSet: a remove that saw every tag does remove") {
    val gen = Uid.at(() => 1_700_000_000_000L)
    val g = summon[Crdt[OrSet[String]]]
    val both = OrSet.empty[String].add("x", gen.next()).add("x", gen.next())
    val gone = both.remove("x")
    assert(!gone.contains("x"))
    assert(!g.merge(gone, both).contains("x"),
      "merging back the pre-removal state must not resurrect it")
  }
}
