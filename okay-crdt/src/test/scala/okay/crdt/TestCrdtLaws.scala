package okay.crdt

import okay.{Hlc, Uid}

/**
 * EVERY INSTANCE RUNS THE THREE LAWS. That is the deliverable of
 * stage 2 — the types are easy and the laws are the content, so a new
 * instance that does not appear here has not been checked.
 *
 * The samples are values a replica could really hold, built the way a
 * replica would build them, because a law checked over impossible
 * values proves nothing and can fail for reasons that never happen.
 */
class TestCrdtLaws extends munit.FunSuite {

  private val a = NodeId("alice")
  private val b = NodeId("bob")
  private val c = NodeId("carol")

  private def lawful[A](name: String, samples: Seq[A])(using Crdt[A]): Unit =
    val bad = Crdt.violations(samples)
    assert(bad.isEmpty, s"$name breaks its laws:\n  " + bad.mkString("\n  "))

  test("GCounter obeys the three laws") {
    val x = GCounter.empty.inc(a).inc(a).inc(b)
    val y = GCounter.empty.inc(b, 5).inc(c)
    val z = GCounter.empty.inc(a, 2)
    lawful("GCounter", Seq(GCounter.empty, x, y, z))
  }

  test("PNCounter obeys the three laws") {
    val x = PNCounter.empty.inc(a).inc(a).dec(b)
    val y = PNCounter.empty.dec(a, 3).inc(c, 2)
    val z = PNCounter.empty.inc(b)
    lawful("PNCounter", Seq(PNCounter.empty, x, y, z))
  }

  test("GSet obeys the three laws") {
    lawful("GSet", Seq(GSet.empty[Int], GSet(Set(1, 2)), GSet(Set(2, 3)), GSet(Set(9))))
  }

  test("OrSet obeys the three laws") {
    val gen = Uid.at(() => 1_700_000_000_000L)
    val x = OrSet.empty[String].add("a", gen.next()).add("b", gen.next())
    val y = x.remove("a").add("c", gen.next())
    val z = OrSet.empty[String].add("a", gen.next())
    lawful("OrSet", Seq(OrSet.empty[String], x, y, z))
  }

  test("LwwRegister obeys the three laws") {
    // each node stamps from its OWN clock, which is the precondition
    // the type documents: (at, by) then identifies a write uniquely
    val ca = Hlc.at(() => 1_700_000_000_000L)
    val cb = Hlc.at(() => 1_700_000_000_000L)
    val x = LwwRegister.write("x", ca, a)
    val y = LwwRegister.write("y", cb, b)
    val z = LwwRegister.write("z", ca, a)
    lawful("LwwRegister", Seq(x, y, z))
  }

  test("the law check ANSWERS what broke, rather than throwing at the first") {
    // a merge that adds: commutative and associative, NOT idempotent —
    // which is exactly the naive replicated counter, and exactly why
    // GCounter is a map of maxima instead
    given Crdt[Int] with
      def merge(x: Int, y: Int): Int = x + y
    val bad = Crdt.violations(Seq(1, 2, 3))
    assert(bad.nonEmpty, "adding is not idempotent and the check must say so")
    assert(bad.forall(_.startsWith("not idempotent")),
      s"addition breaks ONLY idempotence, got: $bad")
    assertEquals(bad.length, 3, "one failure per sample, not one for the lot")
  }
}
