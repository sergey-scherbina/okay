package okay.crdt

import okay.Uid

/**
 * THE LAW A LOG ACTUALLY EXPERIENCES (specs/coordination-free.md
 * stage 3): a fold over records **in any order, with duplicates**
 * lands on the same value.
 *
 * Stage 2 proved the algebra — commutative, associative, idempotent.
 * This proves what the algebra BUYS once a log is involved, which is
 * the form a `View` meets: partitions interleave, so the order is not
 * the order anything happened in; a consumer replays from an offset,
 * so records repeat; a cold rebuild reads whatever compaction left,
 * so it sees a different subset in a different order and must still
 * agree with the warm view.
 *
 * The three laws are the reason this holds. This test is the reason
 * to believe the reason.
 */
class TestCrdtFold extends munit.FunSuite {

  private val a = NodeId("alice")
  private val b = NodeId("bob")
  private val c = NodeId("carol")

  /** a "record": what a partition carries. The seam is generic over
   * this, which is why okay-crdt needs no dependency to have it. */
  private final case class Rec(payload: Option[GCounter])

  /**
   * Every ordering of `records`, each also folded with every record
   * delivered twice, must land on one value.
   *
   * Permutations rather than a shuffle: with five records that is 120
   * orders, which is cheap and exhaustive, and an exhaustive check
   * cannot be lucky.
   */
  private def foldsAlike(records: List[Rec]): Unit =
    val fold = Crdt.folding[GCounter, Rec](_.payload)
    def run(rs: List[Rec]): Option[GCounter] = rs.foldLeft(Option.empty[GCounter])(fold)
    val expected = run(records)
    records.permutations.foreach { p =>
      assertEquals(run(p), expected, s"order changed the answer: $p")
      assertEquals(run(p ++ p), expected, s"redelivery changed the answer: $p")
    }

  test("a CRDT fold is order-independent and duplicate-proof, exhaustively") {
    val rs = List(
      Rec(Some(GCounter.empty.inc(a))),
      Rec(Some(GCounter.empty.inc(a).inc(b, 2))),
      Rec(Some(GCounter.empty.inc(c, 5))),
      Rec(Some(GCounter.empty.inc(b))),
      Rec(None),                                   // an unreadable record
    )
    foldsAlike(rs)
    // and the value is the one the counters mean, not merely a stable one
    val fold = Crdt.folding[GCounter, Rec](_.payload)
    assertEquals(rs.foldLeft(Option.empty[GCounter])(fold).map(_.value), Some(1L + 2L + 5L))
  }

  test("THE CONTROL: a naive last-write-wins-by-ARRIVAL fold is order dependent") {
    // this is the fold a cache reaches for without thinking, and the
    // reason the law above is worth writing: if the permutation check
    // passed for THIS too, it would not be testing order-independence
    val naive: (Option[GCounter], Rec) => Option[GCounter] =
      (state, r) => r.payload.orElse(state)      // last one seen wins
    val rs = List(
      Rec(Some(GCounter.empty.inc(a))),
      Rec(Some(GCounter.empty.inc(b, 9))),
    )
    val forwards = rs.foldLeft(Option.empty[GCounter])(naive).map(_.value)
    val backwards = rs.reverse.foldLeft(Option.empty[GCounter])(naive).map(_.value)
    assertNotEquals(forwards, backwards,
      "the control must be order dependent, or it is not a control")
  }

  test("an unreadable record keeps the state instead of resetting it") {
    // `decode` answering None means "not for me"; a fold that let that
    // clear the state would make one bad record erase a replica
    val fold = Crdt.folding[GCounter, Rec](_.payload)
    val have = Some(GCounter.empty.inc(a, 3))
    assertEquals(fold(have, Rec(None)), have)
    assertEquals(fold(None, Rec(None)), None)
  }

  test("the fold starts from nothing and needs no empty element") {
    // LwwRegister has no identity — there is no "empty register" — so
    // the fold begins with the first value it decodes rather than with
    // a manufactured zero
    val ca = okay.Hlc.at(() => 1_700_000_000_000L)
    val fold = Crdt.folding[LwwRegister[String], Option[LwwRegister[String]]](identity)
    val first = LwwRegister.write("first", ca, a)
    assertEquals(fold(None, Some(first)), Some(first))
  }

  test("mergeAll: the whole of a replica's history, in one call") {
    val gen = Uid.at(() => 1_700_000_000_000L)
    val sets = List(
      OrSet.empty[String].add("x", gen.next()),
      OrSet.empty[String].add("y", gen.next()),
      OrSet.empty[String].add("x", gen.next()),
    )
    assertEquals(Crdt.mergeAll(sets).map(_.value), Some(Set("x", "y")))
    assertEquals(Crdt.mergeAll(sets.reverse).map(_.value), Some(Set("x", "y")))
    assertEquals(Crdt.mergeAll(List.empty[OrSet[String]]), None,
      "nothing seen is None, not an invented empty")
  }

  test("a cold rebuild over a SUBSET agrees with the warm view — compaction's case") {
    // compaction keeps the last record per key, so a rebuild sees
    // fewer records than the warm view folded. For a CRDT that is
    // safe exactly when the kept records still carry every node's
    // count, which is what merging by max guarantees.
    val warm = List(
      GCounter.empty.inc(a),
      GCounter.empty.inc(a).inc(b, 2),
      GCounter.empty.inc(a).inc(b, 2).inc(c, 5),
    )
    val compacted = List(warm.last)          // what compaction would keep
    assertEquals(Crdt.mergeAll(compacted).map(_.value), Crdt.mergeAll(warm).map(_.value))
  }
}
