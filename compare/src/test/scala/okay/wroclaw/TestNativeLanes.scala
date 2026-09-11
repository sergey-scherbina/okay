package okay.wroclaw

/**
 * THE ANCHOR OF §20's SINGLE-NODE TABLE: the plain-JVM fold — the one
 * every library lane carries — answers exactly what okay's own lane
 * answers, on one thread and on four.
 *
 * That equality is what makes the eleven checksums usable as a gate in
 * the benchmark itself: each lane's `main` asserts its answer against
 * `OkayLane.run` before printing a number, so a row can be slow or
 * fast but never wrong. It also pins the one semantic claim the
 * no-eviction shape rests on — with the feed's jitter bounded below
 * the watermark bound, nothing is ever late, so a fold that never
 * evicts and a window operator that evicts on the watermark see the
 * same panes.
 */
class TestNativeLanes extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present

  test("the plain-JVM loop agrees with okay on every checksum") {
    val feed = Gtfs.events(1)
    assertEquals(JvmLane.loop(feed), OkayLane.run(feed))
  }

  test("four threads agree with one — the slice boundaries stitch") {
    val feed = Gtfs.events(1)
    assertEquals(JvmLane.threads(feed, 4), OkayLane.run(feed))
  }

  /**
   * The four corners of §20's "why one core loses" 2x2 answer the same
   * thing. The mutable-cell aggregator is the interesting one: `init`
   * hands out a FRESH cell per pane and `add` bumps it in place, so a
   * shared-cell mistake would show here as a wrong checksum rather
   * than as a suspiciously good number in the table.
   */
  test("the mutable-cell aggregator computes what the algebraic one computes") {
    val feed = Gtfs.events(1)
    val expect = OkayLane.run(feed)
    assertEquals(OkayLane.runCells(feed), expect, "the general operator over a cell differs")
    assertEquals(OkayLane.packedCells(feed), expect, "the packed operator over a cell differs")
    assertEquals(OkayLane.packed(feed), expect, "the packed operator differs")
    assertEquals(OkayLane.runSummary(feed), expect, "the flat summary aggregator differs")
  }
}
