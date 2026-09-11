package okay.zio.wroclaw

import okay.wroclaw.{Gtfs, OkayLane}

/** the lane answers what okay answers (docs/benchmarks.md §20) */
class TestZioLane extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present

  test("the lane agrees with okay on every checksum") {
    val feed = Gtfs.events(1)
    assertEquals(ZioLane.run(feed), OkayLane.run(feed))
  }

  test("four slices through foreachPar agree with one") {
    val feed = Gtfs.events(1)
    assertEquals(ZioLane.parallel(feed, 4), OkayLane.run(feed))
  }
}
