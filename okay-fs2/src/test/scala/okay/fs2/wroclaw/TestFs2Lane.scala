package okay.fs2.wroclaw

import okay.wroclaw.{Gtfs, OkayLane}

/** the lane answers what okay answers (docs/benchmarks.md §20) */
class TestFs2Lane extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present

  test("the lane agrees with okay on every checksum") {
    val feed = Gtfs.events(1)
    assertEquals(Fs2Lane.run(feed), OkayLane.run(feed))
  }
}
