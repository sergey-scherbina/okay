package okay.java.wroclaw

import okay.wroclaw.{Gtfs, OkayLane}

/** the JDK lanes answer what okay answers (docs/benchmarks.md §20) */
class TestJavaLane extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present

  test("groupingBy, parallel groupingBy and the windowed collector all agree with okay") {
    val feed = Gtfs.events(1)
    val expect = OkayLane.run(feed)
    assertEquals(JavaLane.run(feed, parallel = false), expect, "the sequential groupingBy road differs")
    assertEquals(JavaLane.run(feed, parallel = true), expect, "the parallel groupingBy road differs")
    assertEquals(JavaLane.windowed(feed), expect, "the windowed collector differs")
  }
}
