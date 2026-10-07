package okay


import okay.freer.given


import okay.std.given
/** docs/guide.md §6's `Source.joinWithin` example, line for line
 * (TestDocSnippets pins each line of the page to a line here) */
class TestDocExamplesWindowJoin extends munit.FunSuite {

  test("guide §6: joinWithin matches by key within an event-time interval, no clock") {
    val clicks = Source.of(List(("u1", (0L, "home")), ("u2", (3L, "cart")), ("u1", (30L, "pay"))))   // (user, (time, page))
    val buys = Source.of(List(("u1", (8L, 9.99)), ("u2", (50L, 4.50))))
    val paid = Source.joinWithin(clicks, buys, within = 10L, lateness = 0L)(_._1, _._1)
    assertEquals(paid.runCollect.runWith, Vector(("u1", ((0L, "home"), (8L, 9.99)))))
  }
}
