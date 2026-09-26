package okay

/** docs/guide.md §6's `Source.mergeReady` example, line for line
 * (TestDocSnippets pins each line of the page to a line here) */
class TestDocExamplesReadyMerge extends munit.FunSuite {

  test("guide §6: a round-robin of ready sources, and one side given its own fiber") {
    val merged = Source.mergeReady(Source.of(List(1, 2, 3)), Source.of(List(10, 20)))
    val out = merged.runCollect.runWith                 // Vector(1, 10, 2, 20, 3)
    assertEquals(out, Vector(1, 10, 2, 20, 3))

    val offCore = Channel.buffer(64)(LazyList.range(0, 1000)).drained
    val both = offCore mergeReady Source.of(List(-1, -2))
    val all = both.runCollect.runWith
    assertEquals(all.filter(_ >= 0), Vector.range(0, 1000))
    assertEquals(all.filter(_ < 0), Vector(-1, -2))
  }
}
