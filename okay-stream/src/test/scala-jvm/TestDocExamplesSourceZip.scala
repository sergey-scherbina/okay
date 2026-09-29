package okay

/** docs/guide.md §6's `Source.zip` example, line for line
 * (TestDocSnippets pins each line of the page to a line here) */
class TestDocExamplesSourceZip extends munit.FunSuite {

  test("guide §6: zip pairs in lockstep until either side ends, zipWith folds the pair") {
    val ticks = Source.of(LazyList.from(0))                      // endless
    val named = Source.of(List("a", "b", "c"))
    val z = Source.zip(ticks, named)
    assertEquals(z.runCollect.runWith, Vector((0, "a"), (1, "b"), (2, "c")))
    val sums = Source.zipWith(Source.of(List(1, 2, 3)), Source.of(List(10, 20, 30)))(_ + _)
    assertEquals(sums.runCollect.runWith, Vector(11, 22, 33))
  }
}
