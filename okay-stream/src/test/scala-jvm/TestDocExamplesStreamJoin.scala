package okay

import Chunks.elements

/** docs/guide.md §6's `Source.joinSorted` example, line for line
 * (TestDocSnippets pins each line of the page to a line here) */
class TestDocExamplesStreamJoin extends munit.FunSuite {

  test("guide §6: joinSorted joins two key-ordered streams, holding one run") {
    val orders = Source.of(List((1, "book"), (2, "pen"), (2, "ink"), (4, "lamp")))   // by customer
    val names = Source.of(List((1, "Ann"), (2, "Bob"), (3, "Cid")))
    val j = Source.joinSorted(orders, names)
    assertEquals(j.runCollect.runWith, Vector((1, ("book", "Ann")), (2, ("pen", "Bob")), (2, ("ink", "Bob"))))
    val all = Source.leftJoinSorted(orders, names)
    assertEquals(all.runCollect.runWith.last, (4, ("lamp", None)))
    val c = Chunks.joinSorted(Chunks.fromIterator(Iterator((1, "a"), (3, "c"))), Chunks.fromIterator(Iterator((3, "x"))))
    assertEquals(c.elements.toList, List((3, ("c", "x"))))
  }
}
