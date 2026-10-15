package okay

/** docs/direct-style.md's opening snippet, verbatim: the for-comprehension a direct block replaces
 * (the block itself is TestDirect's first test) */
class TestDocExamplesDirectStyle extends munit.FunSuite:

  test("direct-style: the for-comprehension a direct block replaces") {
    def add(mx: Option[Int], my: Option[Int]): Option[Int] =
      for
        x <- mx
        y <- my
      yield x + y
    assertEquals(add(Some(2), Some(3)), Some(5))
    assertEquals(add(None, Some(3)), None)
  }
