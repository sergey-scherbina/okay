package okay.freer



import okay.std.*
/**
 * docs/typestate.md's core examples, VERBATIM (doc-snippet-debt): each
 * line as the page prints it, answer comment included, then asserted.
 * The transaction example on that page is pinned by okay-sql's
 * TestTxData, where its driver lives.
 */
class TestDocExamplesTypestate extends munit.FunSuite:

  test("a typestate as data: Int -> String -> Boolean through the threaded loop") {
    import PState.Threaded.{get, put}
    val r = PState.Threaded.run(
      for
        n <- get[Int]                          // n: Int
        _ <- put[Int, String]((n + 1).toString) // the state is a String now
        s <- get[String]                       // s: String
        old <- put[String, Boolean](s.length == 2) // the old state is the answer, as set's is
      yield s + "!" + old)(41)
    assertEquals(r, (true, "42!42"))
  }

  test("a put after a get of another type does not type") {
    assert(compileErrors("PState.Threaded.get[Int].flatMap(n => PState.Threaded.put[String, Int](n))").nonEmpty)
  }
