package okay

import okay.Rowlift.at

/**
 * A step the program may decline: `case Some(x) <-` and `if` in a
 * for-comprehension, and what each one means in each kind of row.
 */
class TestFail extends munit.FunSuite {

  test("Abort stops: the step after a failed pattern does not run") {
    type R = State % Int + Abort
    val p: Int ! R =
      for
        case Some(n) <- pure[R, Option[Int]](None)
        _            <- State.set[Int](1).at[R]
      yield n
    val (s, answer) =
      State.run[Int, Option[Int]](0)(runOption[Int, State % Int](p.at[Abort + State % Int]))
    assertEquals(answer, None)
    // the state proves it: `set` was not skipped by a branch, it was
    // never reached
    assertEquals(s, 0)
  }

  test("Abort passes the value through when the pattern matches") {
    type R = State % Int + Abort
    val p: Int ! R =
      for
        case Some(n) <- pure[R, Option[Int]](Some(7))
        _            <- State.set[Int](n).at[R]
      yield n * 2
    val (s, answer) =
      State.run[Int, Option[Int]](0)(runOption[Int, State % Int](p.at[Abort + State % Int]))
    assertEquals(answer, Some(14))
    assertEquals(s, 7)
  }

  test("an if guard is a precondition where the row can stop") {
    def check(n: Int): Option[Int] =
      !.run(runOption[Int, okay.Pure](for { x <- pure[Abort, Int](n); if x > 0 } yield x))
    assertEquals(check(5), Some(5))
    assertEquals(check(-5), None)
  }

  test("a pattern binds in a row that merely CONTAINS Choose") {
    // the shape the first design could not do: membership, not a bare
    // Choose row
    type R = Choose + Writer % String
    val p: Int ! R =
      for
        case Some(n) <- effect[R, Option[Int]](Choose(Seq(Some(1), None, Some(3))))
        _            <- Writer.tell(s"kept $n").at[R]
      yield n * 10
    val (told, answers) =
      !.run(Writer.run[String, Seq[Int], okay.Pure](runChoice[Int, Writer % String](p)))
    assertEquals(answers, Seq(10, 30))
    assertEquals(told, Seq("kept 1", "kept 3"))
  }

  test("where a row can do both, the search meaning wins") {
    type R = Choose + Abort
    val p: Int ! R =
      for case Some(n) <- effect[R, Option[Int]](Choose(Seq(Some(1), None, Some(3)))) yield n
    // aborting would answer None for the whole search; pruning keeps
    // the branches that matched
    assertEquals(!.run(runOption[Seq[Int], okay.Pure](runChoice[Int, Abort](p))), Some(Seq(1, 3)))
  }
}
