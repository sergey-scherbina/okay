package okay

import okay.Rowlift.{at, plus}

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
        _            <- State.set[Int](1).plus[Abort]
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
        _            <- State.set[Int](n).plus[Abort]
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

  test("plus names only what is added; the row you are in is already in the type") {
    // `find` is written at its own row and lands in that row + Abort,
    // without the caller spelling `Users` a second time
    enum Users[+A]:
      case Find(id: Long) extends Users[Option[String]]
    def find(id: Long): Option[String] ! Users = effect(Users.Find(id))

    def rename(id: Long): String ! (Users + Abort) =
      for case Some(old) <- find(id).plus[Abort] yield old

    val handler: Handler[Users] = new:
      def handle[A](e: Users[A]): A = e match
        case Users.Find(7L) => Some("ada")
        case Users.Find(_)  => None
    assertEquals(runOption[String, Users](rename(7L).at[Abort + Users]).runWith(using handler),
      Some("ada"))
    assertEquals(runOption[String, Users](rename(9L).at[Abort + Users]).runWith(using handler),
      None)
  }
}
