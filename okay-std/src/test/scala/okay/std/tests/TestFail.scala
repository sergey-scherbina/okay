package okay.std.tests

import okay.*
import scala.language.implicitConversions
import okay.std.*
import okay.{Answers}

/**
 * A step the program may decline: `case Some(x) <-` and `if` in a
 * for-comprehension, and what each one means in each kind of row.
 */
class TestFail extends munit.FunSuite {

  test("Abort stops: the step after a failed pattern does not run") {
    type R = State % Int + Abort
    val p: Int ! R =
      for
        case Some(n) <- pure[Option[Int], R](None)
        _            <- State.set[Int](1)
      yield n
    val (s, answer) =
      State.run[Int, Option[Int]](0)(runOption(p))
    assertEquals(answer, None)
    // the state proves it: `set` was not skipped by a branch, it was
    // never reached
    assertEquals(s, 0)
  }

  test("Abort passes the value through when the pattern matches") {
    type R = State % Int + Abort
    val p: Int ! R =
      for
        case Some(n) <- pure[Option[Int], R](Some(7))
        _            <- State.set[Int](n)
      yield n * 2
    val (s, answer) =
      State.run[Int, Option[Int]](0)(runOption(p))
    assertEquals(answer, Some(14))
    assertEquals(s, 7)
  }

  test("an if guard is a precondition where the row can stop") {
    def check(n: Int): Option[Int] =
      runOption(for { x <- pure[Int, Abort](n); if x > 0 } yield x).run
    assertEquals(check(5), Some(5))
    assertEquals(check(-5), None)
  }

  test("a pattern binds in a row that merely CONTAINS Choose") {
    // the shape the first design could not do: membership, not a bare
    // Choose row
    type R = Choose + Writer % String
    val p: Int ! R =
      for
        case Some(n) <- effect[Choose, Option[Int]](Choose(Seq(Some(1), None, Some(3)))).at[R]
        _            <- Writer.tell(s"kept $n").at[R]
      yield n * 10
    val (told, answers) =
      (Writer.run(runChoice(p))).run
    assertEquals(answers, Seq(10, 30))
    assertEquals(told, Seq("kept 1", "kept 3"))
  }

  test("where a row can do both, the search meaning wins") {
    type R = Choose + Abort
    val p: Int ! R =
      for case Some(n) <- effect[Choose, Option[Int]](Choose(Seq(Some(1), None, Some(3)))).at[R] yield n
    // aborting would answer None for the whole search; pruning keeps
    // the branches that matched
    assertEquals((runOption(runChoice(p))).run, Some(Seq(1, 3)))
  }

  test("plus names only what is added; the row you are in is already in the type") {
    // `find` is written at its own row and lands in that row + Abort,
    // without the caller spelling `Users` a second time
    enum Users[+A]:
      case Find(id: Long) extends Users[Option[String]]
    def find(id: Long): Option[String] ! Users = effect(Users.Find(id))

    def rename(id: Long): String ! Users + Abort =
      for case Some(old) <- find(id).at[Users + Abort] yield old

    val handler: Answers[Users] = new:
      def handle[A](e: Users[A]): A = e match
        case Users.Find(7L) => Some("ada")
        case Users.Find(_)  => None
    assertEquals(runOption(rename(7L)).runWith(using handler),
      Some("ada"))
    assertEquals(runOption(rename(9L)).runWith(using handler),
      None)
  }

  test("ensure is the guard outside a for-comprehension") {
    type R = Abort + State % Int
    def withdraw(amount: Int): Int ! R =
      for
        balance <- State.get[Int].at[R]
        _       <- ensure[R](balance >= amount)
        _       <- State.set[Int](balance - amount).at[R]
      yield balance - amount
    assertEquals(State.run[Int, Option[Int]](100)(runOption(withdraw(30))),
      (70, Some(70)))
    // refused, and the state proves the `set` was never reached
    assertEquals(State.run[Int, Option[Int]](10)(runOption(withdraw(30))),
      (10, None))
  }

  test("orElse answers a failure and leaves the row unchanged") {
    type R = Abort + State % Int
    def withdraw(amount: Int): Int ! R =
      for
        balance <- State.get[Int].at[R]
        _       <- ensure[R](balance >= amount)
        _       <- State.set[Int](balance - amount).at[R]
      yield balance - amount

    // the whole program continues after the recovery: another step
    // runs in the same row, none the wiser
    val p: Int ! R =
      for
        taken <- withdraw(30).orElse(pure[Int, R](0))
        _     <- State.set[Int](999).at[R]
      yield taken
    assertEquals(State.run[Int, Option[Int]](10)(runOption(p)),
      (999, Some(0)))
  }

  test("recover sees the error; the row may be written in either order") {
    type R = State % Int + Throws % String
    val boom: Int ! R = raise[String, Int]("no disk").at[R]
    val p: Int ! R = boom.recover(e => pure[Int, R](e.length))
    val (s, answer) =
      State.run[Int, Either[String, Int]](0)(
        runEither(p.at[Throws % String + State % Int]))
    assertEquals(answer, Right(7))
    assertEquals(s, 0)
  }

  test("row order: + is a sum of effects, either order fitted to the other by at") {
    // on the kernel `+` is a sum, not a union: the two spellings are two
    // types, each fitted to the other by `at` (each effect found by its
    // path), and both run the same
    val a: String ! Abort + State % Int = pure("x")
    val b: String ! State % Int + Abort = a.at[State % Int + Abort]
    val c: String ! Abort + State % Int = b.at[Abort + State % Int]
    assertEquals(State.run[Int, Option[String]](0)(runOption(b)), (0, Some("x")))
    assertEquals(State.run[Int, Option[String]](0)(runOption(c)), (0, Some("x")))
  }
}
