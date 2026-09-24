package okay2

class TestState extends munit.FunSuite {

  test("modify: get and set, said once") {
    val p: Int ! (State[Int]) =
      for {
        _ <- State.modify[Int](_ + 1)
        _ <- State.modify[Int](_ * 10)
        n <- State.get[Int]
      } yield n
    assertEquals(State.run[Int, Int](4)(p), (50, 50))
    // it answers the NEW state, as get and set do
    assertEquals(State.run[Int, Int](4)(State.modify[Int](_ + 1)), (5, 5))
  }

  test("update answers what the write destroys") {
    val p: Int ! (State[Int]) = State.update[Int, Int](s => (s, s * 10))
    assertEquals(State.run[Int, Int](4)(p), (40, 4))
  }

  test("swap answers both states") {
    assertEquals(State.run[Int, (Int, Int)](4)(State.swap[Int](_ * 10)), (40, (4, 40)))
  }

  test("index") {
    val x = State.index(List("a", "b", "c", "d", "e", "f", "g"), 1)
    assertEquals(x, (8L, List((7L, "g"), (6L, "f"), (5L, "e"), (4L, "d"), (3L, "c"), (2L, "b"), (1L, "a"))))
  }

  test("stack safety: indexing a 1M stream") {
    val n = 1000000
    assertEquals(State.index(LazyList.from(1).take(n))._1, n.toLong)
  }

  test("a lone operation is handled: the last node of a program") {
    // the arm every Scala 2 handler shares: `Inject(e)` with no
    // continuation is a Bind with a pure one (package.scala)
    assertEquals(State.run[Int, Int](4)(State.get[Int]), (4, 4))
    assertEquals(State.run[Int, Int](4)(State.set(9)), (9, 9))
  }

  test("forwarding: a State program in a wider row keeps the other effect") {
    type Row = State[Int] + Produce
    val p: Int ! Row =
      for {
        n <- State.get[Int].at[Row]
        m <- Produce.produce(n * 2).at[Row]
        _ <- State.set(m).at[Row]
      } yield n + m
    val (s, a) = State.handle(4)(p).runWith
    assertEquals((s, a), (8, 12))
  }

  test("PState: type-changing state, Int -> String -> Boolean") {
    val r = PState.run[Int, Boolean, String](41) {
      for {
        n <- PState.get[Int, (Boolean, String)]                              // n: Int
        _ <- PState.set[Int, String, (Boolean, String)]((n + 1).toString)    // the state is a String now
        s <- PState.get[String, (Boolean, String)]                           // s: String
        _ <- PState.set[String, Boolean, (Boolean, String)](s.length == 2)   // the state is a Boolean now
      } yield s + "!"
    }
    assertEquals(r, (true, "42!"))
  }
}
