package okay

import scala.util.chaining.*

class TestState extends munit.FunSuite {

  test("modify: get and set, said once") {
    val p: Int ! State % Int =
      for
        _ <- State.modify[Int](_ + 1)
        _ <- State.modify[Int](_ * 10)
        n <- State.get[Int]
      yield n
    assertEquals(State.run[Int, Int](4)(p), (50, 50))
    // it answers the NEW state, as get and set do
    assertEquals(State.run[Int, Int](4)(State.modify[Int](_ + 1)), (5, 5))
  }

  test("update answers what the write destroys") {
    // the value that WAS there, which `modify` cannot give back
    val p: Int ! State % Int = State.update[Int, Int](s => (s, s * 10))
    assertEquals(State.run[Int, Int](4)(p), (40, 4))
  }

  test("swap answers both states") {
    assertEquals(State.run[Int, (Int, Int)](4)(State.swap[Int](_ * 10)), (40, (4, 40)))
  }

  test("index") {
    val x = State.index(List("a", "b", "c", "d", "e", "f", "g"), 1).tap(println)
    assertEquals(x, (8L, List((7L, "g"), (6L, "f"), (5L, "e"), (4L, "d"), (3L, "c"), (2L, "b"), (1L, "a"))))
  }

  test("stack safety: indexing a 1M stream") {
    val n = 1000000
    assertEquals(State.index(fibs[Int, LazyList].take(n))._1, n.toLong)
  }

  test("PState: type-changing state, Int -> String -> Boolean") {
    val r = PState.run(41):
      for
        n <- PState.get                    // n: Int
        _ <- PState.set((n + 1).toString)  // the state is a String now
        s <- PState.get                    // s: String
        _ <- PState.set(s.length == 2)     // the state is a Boolean now
      yield s + "!"
    assertEquals(r, (true, "42!"))
  }

  // effect-row-cost D1: a counter that pays two operations per update
  // pays them twice again when forwarded (specs/effect-row-cost.md)
  test("modify is ONE operation: a single injected Modify, not get then set") {
    State.modify[Int](_ + 1) match
      case Free.Inject(State.Modify(_)) => ()
      case other => fail(s"modify is not one operation: $other")
  }

  test("Modify answers the new state, and chains as modify always has") {
    val p: (Int, Int) ! State % Int =
      for
        a <- State.modify[Int](_ + 1)
        b <- State.modify[Int](_ * 10)
      yield (a, b)
    assertEquals(State.run(0)(p), (10, (1, 10)))
  }

  test("a forwarded Modify is handled by the outer State handler") {
    type Fx = State % Int + Writer % String
    import okay.Row.at
    val p: Int ! Fx =
      State.modify[Int](_ + 5).at[Fx].flatMap(n => Writer.tell(s"n=$n").at[Fx].map(_ => n))
    assertEquals(State.run[Int, (Seq[String], Int)](1)(Writer.run[String, Int, State % Int](p)), (6, (Seq("n=6"), 6)))
  }
}
