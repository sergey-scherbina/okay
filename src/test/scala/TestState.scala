package okay

import scala.util.chaining.*

class TestState extends munit.FunSuite {

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

}
