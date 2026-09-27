package okay

import okay.!.*

/** specs map-flatmap-pair-cost: `map` fused into the next bind */
class TestMapFusion extends munit.FunSuite {
  type W = Writer % String
  def tell(s: String): Unit ! W = Writer.tell(s)

  test("map then flatMap is ONE bind over the operation: no nested Bind to rotate") {
    val p: Int ! W = tell("a").map(_ => 1).flatMap(n => pure(n + 1))
    p match
      case Free.Bind(Free.Inject(_), _) => ()
      case other => fail(s"not fused: ${other}")
    assertEquals(!.run(Writer.run[String, Int, Pure](p)), (List("a"), 2))
  }

  test("map then map is one bind too, and the functions apply in order") {
    val p: String ! W = tell("a").map(_ => 1).map(_ + 1).map(n => s"n=$n")
    p match
      case Free.Bind(Free.Inject(_), _) => ()
      case other => fail(s"not fused: ${other}")
    assertEquals(!.run(Writer.run[String, String, Pure](p)), (List("a"), "n=2"))
  }

  test("the mapped function runs when the program runs, not when it is built") {
    var calls = 0
    val p: Int ! W = tell("a").map(_ => { calls += 1; 1 }).flatMap(n => pure(n))
    assertEquals(calls, 0)
    val _ = !.run(Writer.run[String, Int, Pure](p))
    assertEquals(calls, 1)
    val _ = !.run(Writer.run[String, Int, Pure](p))
    assertEquals(calls, 2, "a program is a value: it runs its map again")
  }

  test("a long chain of maps stays stack-safe (the fusion depth is bounded)") {
    val n = 1000000
    val p = (1 to n).foldLeft(tell("a").map(_ => 0))((m, _) => m.map(_ + 1))
    assertEquals(!.run(Writer.run[String, Int, Pure](p))._2, n)
    val q = (1 to n).foldLeft(pure[W, Int](0))((m, _) => m.map(_ + 1).flatMap(x => pure(x)))
    assertEquals(!.run(Writer.run[String, Int, Pure](q))._2, n)
  }

  test("a map over a raise still stops, and over Choose it runs per branch") {
    val r: Int ! Throws % String = raise[String, Int]("no").map(_ + 1).flatMap(x => pure(x * 2))
    assertEquals(!.run(runEither(r)), Left("no"))
    val c: Int ! Choose = choose(1, 2, 3).map(_ * 10).flatMap(x => pure(x + 1))
    assertEquals(!.run(runChoice(c)), Seq(11, 21, 31))
  }
}
