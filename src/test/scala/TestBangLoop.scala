package okay

/**
 * specs/fold-until.md, stage 2 — `!.loop(s)(f)`: continue on Left,
 * answer on Right, stack-safe by flatMap's laziness, every
 * iteration's effect performed in order and its state carried.
 */
class TestBangLoop extends munit.FunSuite:

  test("a counter to 1 000 000 at the Pure row, on the default stack") {
    val p: Int ! Nothing = !.loop[Int, Int, Nothing](0)(i => pure(if i < 1_000_000 then Left(i + 1) else Right(i)))
    assertEquals(!.run(p), 1_000_000)
  }

  test("f runs once per iteration, and the answer is the Right") {
    var calls = 0
    val p: String ! Nothing = !.loop[Int, String, Nothing](3) { n =>
      calls += 1
      pure(if n == 0 then Right("done") else Left(n - 1))
    }
    assertEquals(!.run(p), "done")
    assertEquals(calls, 4)
  }

  test("each iteration sees the state the previous one wrote: a loop over State") {
    // the loop's own state is the round; the State cell accumulates it
    val p: Int ! State % Int = !.loop[Int, Int, State % Int](0) { round =>
      State.modify[Int](_ + round).map(cell => if round < 5 then Left(round + 1) else Right(cell))
    }
    assertEquals(State.run(100)(p), (115, 115))
  }
