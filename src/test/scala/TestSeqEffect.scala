package okay

import okay.Rowlift.{at, plus}

/** A collection as a signature: nondeterminism with no wrapper. */
class TestSeqEffect extends munit.FunSuite {

  test("List is the alternatives; the handler is runChoice's") {
    val pairs: (Int, Int) ! List =
      for
        x <- List(1, 2).perform
        y <- List(10, 20).perform
      yield (x, y)
    assertEquals(!.run(runSeq[List, (Int, Int), Pure](pairs)),
      Seq((1, 10), (1, 20), (2, 10), (2, 20)))
  }

  test("an empty list prunes the branch, as failure should") {
    val p: Int ! List =
      for
        x <- List(1, 2, 3).perform
        _ <- (if x % 2 == 0 then List(()) else List.empty[Unit]).perform
      yield x * 10
    assertEquals(!.run(runSeq[List, Int, Pure](p)), Seq(20))
  }

  test("it forwards other effects, like any other signature") {
    type Row = List + Writer % String
    val p: Int ! Row =
      for
        x <- List(1, 2).perform.plus[Writer % String]
        _ <- Writer.tell(s"saw $x").at[Row]
      yield x
    assertEquals(!.run(Writer.run[String, Seq[Int], Pure](runSeq[List, Int, Writer % String](p))),
      (Seq("saw 1", "saw 2"), Seq(1, 2)))
  }

  test("Vector too, and the two do not confuse each other") {
    val p: Int ! Vector = Vector(1, 2, 3).perform
    assertEquals(!.run(runSeq[Vector, Int, Pure](p)), Seq(1, 2, 3))
  }
}
