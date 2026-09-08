package okay

import okay.RowLift.plus

/** Cells made at run time: one row member, however many there are. */
class TestCells extends munit.FunSuite {

  test("two cells, one row member, each its own value") {
    val p: (Int, String) ! Cells =
      for
        a <- Cells.cell(1)
        b <- Cells.cell("ada")
        _ <- Cells.write(a, 2)
        x <- Cells.read(a)
        y <- Cells.read(b)
      yield (x, y)
    assertEquals(Cells.run(p), (2, "ada"))
  }

  test("cells made in a loop — what a type could not have listed") {
    val p: Seq[Int] ! Cells =
      (1 to 5).foldLeft(pure[Cells, Seq[Cells.Cell[Int]]](Seq.empty)) { (acc, i) =>
        acc.flatMap(cs => Cells.cell(i * 10).map(cs :+ _))
      }.flatMap { cs =>
        cs.foldLeft(pure[Cells, Seq[Int]](Seq.empty)) { (acc, c) =>
          acc.flatMap(vs => Cells.read(c).map(vs :+ _))
        }
      }
    assertEquals(Cells.run(p), Seq(10, 20, 30, 40, 50))
  }

  test("the same value type in two cells stays separate") {
    val p: (Int, Int) ! Cells =
      for
        a <- Cells.cell(0)
        b <- Cells.cell(0)
        _ <- Cells.write(a, 1)
        _ <- Cells.write(b, 2)
        x <- Cells.read(a)
        y <- Cells.read(b)
      yield (x, y)
    assertEquals(Cells.run(p), (1, 2))
  }

  test("other effects forward through the heap") {
    type Row = Cells + Writer % String
    val p: Int ! Row =
      for
        c <- Cells.cell(41).plus[Writer % String]
        _ <- Writer.tell("made it").plus[Cells]
        n <- Cells.write(c, 42).plus[Writer % String]
      yield n
    assertEquals(!.run(Writer.run[String, Int, okay.Pure](Cells.handle[Int, Writer % String](p))),
      (Seq("made it"), 42))
  }
}
