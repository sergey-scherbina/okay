package okay2

/** refs made at run time: one row member, however many there are. */
class TestRefs extends munit.FunSuite {

  test("two cells, one row member, each its own value") {
    val p: (Int, String) ! Refs =
      for {
        a <- Refs.ref(1)
        b <- Refs.ref("ada")
        _ <- Refs.write(a, 2)
        x <- Refs.read(a)
        y <- Refs.read(b)
      } yield (x, y)
    assertEquals(Refs.run(p), (2, "ada"))
  }

  test("cells made in a loop — what a type could not have listed") {
    val p: Seq[Int] ! Refs =
      (1 to 5).foldLeft(pure[Refs, Seq[Refs.Ref[Int]]](Seq.empty)) { (acc, i) =>
        acc.flatMap(cs => Refs.ref(i * 10).map(cs :+ _))
      }.flatMap { cs =>
        cs.foldLeft(pure[Refs, Seq[Int]](Seq.empty)) { (acc, c) =>
          acc.flatMap(vs => Refs.read(c).map(vs :+ _))
        }
      }
    assertEquals(Refs.run(p), Seq(10, 20, 30, 40, 50))
  }

  test("the same value type in two cells stays separate") {
    val p: (Int, Int) ! Refs =
      for {
        a <- Refs.ref(0)
        b <- Refs.ref(0)
        _ <- Refs.write(a, 1)
        _ <- Refs.write(b, 2)
        x <- Refs.read(a)
        y <- Refs.read(b)
      } yield (x, y)
    assertEquals(Refs.run(p), (1, 2))
  }

  test("other effects forward through the heap") {
    type Row = Refs + Writer[String]
    val p: Int ! Row =
      for {
        c <- Refs.ref(41)
        _ <- Writer.tell("made it")
        n <- Refs.write(c, 42)
      } yield n
    assertEquals(!.run(Writer.run(Refs.handle(p))), (Seq("made it"), 42))
  }

  test("100 000 cells and reads, on the default stack") {
    val n = 100000
    val p: Int ! Refs = (1 to n).foldLeft(pure[Refs, Int](0)) { (acc, i) =>
      acc.flatMap(s => Refs.ref(i).flatMap(c => Refs.read(c).map(_ + s - i + 1)))
    }
    assertEquals(Refs.run(p), n)
  }
}
