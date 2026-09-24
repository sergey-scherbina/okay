package okay2

import Produce.produce

class TestWriter extends munit.FunSuite {

  test("run collects everything told, in order, forwarding the rest") {
    type Row = Writer[String] + Produce
    val p: Int ! Row = for {
      _ <- Writer.tell("a").at[Row]
      n <- produce(1).at[Row]
      _ <- Writer.tell("b").at[Row]
    } yield n
    assertEquals(Writer.run[String, Int, Row](p).runWith, (Seq("a", "b"), 1))
    assertEquals(Writer.collect[String, Int, Row](p).runWith, (Vector("a", "b"), 1))
  }

  test("foldWith folds the told values") {
    val p: Unit ! (Writer[Int] + Pure) = (1 to 100).foldLeft(pure[Writer[Int] + Pure, Unit](())) { (m, i) =>
      m.flatMap(_ => Writer.tell(i).plus[Pure])
    }
    assertEquals(!.run(Writer.foldWith[Int, Int, Unit, Writer[Int] + Pure](p)(0)(_ + _)), (5050, ()))
  }

  test("stack safety: 1M tells") {
    val n = 1000000
    val p: Unit ! (Writer[Int] + Pure) = (1 to n).foldLeft(pure[Writer[Int] + Pure, Unit](())) { (m, i) =>
      m.flatMap(_ => Writer.tell(i).plus[Pure])
    }
    assertEquals(!.run(Writer.foldWith[Int, Long, Unit, Writer[Int] + Pure](p)(0L)(_ + _))._1, n.toLong * (n + 1) / 2)
  }

  test("map transforms the telling in place and forwards the rest") {
    type Row = Writer[Int] + Produce
    val p: Int ! Row = Writer.tell(1).at[Row].flatMap(_ => produce(5).at[Row]).flatMap(x => Writer.tell(x).at[Row].map(_ => x))
    val q: Int ! (Writer[String] + Produce) = Writer.map[Int, String, Int, Row](p)(i => "n" + i)
    assertEquals(Writer.run[String, Int, Writer[String] + Produce](q).runWith, (Seq("n1", "n5"), 5))
  }
}

class TestThrows extends munit.FunSuite {

  test("raise and runEither") {
    val ok: Int ! (Throws[String] + Pure) = pure[Throws[String] + Pure, Int](1)
    val bad: Int ! (Throws[String] + Pure) = Throws.raise[String, Int]("no").plus[Pure]
    assertEquals(!.run(Throws.runEither[Int, String, Throws[String] + Pure](ok)), Right(1))
    assertEquals(!.run(Throws.runEither[Int, String, Throws[String] + Pure](bad)), Left("no"))
  }

  test("abort and runOption") {
    val p: Int ! (Abort + Pure) = abort[Int].plus[Pure]
    assertEquals(!.run(Throws.runOption(p)), None)
    assertEquals(!.run(Throws.runOption(pure[Abort + Pure, Int](2))), Some(2))
  }

  test("recover and orElse: the failure answered in the row, the row unchanged") {
    type Row = Throws[String] + Produce
    val p: Int ! Row = Throws.raise[String, Int]("x").at[Row].recover(e => produce(e.length).at[Row])
    assertEquals(Throws.runEither[Int, String, Row](p).runWith, Right(1))
    val q: Int ! Row = Throws.raise[String, Int]("x").at[Row].orElse(pure(9))
    assertEquals(Throws.runEither[Int, String, Row](q).runWith, Right(9))
  }

  test("runUnsafe: the JVM is the handler") {
    val p: Int ! (Throws[RuntimeException] + Pure) = Throws.raise[RuntimeException, Int](new RuntimeException("boom")).plus[Pure]
    intercept[RuntimeException](!.run(Throws.runUnsafe[Int, RuntimeException, Throws[RuntimeException] + Pure](p)))
  }

  test("an abort does not run what follows it") {
    type Row = Throws[String] + Writer[String]
    val p: Int ! Row = Writer.tell("before").at[Row].flatMap(_ => Throws.raise[String, Int]("stop").at[Row]).flatMap(x => Writer.tell("after").at[Row].map(_ => x))
    val (ws, r) = !.run(Writer.run[String, Either[String, Int], Writer[String] + Pure](Throws.runEither[Int, String, Row](p).plus[Pure]))
    assertEquals(r, Left("stop"))
    assertEquals(ws, Seq("before"))
  }
}

class TestReader extends munit.FunSuite {

  test("ask answers the environment; local modifies it for a sub-program") {
    type Row = Reader[Int] + Produce
    val p: Int ! Row = for {
      a <- Reader.ask[Int].at[Row]
      b <- Reader.local[Int, Int, Row](_ * 10)(Reader.ask[Int].at[Row])
      c <- produce(1).at[Row]
    } yield a + b + c
    assertEquals(Reader.run(2)(p).runWith, 23)
  }
}
