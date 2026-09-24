package okay2

import Produce.produce

/**
 * The row discipline: what `Member` admits, what it refuses, and that
 * the erasure argument holds at run time — an operation of a union row
 * is held RAW in the tree, with no wrapper and no checkcast.
 */
class TestRow extends munit.FunSuite {

  type Row = State[Int] + Writer[String] + Produce

  test("at: a program lands in any row that contains its own, left, right or deeper") {
    val a: Int ! Row = State.get[Int].at[Row]
    val b: Unit ! Row = Writer.tell("x").at[Row]
    val c: Int ! Row = produce(1).at[Row]
    val p = for { n <- a; _ <- b; m <- c } yield n + m
    // handlers in EITHER order: `Remove` finds the signature anywhere in the row
    val (ws, (s, x)) = Writer.run[String, (Int, Int), Writer[String] + Produce](State.handle(41)(p)).runWith
    assertEquals((s, ws, x), (41, Seq("x"), 42))
    val (s2, (ws2, x2)) = State.handle(41)(Writer.run[String, Int, Row](p)).runWith
    assertEquals((s2, ws2, x2), (41, Seq("x"), 42))
  }

  test("a union does not commute in Scala 2: reordering is an `at`") {
    val p: Int ! (State[Int] + Produce) = State.get[Int].plus[Produce]
    val q: Int ! (Produce + State[Int]) = p.at[Produce + State[Int]]
    assertEquals(State.handle(3)(q).runWith, (3, 3))
  }

  test("Pure rides into any row") {
    val p: Int ! Pure = pure(1)
    val q: Int ! Row = p.at[Row]
    val (_, (_, x)) = Writer.run[String, (Int, Int), Writer[String] + Produce](State.handle(0)(q)).runWith
    assertEquals(x, 1)
  }

  test("membership refused: a program cannot land in a row without its effect") {
    val errors = compileErrors("Writer.tell(\"x\").at[State[Int] + Produce]")
    assert(errors.contains("does not fit in the row"), errors)
  }

  test("the row erases: an operation of a union row is held raw") {
    val p: Int ! Row = State.get[Int].at[Row].flatMap(n => produce(n).at[Row])
    val Free.Bind(Free.Inject(op), _) = (p.resume: @unchecked)
    assertEquals((op: Any).getClass.getName, classOf[State.Get[_]].getName)
  }

  test("bind and andThen: a bind across rows infers the union") {
    val p: Int ! (State[Int] + Produce) = State.get[Int].bind(n => produce(n + 1))
    assertEquals(State.handle(1)(p).runWith, (1, 2))
    val q: Int ! (State[Int] + Produce) = State.set(5).andThen(produce(7))
    assertEquals(State.handle(1)(q).runWith, (5, 7))
  }

  test("split: the F side by its test, G by exclusion, and <|> is the same at Left/Right") {
    // an operation of a union row comes out of a program: the union's
    // Op is abstract, so nothing builds one directly
    def opOf(p: Int ! (State[Int] + Produce)): (State[Int] + Produce)#Op[Int] = (p: @unchecked) match { case Free.Inject(e) => e }
    val e1 = opOf(State.get[Int].at[State[Int] + Produce])
    val e2 = opOf(produce(3).at[State[Int] + Produce])
    assertEquals(Split.<|>[State[Int], Produce, Int](e1), Left(State.Get[Int]()))
    assertEquals(Split.<|>[State[Int], Produce, Int](e2), Right(Produce.Emit(3)))
  }

  test("Effect.of refuses a row: a union has no ClassTag for its abstract Op") {
    val errors = compileErrors("Effect.of[State[Int] + Produce]")
    assert(errors.contains("No ClassTag"), errors)
  }
}
