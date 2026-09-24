package okay2

import Produce.produce

/**
 * The row discipline since stage 7 (specs/okay2.md): a row is an
 * INTERSECTION of requirements (`+` is `with`) and `Free` is
 * contravariant in it — so widening is subtyping, the order a row is
 * written in does not matter, and a handler infers the rest of the row.
 * And the erasure argument at run time: an operation of a row of
 * several is held RAW in the tree.
 */
class TestRow extends munit.FunSuite {

  type Row3 = State[Int] + Writer[String] + Reader[Int]

  val prog: Int ! Row3 = for {
    n <- State.get[Int]
    _ <- Writer.tell("saw " + n)
    k <- Reader.ask[Int]
    _ <- State.set(n + k)
  } yield n * 10

  test("handlers in all SIX orders over three effects, the rest inferred with no annotation") {
    val results = List(
      !.run(Writer.run(Reader.run(5)(State.handle(1)(prog)))),
      !.run(Reader.run(5)(Writer.run(State.handle(1)(prog)))),
      !.run(State.handle(1)(Reader.run(5)(Writer.run(prog)))),
      !.run(Reader.run(5)(State.handle(1)(Writer.run(prog)))),
      !.run(State.handle(1)(Writer.run(Reader.run(5)(prog)))),
      !.run(Writer.run(State.handle(1)(Reader.run(5)(prog)))))
    assertEquals(results(0), (Seq("saw 1"), (6, 10)))
    assertEquals(results(2), (6, (Seq("saw 1"), 10)))
    assertEquals(results.size, 6)
  }

  test("widening is subtyping: a one-effect program IS a program in a wider row, in either order") {
    val one: Int ! State[Int] = State.get[Int]
    val wide: Int ! (Reader[Int] + State[Int]) = one
    val other: Int ! (State[Int] + Reader[Int]) = wide
    assertEquals(!.run(State.handle(7)(Reader.run(0)(wide))), (7, 7))
    assertEquals(!.run(Reader.run(0)(State.handle(7)(other))), (7, 7))
    // the order a row is written in does not matter
    implicitly[(Int ! (State[Int] + Writer[String])) <:< (Int ! (Writer[String] + State[Int]))]
    implicitly[(Int ! (Writer[String] + State[Int])) <:< (Int ! (State[Int] + Writer[String]))]
    // `.at` is the identity, kept so a call site can name the row
    val named: Int ! Row3 = one.at[Row3]
    assert(named eq one)
  }

  test("a helper polymorphic in the rest of the row, spelled with Free") {
    def bump[R <: okay2.Row](by: Int): Int ! (State[Int] + R) = State.get[Int].flatMap(n => State.set(n + by))
    def countFrom[R <: okay2.Row, A](p: Free[State[Int] with R, A]): (Int, A) ! R = State.handle[Int, A, R](0)(p)
    assertEquals(!.run(State.handle(0)(bump[Pure](3))), (3, 3))
    val (ws, (s, a)) = !.run(Writer.run(Reader.run(5)(countFrom(prog))))
    assertEquals((ws, s, a), (Seq("saw 0"), 5, 0))
  }

  test("Pure rides into any row") {
    val p: Int ! Pure = pure(1)
    val q: Int ! Row3 = p
    assertEquals(!.run(Writer.run(Reader.run(0)(State.handle(0)(q)))), (Seq(), (0, 1)))
  }

  test("an effect left unhandled: run refuses the program") {
    val errors = compileErrors("!.run(State.handle(1)(prog))")
    assert(errors.contains("type mismatch"), errors)
  }

  test("the row erases: an operation of a row of several is held raw") {
    val p: Int ! (State[Int] + Produce) = State.get[Int].flatMap(n => produce(n))
    val Free.Bind(Free.Inject(op), _) = (p.resume: @unchecked)
    assertEquals(op.getClass.getName, classOf[State.Get[_]].getName)
  }

  test("bind and andThen: a bind across rows infers the union") {
    val p: Int ! (State[Int] + Produce) = State.get[Int].bind(n => produce(n + 1))
    assertEquals(State.handle(1)(p).runWith, (1, 2))
    val q: Int ! (State[Int] + Produce) = State.set(5).andThen(produce(7))
    assertEquals(State.handle(1)(q).runWith, (5, 7))
  }

  test("split: the F side typed by its test, the rest untyped, and <|> is the same at Left/Right") {
    def opOf(p: Int ! (State[Int] + Produce)): Any = (p: @unchecked) match { case Free.Inject(e) => e }
    val e1 = opOf(State.get[Int])
    val e2 = opOf(produce(3))
    assertEquals(Split.<|>[State[Int], Int](e1), Left(State.Get[Int]()))
    assertEquals(Split.<|>[State[Int], Int](e2), Right(Produce.Emit(3)))
  }

  test("THE TRAP the kernel avoids: an intersection's #Op is its LAST parent's") {
    // which is why `Inject` holds an operation as Any, and a typed view
    // exists only at one signature: read at this type, a Writer
    // operation would be checkcast to State.Op and fail
    implicitly[(Writer[String] + State[Int])#Op[Int] =:= State.Op[Int, Int]]
    implicitly[(State[Int] + Writer[String])#Op[Unit] =:= Writer.Op[String, Unit]]
  }

  test("a row has no TypeableK: the split tests one signature, never a union") {
    val errors = compileErrors("implicitly[TypeableK[State[Int] + Produce]]")
    assert(errors.contains("no TypeableK"), errors)
  }
}
