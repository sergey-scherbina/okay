package okay2

import Produce.produce

/** a second signature for the relay test */
sealed trait Op extends Row { type Op[+A] = okay2.Op.Val[A] }
object Op {
  final case class Val[+A](a: A)
  implicit val effect: Effect[Op] = Effect.of[Op]
  def op[A](a: A): A ! Op = Free.inject[Op, A](Val(a))
}

class TestEffects extends munit.FunSuite {

  test("runWith: a program over one signature, by its Handler") {
    val p: Int ! Produce = produce(1).flatMap(x => produce(x + 1).map(y => x + y))
    assertEquals(p.runWith, 3)
    assertEquals(p.peek, 1) // the tree can be stepped
  }

  test("stack safety: runWith over a 1M bind chain") {
    val n = 1000000
    val e = (1 to n).foldLeft(pure[Produce, Int](0)) { (m, _) =>
      m.flatMap(x => produce(x + 1))
    }
    assertEquals(e.runWith, n)
  }

  test("!.tailcall: mutual tail recursion across two functions, stack-safe") {
    def isEven(n: Int): Boolean ! Pure =
      if (n == 0) pure(true) else !.tailcall(isOdd(n - 1))
    def isOdd(n: Int): Boolean ! Pure =
      if (n == 0) pure(false) else !.tailcall(isEven(n - 1))
    assert(!.run(isEven(1000000)))
    assertEquals(!.run(isOdd(1000000)), false)
    assertEquals(isEven(1000000).resume, Free.Return[Pure, Boolean](true))
    assertEquals(isEven(1000000).peek, true)
  }

  test("!.loop: tailRecM for programs, 1M iterations") {
    val p: Int ! Produce = !.loop[Int, Int, Produce](0)(i => produce(if (i < 1000000) Left(i + 1) else Right(i)))
    assertEquals(p.runWith, 1000000)
  }

  test("Effects.handle: abort and forwarding (Throws)") {
    type F = Throws[String] + Produce
    def calc(b: Boolean): Int ! F =
      produce(2).at[F].flatMap { x =>
        (if (b) Throws.raise[String, Int]("boom").at[F] else produce(3).at[F]).map(_ + x)
      }

    def run(b: Boolean): Int =
      !.handle[Throws[String], Produce](calc(b))(a => pure(a))(new Interpr[Throws[String], Int ! Produce] {
        def apply[X](e: Throws.Op[String, X]): Cont[X, Int ! Produce, Int ! Produce] =
          shift[X, Int ! Produce, Int ! Produce](_ => pure(-1))
      }).runWith

    assertEquals(run(false), 5)
    assertEquals(run(true), -1)

    assertEquals(Throws.runEither(calc(false)).runWith, Right(5))
    assertEquals(Throws.runEither(calc(true)).runWith, Left("boom"))
  }

  test("Effects.handle: a multi-shot handler resumes the rest of the program twice") {
    type F = Op + Produce
    val p: Int ! F = Op.op(1).at[F].flatMap(x => produce(x * 10).at[F].map(_ + 1))
    val both: List[Int] ! Produce =
      !.handle[Op, Produce](p)(a => pure(List(a)))(new Interpr[Op, List[Int] ! Produce] {
        def apply[X](e: Op.Val[X]): Cont[X, List[Int] ! Produce, List[Int] ! Produce] =
          shift[X, List[Int] ! Produce, List[Int] ! Produce](k => k(e.a).flatMap(a => k(e.a).map(b => a ++ b)))
      })
    assertEquals(both.runWith, List(11, 11))
  }

  test("stack safety: a 1M tail-resumptive relay with forwarding") {
    val n = 1000000
    type FG = Op + Produce
    val prog = (1 to n).foldLeft(Op.op(0).at[FG]) { (m, i) =>
      m.flatMap(x => if (i % 2 == 0) Op.op(x + 1).at[FG] else produce(x + 1).at[FG])
    }
    val handled: Int ! Produce = !.relay[Int, Int, Op, Produce](prog)(a => pure(a))(new Relay[Op] {
      def apply[X, Y](o: Op.Val[X]): X /> Y = Cont.Pure(o.a)
    })
    assertEquals(handled.runWith, n)
  }

  test("stack safety: 1M handled operations under Effects.handle") {
    val n = 1000000
    type FG = Op + Produce
    val prog = (1 to n).foldLeft(Op.op(0).at[FG]) { (m, _) =>
      m.flatMap(x => Op.op(x + 1).at[FG])
    }
    val handled: Int ! Produce =
      !.handle[Op, Produce](prog)(a => pure(a))(new Interpr[Op, Int ! Produce] {
        def apply[X](o: Op.Val[X]): Cont[X, Int ! Produce, Int ! Produce] = Cont.Pure(o.a)
      })
    assertEquals(handled.runWith, n)
  }

  test("translate: a handler valued in ANOTHER ROW, not in a value") {
    type Row = Reader[Int] + (Writer[String] + Pure)

    val prog: Int ! Row =
      Reader.ask[Int].at[Row].flatMap(x =>
        Reader.ask[Int].at[Row].map(_ + x))

    // the Reader is answered by a program that TELLS on the way
    val told: Int ! (Writer[String] + Pure) =
      !.translate[Int, Reader[Int], Writer[String] + Pure](prog)(new Interpret[Reader[Int], Writer[String] + Pure] {
        // `X` is a method type parameter, which scalac 2 does not refine
        // by the constructor (a type test, not a constructor pattern);
        // the answer is asserted, as the Scala 3 core's own test does
        // (`21.asInstanceOf[X]`)
        def apply[X](e: Reader.Op[Int, X]): X ! (Writer[String] + Pure) = e match {
          case _: Reader.Ask[_] => Writer.tell("asked").at[Writer[String] + Pure].map(_ => 21.asInstanceOf[X])
        }
      })

    val (ws, a) = !.run(Writer.run(told))
    assertEquals(a, 42)
    assertEquals(ws, Seq("asked", "asked"))
  }

  test("translate forwards the effects it was not given") {
    type Row = Reader[Int] + (Writer[String] + Pure)
    val prog: Int ! Row =
      Writer.tell("before").at[Row].flatMap(_ =>
        Reader.ask[Int].at[Row]).flatMap(x =>
        Writer.tell("after").at[Row].map(_ => x))

    val told = !.translate[Int, Reader[Int], Writer[String] + Pure](prog)(new Interpret[Reader[Int], Writer[String] + Pure] {
      def apply[X](e: Reader.Op[Int, X]): X ! (Writer[String] + Pure) = e match {
        case _: Reader.Ask[_] => pure(5.asInstanceOf[X])
      }
    })
    val (ws, a) = !.run(Writer.run(told))
    assertEquals(a, 5)
    assertEquals(ws, Seq("before", "after"))
  }

  test("Handler.union: a row run by one handler per effect") {
    type Row = Op + Produce
    implicit val opH: Handler[Op] = new Handler[Op] { def handle[A](a: Op.Val[A]): A = a.a }
    implicit val rowH: Handler[Row] = Handler.union[Op, Produce]
    val p: Int ! Row = Op.op(1).at[Row].flatMap(x => produce(x + 1).at[Row])
    assertEquals(p.runWith, 2)
    // recording is a decorator over the real handler
    val log = List.newBuilder[Any]
    assertEquals(p.runWith(rowH.tracing(log += _)), 2)
    assertEquals(log.result(), List(Op.Val(1), Produce.Emit(2)))
  }

  test("next: stepping a program one operation at a time") {
    val p: Int ! Produce = produce(1).flatMap(x => produce(x + 1)).flatMap(y => produce(y + 1))
    val one = p.next()
    assertEquals(one.peek, 2)
    assertEquals(one.runWith, 3)
    // the last operation has no continuation: `next` leaves it as the
    // lone Inject it is, as the Scala 3 core's does
    assertEquals(p.next(2), Free.Inject[Produce, Int](Produce.Emit(3)))
    assertEquals(p.next(3).peek, 3)
  }
}
