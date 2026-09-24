package okay2.cats

import _root_.cats.{Monad, MonadError}
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import _root_.cats.syntax.all._
import okay2._
import okay2.Produce.produce
import CatsInterop.{Into, foldTo, toIO, toCats, fromCats}
import okay2.cats.instances._

class TestCatsInterop extends munit.FunSuite {

  test("a program row is a cats Monad, and tailRecM through it is stack-safe") {
    type Prog[A] = A ! Produce
    val M = Monad[Prog]
    val p: Prog[Int] = M.flatMap(M.pure(1))(x => produce(x + 1))
    assertEquals(p.runWith, 2)
    val n = 1000000
    val counted: Prog[Int] = M.tailRecM(0)(i => produce(if (i < n) Left(i + 1) else Right(i)))
    assertEquals(counted.runWith, n)
  }

  test("a row with Throws at its head is a MonadError: raiseError raises, handleErrorWith recovers") {
    type Row = Throws[String] + Produce
    type Prog[A] = A ! Row
    val E = MonadError[Prog, String]
    val failed: Prog[Int] = E.raiseError[Int]("no")
    val mended: Prog[Int] = E.handleErrorWith(failed)(e => produce(e.length).at[Row])
    assertEquals(Throws.runEither(failed).runWith, Left("no"))
    assertEquals(Throws.runEither(mended).runWith, Right(2))
    // cats' own syntax reaches the instance
    val viaSyntax: Prog[Int] = failed.handleError(_ => 7)
    assertEquals(Throws.runEither(viaSyntax).runWith, Right(7))
  }

  test("foldTo: a program interpreted into Option, Either and IO by an Into") {
    val p: Int ! Produce = produce(1).flatMap(x => produce(x + 1).map(_ + x))
    val intoOption: Into[Produce, Option] = new Into.Of[Produce, Option] {
      def apply[X](e: Produce.Emit[X]): Option[X] = Some(e.a)
    }
    assertEquals(foldTo[Option, Int, Produce](p)(intoOption), Some(3))
    type Err[A] = Either[String, A]
    val intoEither: Into[Produce, Err] = new Into.Of[Produce, Err] {
      def apply[X](e: Produce.Emit[X]): Either[String, X] = Right(e.a)
    }
    assertEquals(foldTo[Err, Int, Produce](p)(intoEither), Right(3))
    val intoIO: Into[Produce, IO] = new Into.Of[Produce, IO] {
      def apply[X](e: Produce.Emit[X]): IO[X] = IO.pure(e.a)
    }
    assertEquals(foldTo[IO, Int, Produce](p)(intoIO).unsafeRunSync(), 3)
  }

  test("foldTo is stack-safe: 1M operations into Option") {
    val n = 1000000
    val p = (1 to n).foldLeft(pure[Produce, Int](0))((m, _) => m.flatMap(x => produce(x + 1)))
    val intoOption: Into[Produce, Option] = new Into.Of[Produce, Option] {
      def apply[X](e: Produce.Emit[X]): Option[X] = Some(e.a)
    }
    assertEquals(foldTo[Option, Int, Produce](p)(intoOption), Some(n))
  }

  test("the Io row: IO as an operation, State handled first, the rest folded into one IO") {
    type Row = State[Int] + Io
    val p: Int ! Row = for {
      n <- State.get[Int].at[Row]
      m <- Io.lift(IO(n * 2)).at[Row]
      _ <- State.set(m).at[Row]
      k <- Io.lift(IO.pure(1)).at[Row]
    } yield m + k
    val io: IO[(Int, Int)] = Io.run(State.handle(21)(p))
    assertEquals(io.unsafeRunSync(), (42, 43))
  }

  test("the Io row beside another effect, both interpreted into IO by a union Into") {
    type Row = Produce + Io
    implicit val produceIntoIO: Into[Produce, IO] = new Into.Of[Produce, IO] {
      def apply[X](e: Produce.Emit[X]): IO[X] = IO.pure(e.a)
    }
    implicit val rowIntoIO: Into[Row, IO] = Into.union[Produce, Io, IO]
    val p: Int ! Row = produce(20).at[Row].flatMap(x => Io.lift(IO(x + 22)).at[Row])
    assertEquals(toIO(p).unsafeRunSync(), 42)
    // an IO's effects happen when the IO runs, not when the tree is built
    var ran = 0
    val q: Unit ! Io = Io.lift(IO { ran += 1 })
    val built = Io.run(q)
    assertEquals(ran, 0)
    built.unsafeRunSync()
    assertEquals(ran, 1)
  }

  test("cats.free.Free both ways: the same answers, and a round trip") {
    val p: Int ! Produce = produce(1).flatMap(x => produce(x + 1).map(_ + x))
    val c = toCats[Produce, Int](p)
    val answered = c.foldMap(new (Produce.Emit ~> Option) {
      def apply[X](e: Produce.Emit[X]): Option[X] = Some(e.a)
    })
    assertEquals(answered, Some(3))
    assertEquals(fromCats[Produce, Int](c).runWith, 3)
    // a 100k chain survives both directions
    val n = 100000
    val long = (1 to n).foldLeft(pure[Produce, Int](0))((m, _) => m.flatMap(x => produce(x + 1)))
    assertEquals(fromCats[Produce, Int](toCats[Produce, Int](long)).runWith, n)
  }

  private type ~>[F[_], G[_]] = _root_.cats.arrow.FunctionK[F, G]
}
