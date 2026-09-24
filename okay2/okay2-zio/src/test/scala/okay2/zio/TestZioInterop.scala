package okay2.zio

import _root_.zio.{ZIO, Task, Runtime, Unsafe, ZLayer}
import _root_.zio.stream.ZStream
import okay2._
import okay2.Produce.produce
import ZioInterop.{IntoZ, foldTo, toZStream}

/** a service for the environment test: at the top level, where `Tag` derives cleanly */
trait Log { def log(a: Any): Unit }

class TestZioInterop extends munit.FunSuite {

  private def run[E, A](z: ZIO[Any, E, A]): A =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  test("the Zio row: a Task as an operation, State handled first, the rest folded into one Task") {
    type Row = State[Int] + Zio
    val p: Int ! Row = for {
      n <- State.get[Int].at[Row]
      m <- Zio.lift(ZIO.attempt(n * 2)).at[Row]
      _ <- State.set(m).at[Row]
    } yield m + 1
    val z: Task[(Int, Int)] = Zio.run(State.handle(21)(p))
    assertEquals(run(z), (42, 43))
  }

  test("foldTo: an environment and an error type of the caller's choosing") {
    type Row = Produce + Zio
    val seen = List.newBuilder[Any]
    // the caller's IntoZ answers Produce THROUGH a service in the environment
    val produceZ: IntoZ[Produce, Log, Throwable] = new IntoZ.Of[Produce, Log, Throwable] {
      def apply[X](e: Produce.Emit[X]): ZIO[Log, Throwable, X] = ZIO.serviceWith[Log](_.log(e.a)).as(e.a)
    }
    val env: IntoZ[Zio, Log, Throwable] = new IntoZ.Of[Zio, Log, Throwable] {
      def apply[X](e: Task[X]): ZIO[Log, Throwable, X] = e
    }
    val p: Int ! Row = produce(20).at[Row].flatMap(x => Zio.lift(ZIO.attempt(x + 22)).at[Row])
    val z: ZIO[Log, Throwable, Int] =
      foldTo[Log, Throwable, Int, Row](p)(IntoZ.union[Produce, Zio, Log, Throwable](Produce.effect, produceZ, env, implicitly[Distinct[Produce + Zio]]))
    val log: Log = new Log { def log(a: Any): Unit = seen += a }
    assertEquals(run(z.provideLayer(ZLayer.succeed(log))), 42)
    assertEquals(seen.result(), List(20))
    // a ZIO's effects happen when it runs, not when the tree is built
    var ran = 0
    val q: Unit ! Zio = Zio.lift(ZIO.attempt { ran += 1 })
    val built = Zio.run(q)
    assertEquals(ran, 0)
    run(built)
    assertEquals(ran, 1)
  }

  test("stack safety: 1M operations folded into ZIO") {
    val n = 1000000
    val p = (1 to n).foldLeft(pure[Produce, Int](0))((m, _) => m.flatMap(x => produce(x + 1)))
    val into: IntoZ[Produce, Any, Nothing] = new IntoZ.Of[Produce, Any, Nothing] {
      def apply[X](e: Produce.Emit[X]): ZIO[Any, Nothing, X] = ZIO.succeed(e.a)
    }
    assertEquals(run(foldTo[Any, Nothing, Int, Produce](p)(into)), n)
  }

  test("a Writer program beside Zio is a ZStream, the ZIO run between the elements") {
    type Row = Writer[String] + Zio
    var side = List.empty[String]
    val p: Unit ! Row = for {
      _ <- Writer.tell("a").at[Row]
      _ <- Zio.lift(ZIO.attempt { side ::= "zio" }).at[Row]
      _ <- Writer.tell("b").at[Row]
    } yield ()
    val s: ZStream[Any, Throwable, String] = toZStream[Any, Throwable, String, Unit, Zio](p)
    assertEquals(side, Nil)
    assertEquals(run(s.runCollect).toList, List("a", "b"))
    assertEquals(side, List("zio"))
    side = Nil
    assertEquals(run(s.take(1).runCollect).toList, List("a"))
    assertEquals(side, Nil)
  }

  test("stack safety: a million tells as a ZStream") {
    val n = 100000
    val p: Unit ! Writer[Int] = (1 to n).foldLeft(pure[Writer[Int], Unit](()))((m, i) => m.flatMap(_ => Writer.tell(i)))
    val s = toZStream[Any, Nothing, Int, Unit, Pure](p)
    assertEquals(run(s.runFold(0L)(_ + _)), n.toLong * (n + 1) / 2)
  }
  // fromZStream is scoped now: TestZioAsync
}
