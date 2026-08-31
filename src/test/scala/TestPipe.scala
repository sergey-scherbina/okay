package okay

import !.*

/** Coroutine pipelines: tell meets await, one element at a time. */
class TestPipe extends munit.FunSuite {

  def count(n: Int): Nothing ! Writer % Int =
    Writer.tell(n).flatMap(_ => count(n + 1))

  def sums(n: Int, acc: Int): Int ! Take % Int =
    if n == 0 then pure(acc)
    else Take.await[Int].flatMap:
      case Some(x) => sums(n - 1, acc + x)
      case None => pure(acc)

  test("the consumer drives: a finite consumer ends an infinite producer") {
    assertEquals(pipe(count(0))(sums(5, 0)), 0 + 1 + 2 + 3 + 4)
  }

  test("a producer that ends first answers None to every further await") {
    val short: Int ! Writer % Int =
      Writer.tell(10).flatMap(_ => Writer.tell(20)).map(_ => 0)
    assertEquals(pipe(short)(sums(5, 0)), 30)
    val nones: Int ! Take % Int =
      Take.await[Int].flatMap(_ => Take.await[Int]).flatMap(_ =>
        Take.await[Int].map(_.fold(-1)(identity)))
    assertEquals(pipe(short)(nones), -1)
  }

  test("stages: a producer through transducers, demand-driven") {
    def double: Stage[Int, Int, Unit] =
      Stage.await[Int, Int].flatMap {
        case Some(x) => Stage.tell[Int, Int](x * 2).flatMap(_ => double)
        case None => pure(())
      }
    // infinite producer, two stages, finite consumer: still lazy
    assertEquals(pipe(through(count(0))(double))(sums(3, 0)), 0 + 2 + 4)
    // through is associative on behavior
    val s1 = through(through(count(0))(double))(Stage.id[Int])
    val s2 = through(count(0))(through(double)(Stage.id[Int]))
    assertEquals(s1.toLazyList.take(5).toList, s2.toLazyList.take(5).toList)
  }

  test("chunked/unchunk stages: batching with a flush, then flattening back") {
    val ten: Int ! Writer % Int =
      (1 to 10).foldLeft(Writer.tell(0).map(_ => 0))((m, i) =>
        m.flatMap(_ => Writer.tell(i).map(_ => i)))
    val chunks = through(ten)(Stage.chunked[Int](4)).toLazyList.toList
    assertEquals(chunks.map(_.length), List(4, 4, 3))   // 0..10 is eleven told values
    assertEquals(chunks.flatten, (0 to 10).toList)
    val back = through(through(ten)(Stage.chunked[Int](4)))(Stage.unchunk[Int])
    assertEquals(back.toLazyList.toList, (0 to 10).toList)
  }

  test("an effectful producer pipes into a program of its effects") {
    type F = Writer % Int + Async
    def ticks(n: Int): Unit ! F =
      effect[F, Unit](Async.Run(() => Thread.sleep(1))).flatMap: _ =>
        effect[F, Unit](Writer(n)).flatMap(_ => ticks(n + 1))
    val result: Int ! Async = pipe[Int, Unit, Int, Async](ticks(7))(sums(3, 0))
    assertEquals(result.runWith, 7 + 8 + 9)
  }

  test("effectful stages: G ops forward through composition, lazily") {
    type Row = Take % Int + (Writer % Int + Async)
    var effects = 0
    // an effectful stage: an Async op before every doubled output
    def double: Unit ! Row =
      effect[Row, Option[Int]](Take.Await()).flatMap {
        case Some(x) =>
          effect[Row, Unit](Async.Run(() => effects += 1)).flatMap(_ =>
            effect[Row, Unit](Writer(x * 2)).flatMap(_ => double))
        case None => pure(())
      }
    // a pure stage widened into the row (union ACI lets the ascription)
    def incPure: Stage[Int, Int, Unit] =
      Stage.await[Int, Int].flatMap {
        case Some(x) => Stage.tell[Int, Int](x + 1).flatMap(_ => incPure)
        case None => pure(())
      }
    val inc: Unit ! Row = !.widen[Unit, Take % Int + Writer % Int, Async](incPure)

    type Src = Writer % Int + Async
    var told = 0
    def src(n: Int): Unit ! Src =
      if n == 0 then pure(())
      else effect[Src, Unit](Async.Run(() => told += 1)).flatMap(_ =>
        effect[Src, Unit](Writer(n)).flatMap(_ => src(n - 1)))

    // stage∘stage, then the producer through the composite
    val composed = through[Int, Int, Int, Async, Unit, Unit](double)(inc)
    val out = through[Int, Int, Async, Unit, Unit](src(3))(composed)
    assertEquals(effects, 0)   // programs are values: nothing ran yet
    assertEquals(told, 0)
    assertEquals(out.toLazyList.toList, List(7, 5, 3))   // 3,2,1 doubled + 1
    assertEquals(effects, 3)
    assertEquals(told, 3)

    // associativity holds with effects in the row
    effects = 0; told = 0
    val left = through[Int, Int, Async, Unit, Unit](
      through[Int, Int, Async, Unit, Unit](src(3))(double))(inc)
    assertEquals(left.toLazyList.toList, List(7, 5, 3))
  }
}
