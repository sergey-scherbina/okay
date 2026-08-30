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
      (1 to 10).foldLeft(Writer.tell(0).map(_ => 0))((m, i) => m.flatMap(_ => Writer.tell(i)))
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
        effect[F, Int](Writer(n)).flatMap(_ => ticks(n + 1))
    val result: Int ! Async = pipe[Int, Unit, Int, Async](ticks(7))(sums(3, 0))
    assertEquals(result.runWith, 7 + 8 + 9)
  }
}
