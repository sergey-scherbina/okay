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

  test("an effectful producer pipes into a program of its effects") {
    type F = Writer % Int + Async
    def ticks(n: Int): Unit ! F =
      effect[F, Unit](Async.Run(() => Thread.sleep(1))).flatMap: _ =>
        effect[F, Int](Writer(n)).flatMap(_ => ticks(n + 1))
    val result: Int ! Async = pipe[Int, Unit, Int, Async](ticks(7))(sums(3, 0))
    assertEquals(result.runWith, 7 + 8 + 9)
  }
}
