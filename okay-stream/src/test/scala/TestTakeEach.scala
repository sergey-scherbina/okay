package okay

/**
 * specs/direct-loops.md v3 — `Take.each[I]`: the consumer side of an
 * iteratee as a source. Its `foreach` is the consumer loop as a
 * program; `pipe` pairs it with a producer as it pairs any consumer.
 */
class TestTakeEach extends munit.FunSuite:

  def told(xs: Int*): Unit ! Writer % Int =
    xs.foldLeft(pure(()): Unit ! Writer % Int)((p, x) => p.flatMap(_ => Writer.tell(x)))

  test("Take.each.loop is the consumer loop: every element the producer tells, then the end") {
    val seen = scala.collection.mutable.Buffer[Int]()
    pipe(told(1, 2, 3))(Take.each[Int].loop(seen += _ * 2))
    assertEquals(seen.toList, List(2, 4, 6))
    seen.clear()
    pipe(told())(Take.each[Int].loop(seen += _))
    assertEquals(seen.toList, Nil)
  }

  test("a finite consumer ends an infinite producer — the loop pulls only what it reads") {
    def naturals(n: Int): Unit ! Writer % Int = Writer.tell(n).flatMap(_ => naturals(n + 1))
    val firstThree: List[Int] ! Take % Int =
      Take.each[Int].step.flatMap {
        case Some((a, rest)) => rest.step.flatMap {
          case Some((b, rest2)) => rest2.step.map(_.map((c, _) => List(a, b, c)).getOrElse(List(a, b)))
          case None => pure(List(a))
        }
        case None => pure(Nil)
      }
    assertEquals(pipe(naturals(0))(firstThree), List(0, 1, 2))
  }
