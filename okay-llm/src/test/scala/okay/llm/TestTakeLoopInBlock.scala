package okay.llm

import okay.*
import okay.Direct.*

/**
 * specs/direct-loops.md v3 — `for i <- Take.each[I] do …` INSIDE a
 * direct block: the iteratee written as a loop. It lives here because
 * okay-llm is a module whose test classpath holds both okay-direct
 * (the macro) and okay-stream (`Take`, `pipe`, `through`), which do
 * not depend on each other; the loop itself is the same road as over
 * any `Pull`, tested in okay-direct.
 */
class TestTakeLoopInBlock extends munit.FunSuite:

  def told(xs: Int*): Unit ! Writer % Int =
    xs.foldLeft(pure(()): Unit ! Writer % Int)((p, x) => p.flatMap(_ => Writer.tell(x)))

  test("a Stage written as a loop: for i <- Take.each[Int] do tell(i * 2)") {
    val doubling: Stage[Int, Int, Unit] = direct[[A] =>> A ! Take % Int + Writer % Int] {
      for i <- Take.each[Int] do Writer.tell(i * 2).?
    }
    val (out, _) = !.run(Writer.run[Int, Unit, okay.Pure](through(told(1, 2, 3))(doubling)))
    assertEquals(out, Seq(2, 4, 6))
  }

  test("a consumer as a loop with a guard, piped to a producer: only the odd elements are seen") {
    val seen = scala.collection.mutable.Buffer[Int]()
    val odd: Unit ! Take % Int = direct[[A] =>> A ! Take % Int] {
      for i <- Take.each[Int] if i % 2 == 1 do seen += i
    }
    pipe(told(1, 2, 3, 4, 5))(odd)
    assertEquals(seen.toList, List(1, 3, 5))
  }
