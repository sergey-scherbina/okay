package okay

import !.*

import scala.util.chaining.*

class TestGenerate extends munit.FunSuite {

  // the million-element run proves STACK safety, not speed; under a
  // full-family parallel run a 30s default has twice been the only
  // thing that failed, so the clock gets room the property does not need
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  test("fibs by laziness: LazyList") {
    println(fibs[BigInt, LazyList].take(1000).force)
    assertEquals(fibs[Int, LazyList].take(10).toList,
      List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  test("fibs by effects: Producer, pure and logged") {
    val p = fibs[BigInt, Producer]
    val n = 100
    val x = p.next(n).?.tap(println)
    val y = p.next(n)(using Producer.log()).?.tap(println)
    assertEquals(x, y)
  }

  test("stack safety: 1M produced values") {
    fibs[BigInt, Producer].next(1000000).?.tap(println)
  }

}
