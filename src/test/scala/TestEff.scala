package okay

import org.junit.*

import scala.util.chaining.*

class TestEff {

  @Test def t1(): Unit = {
    val x = State.index(List("a", "b", "c", "d", "e", "f", "g"), 1).tap(println)
    Assert.assertEquals((8, List((7, "g"), (6, "f"), (5, "e"), (4, "d"), (3, "c"), (2, "b"), (1, "a"))), x)
  }

  @Test def t21(): Unit = {
    val p = fib[BigInt, Producer]
    val n = 100
    println("Run pure")
    val x = p.next(n).?.tap(println)
    println("Run effect")
    val y = p.next(n)(using Producer.log()).?.tap(println)
    Assert.assertEquals(x, y)
    println("Test stack safety")
    p.next(100000).?.tap(println)
    println("Okay!")
  }

}