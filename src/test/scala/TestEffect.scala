package okay

import org.junit.*

import scala.util.chaining.*

class TestEffect {

  @Test def t1(): Unit = {
    val x = State.index(List("a", "b", "c", "d", "e", "f", "g"), 1).tap(println)
    Assert.assertEquals(List(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e", 6 -> "f", 7 -> "g"), x)
  }

  @Test def t2(): Unit = {
    val p = fib[BigInt, Producer]
    println("Run pure")
    val x = p.next(100).?.tap(println)
    println("Run effect")
    val y = p.next(100)(using Producer.log()).?.tap(println)
    Assert.assertEquals(x, y)
  }

}