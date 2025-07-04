package okay

import org.junit.Assert.*
import org.junit.Test

class TestPut {

  @Test def t1(): Unit = {
    println(fib[BigInt, LazyList].take(1000).force)
    assertEquals(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34),
      fib[Int, LazyList].take(10).toList)
  }

}