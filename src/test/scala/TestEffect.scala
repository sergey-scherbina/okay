package okay

import org.junit.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.given
import scala.util.Try
import scala.util.chaining.*

class TestEffect {

  @Test def t1(): Unit = {
    val p = fib[BigInt, Producer]
    println("Run pure:")
    println:
      p.next(10).pipe: x1 =>
        x1.next().pipe: x2 =>
          x2.next().pipe: x3 =>
            (x1.?.run, x2.?.run, x3.?.run)
    println("Run impure:")
    p.next(10)(using Impure.print("\n"))
    println("Run pure again:")
    Try(Await.ready(Future(p.run), 1.micros))
    println("Run impure again:")
    Try(Await.ready(Future(p.run(using Impure.print("\n"))), 1.micros))
  }

}