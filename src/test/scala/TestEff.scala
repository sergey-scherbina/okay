package okay

import org.junit.*
import util.chaining.*

class TestEff {

  val p: Produce[BigInt] = fib[BigInt, Produce]

  @Test def t1(): Unit = {
    println(p(100000).?)
  }

  @Test def t2(): Unit = {
    import scala.util.Try
    import scala.concurrent.*
    import scala.concurrent.duration.*
    import ExecutionContext.Implicits.global

    given Impure with
      inline override def impure[A](a: A): A =
        a.tap(println)

    Try(Await.ready(Future(p.run), 1.micros))
  }

}