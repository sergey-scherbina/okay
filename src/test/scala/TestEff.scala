package okay

import org.junit.*

class TestEff {

  val p = fib[BigInt, Produce]

  @Test def t1(): Unit = {
    for (n <- 0 to 10) {
      println(p.skip(n).?)
    }
  }

  @Test def t2(): Unit = {
    import scala.util.Try
    import scala.concurrent.*
    import scala.concurrent.duration.*
    import ExecutionContext.Implicits.global

    given Handler[Pure] with
      override def apply[A, B](a: A): A \ B = k => {
        println(a)
        k(a)
      }

    Try(Await.ready(Future(p.run), 1.micros))
  }

}