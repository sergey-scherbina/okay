package okay

import org.junit.Test

class TestCont {

  @Test def t1(): Unit = {
    extension [A1, A2](t: (() => A1, () => A2))
      inline def ? : (A1, A2) = (t._1(), t._2())

    inline def delay[A, B](a: => A) =
      shift((k: A => B) => () => k(a))

    val example1 = reset(for {
      _ <- delay(println("Hello,"))
      _ <- delay(println("World!"))
      _ <- delay(println("Goodbye!"))
    } yield ())

    val example2 = reset(for {
      _ <- delay(println("1"))
      _ <- delay(println("2"))
      _ <- delay(println("3"))
      _ <- delay(println("4"))
    } yield ())

    (example1, example2).?.?.?
  }

}