package okay.zio

class TestFree2 extends munit.FunSuite {

  import TestFree2.*
  
  test("test") {
    println("test")

    ZioBenchmark.run
  }

}

object TestFree2 {

  import zio.*

  // Взаимная рекурсия на ZIO
  def isEvenZIO(n: Long): UIO[Boolean] =
    if n == 0 then ZIO.succeed(true)
    else ZIO.succeed(n - 1).flatMap(isOddZIO) // Правоассоциативный flatMap

  def isOddZIO(n: Long): UIO[Boolean] =
    if n == 0 then ZIO.succeed(false)
    else ZIO.succeed(n - 1).flatMap(isEvenZIO)

  object ZioBenchmark extends ZIOAppDefault:
    val run = for {
      start <- Clock.nanoTime
      // Запускаем 10 миллионов шагов
      result <- isEvenZIO(10000000L)
      end <- Clock.nanoTime
      _ <- Console.printLine(s"Результат ZIO: $result")
      _ <- Console.printLine(s"Время выполнения: ${(end - start) / 1000000.0} мс")
    } yield ()
}