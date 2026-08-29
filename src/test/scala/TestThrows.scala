package okay

case class Fault(msg: String) extends Exception(msg)

class TestThrows extends munit.FunSuite {
  test("the union absorbs values, errors, Either and Try") {
    val x = List[String throws Fault]("a", Right("b"), Fault("c"),
      unsafe(throw Fault("d")), unsafe(s"${0 / 1}"), unsafe(s"${0 / 0}"))
    println(x)
  }

  test("safe: only a value to extract") {
    val y: String throws Safe = "safe"
    println(y)
    println(y.?)
    println(y.?(println))
    println(y.??)
  }

  test("bridges") { // JVM -> Throws effect -> Either / throws / JVM throw
    def div(n: Int, d: Int) = catching(unsafe[Int, Unsafe](n / d))
    assertEquals(!.run(runEither[Int, Nothing, Unsafe](div(84, 2))), Right(42))
    assert(!.run(runEither[Int, Nothing, Unsafe](div(1, 0))).isLeft)
    assertEquals(!.run(runThrows[Int, Nothing, Unsafe](div(84, 2))).?, 42)
    assertEquals(!.run(runThrows[Int, Nothing, Unsafe](div(1, 0))).?(_ => -1), -1)
    assertEquals(!.run(runUnsafe[Int, Nothing, Unsafe](div(84, 2))), 42)
    intercept[ArithmeticException](!.run(runUnsafe[Int, Nothing, Unsafe](div(1, 0))))
  }

  test("for") { // direct-style for-comprehension on throws, companion-scoped
    import throws.*
    def half(n: Int): Int throws Unsafe =
      unsafe[Int, Unsafe](if n % 2 == 0 then n / 2 else throw Fault(s"odd $n"))
    val ok = for x <- half(84); y <- half(x) yield x + y
    assertEquals(ok.?, 63)
    val bad = for x <- half(84); y <- half(21) yield x + y
    assertEquals(bad.?(_ => -1), -1)
  }
}
