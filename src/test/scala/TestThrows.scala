package okay

import org.junit.*

case class Fault(msg: String) extends Exception(msg)

class TestThrows {
  @Test def testThrows1(): Unit = {
    val x = List[String throws Fault]("a", Right("b"), Fault("c"),
      unsafe(throw Fault("d")), unsafe(s"${0 / 1}"), unsafe(s"${0 / 0}"))

    println(x)
    // List(a, Right(b), TestResult$package$Fault$1: c, TestResult$package$Fault$1: d
    // , 0, Failure(java.lang.ArithmeticException: / by zero))
  }

  @Test def testThrows2(): Unit = {
    val y: String throws Safe = "safe"
    println(y)
    println(y.?)
    println(y.?(println))
    println(y.??)
  }
}

