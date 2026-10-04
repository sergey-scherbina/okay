package okay2

/** Layer 1 A keeps the meaning of every body (cont-stack-okay2-macro): order, laziness, failure, multi-shot */
class TestContMacroMeaning extends munit.FunSuite {

  test("statements before the tail call run once, in order, when the runner reaches the shift") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val m = Cont.shift[Int, Int, Int] { k => log += "a"; log += "b"; k(1) }
    assertEquals(log.toList, Nil)
    assertEquals(Cont.reset(m.map(_ + 1)), 2)
    assertEquals(log.toList, List("a", "b"))
    assertEquals(Cont.reset(m), 1)
    assertEquals(log.toList, List("a", "b", "a", "b"))
  }

  test("an exception from the body surfaces at run time, not at construction") {
    val m = Cont.shift[Int, Int, Int](k => if (k == null) k(1) else throw new IllegalStateException("run"))
    val e = intercept[IllegalStateException](Cont.reset(m))
    assertEquals(e.getMessage, "run")
  }

  test("bodies that are NOT tail-shaped keep their meaning") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(1) + k(10))), 11)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](_ => 42).map(_ + 1)), 42)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if (true) k(5) else 0)), 5)
    // multi-shot: an outer k resumed twice, a tail body under it
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z => Cont.shift[Int, Int, Int](k => k(z * 10)))), 30)
  }

  test("a tail body's answer type: S <: R, at the types the call gave") {
    val m: Cont[Int, String, Any] = Cont.shift[Int, String, Any](k => k(3))
    assertEquals(Cont.run(m)(_.toString), "3")
  }

  test("the Control[Cont] instance's shift keeps its meaning") {
    val c = Control[Cont.Rep]
    assertEquals(c.run(c.shift[Int, Int, Int](k => k(5)))(identity), 5)
  }
}
