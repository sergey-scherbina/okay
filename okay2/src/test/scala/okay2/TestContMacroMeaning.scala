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

  test("Layer 1 B: answer-using shapes keep their meaning, multi-shot included") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(1) + k(10))), 11)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(k(1)))), 1)
    assertEquals(Cont.reset(Cont.shift[String, String, String](k => s"${k("a")}-${k("b")}")), "a-b")
    assertEquals(Cont.reset(Cont.shift[List[Int], List[Int], List[Int]](k => 0 :: k(List(1)))), List(0, 1))
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int] { k => val a = k(1); val b = k(2); a * 10 + b }), 12)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if (k(1) > 0) k(2) + 1 else k(3))), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(1) match { case 1 => k(7); case _ => 0 })), 7)
    // the answer used by a continuation after the shift
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(1) + k(10)).flatMap(x => Cont.Pure[Int, Int](x * 2))), 22)
    // multi-shot across the walked body
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z => Cont.shift[Int, Int, Int](k => k(z) + 100))), 203)
  }

  test("Layer 1 B: what ran before a call of k still runs before it, and what came after still after") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    def pre(): Int = { log += "pre"; 1 }
    def post(): Int = { log += "post"; 1 }
    def add(a: Int, b: Int, c: Int): Int = a + b + c
    val m = Cont.shift[Int, Int, Int] { k => log += "body"; add(pre(), k(10), post()) }
      .flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    assertEquals(Cont.reset(m), 12)
    assertEquals(log.toList, List("body", "pre", "then 10", "post"))
  }

  test("Layer 1 B: an exception after a call of k surfaces once the rest has run") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    def boom(): Int = throw new IllegalStateException("boom")
    val m = Cont.shift[Int, Int, Int](k => k(1) + boom()).flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    val e = intercept[IllegalStateException](Cont.reset(m))
    assertEquals(e.getMessage, "boom")
    assertEquals(log.toList, List("then 1"))
  }

  test("Layer 1 B: bodies the transform cannot read stay opaque and keep their meaning") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => 1 + (if (true) k(1) else 2))), 2)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => try k(1) catch { case _: Exception => 0 })), 1)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option.empty[Int].getOrElse(k(4)))), 4)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int] { k => var i = 0; while (i < 3) i += k(1); i }), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2).map(x => k(x)).sum)), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (() => k(1))())), 1)
  }
}
