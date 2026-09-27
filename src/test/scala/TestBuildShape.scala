package okay

import okay.!.*

/** specs/left-nested-build-cost.md: the right-nested builders */
class TestBuildShape extends munit.FunSuite {
  type W = Writer % String

  test("each performs in order, and the program is a value: it runs again") {
    val p: Unit ! W = !.each(List("a", "b", "c"))(s => Writer.tell(s))
    assertEquals(!.run(Writer.run[String, Unit, Pure](p))._1, List("a", "b", "c"))
    assertEquals(!.run(Writer.run[String, Unit, Pure](p))._1, List("a", "b", "c"))
  }

  test("foldM threads the accumulator through effects, left to right") {
    val p: Int ! State % Int = !.foldM(1 to 4)(0)((acc, x) => State.modify[Int](_ + x).map(s => acc * 10 + s))
    assertEquals(!.run(State.handle[Int](0)(p)), (10, 1 * 1000 + 3 * 100 + 6 * 10 + 10))
  }

  test("foldM collecting answers in order; the empty input answers at once") {
    val p: Vector[Int] ! W = !.foldM(Vector(1, 2, 3))(Vector.empty[Int])((bs, x) => Writer.tell(x.toString).map(_ => bs :+ x * 2))
    assertEquals(!.run(Writer.run[String, Vector[Int], Pure](p)), (List("1", "2", "3"), Vector(2, 4, 6)))
    assertEquals(!.run(!.foldM(Vector.empty[Int])(0)((a, x) => pure[Pure, Int](a + x))), 0)
    assertEquals(!.run(!.each(Nil: List[Int])(_ => pure[Pure, Unit](()))), ())
  }

  test("the program is RIGHT-nested: its head is one operation, not a chain to rotate") {
    val p: Unit ! W = !.each(1 to 5)(i => Writer.tell(i.toString))
    // a foldLeft build's root is a Bind whose left side is another Bind,
    // four deep here; this one's root is Bind(Inject(...), k)
    p match
      case Free.Bind(Free.Inject(_), _) => ()
      case other => fail(s"not right-nested: ${other.getClass.getSimpleName}")
  }

  test("stack-safe over 1 000 000 elements") {
    val n = 1000000
    val p: Int ! State % Int = !.foldM(0 until n)(0)((acc, _) => State.modify[Int](_ + 1).map(_ => acc + 1))
    assertEquals(!.run(State.handle[Int](0)(p)), (n, n))
  }
}
