package okay.freer
import scala.collection.mutable.ListBuffer

/** cont-program-answer's programs, shared by the cross suite and the 128 KB one */
object ContProgramAnswer:
  type Ans = Int ! Pure
  type Dyn = Int ! Shift % ? + Pure

  /** `n` levels, each an opaque body that calls `k` itself and answers a program, built by `leaf` */
  def nested[S](n: Int, step: S => S)(leaf: ((Int => S) => S) => Cps[Int, S, S]): Cps[Int, S, S] =
    (1 to n).foldLeft(Cps.Pure[Int, S](0))((m, _) => m.flatMap(x => leaf(k => step(k(x + 1)))))

  def pureAns(n: Int, leaf: ((Int => Ans) => Ans) => Cps[Int, Ans, Ans]): Ans =
    Cps.run(nested[Ans](n, _.map(identity))(leaf))((v: Int) => pure(v))

  def dynAns(n: Int): Dyn =
    Cps.run(nested[Dyn](n, _.map(identity))(Cps.programLeaf[Int, Dyn, Dyn]))((v: Int) => pure(v))

/** specs/cont-js-depth.md, cont-program-answer: an opaque body that calls `k` and answers a program gets a lazy
 * `k` — no nested run — with the strict leaf's answers, on every platform */
class TestContProgramAnswer extends munit.FunSuite:
  import ContProgramAnswer.*

  test("the same answers as the strict leaf: multi-shot, drop, and a rest that maps") {
    def both(body: (Int => Ans) => Ans): (Int, Int) =
      (!.run(Cps.run(Cps.programLeaf[Int, Ans, Ans](body).map(_ * 2))((v: Int) => pure(v))),
       !.run(Cps.run(Cps.shiftLeaf[Int, Ans, Ans](body).map(_ * 2))((v: Int) => pure(v))))
    val (twice, twiceStrict) = both(k => k(1).flatMap(a => k(10).map(b => a + b)))
    assertEquals(twice, 22)
    assertEquals(twice, twiceStrict)
    val (dropped, droppedStrict) = both(_ => pure(7))
    assertEquals(dropped, droppedStrict)
    val (once, onceStrict) = both(k => k(5).map(_ + 1))
    assertEquals(once, onceStrict)
  }

  test("the contract: host effects after k(a) in the body run before k's rest (the strict leaf: after)") {
    def order(leaf: ((Int => Ans) => Ans) => Cps[Int, Ans, Ans]): List[String] =
      val log = ListBuffer.empty[String]
      val c = leaf(k => { val p = k(1); log += "body"; p }).map(v => { log += "rest"; v })
      val _ = !.run(Cps.run(c)((v: Int) => pure(v)))
      log.toList
    assertEquals(order(Cps.programLeaf[Int, Ans, Ans]), List("body", "rest"))
    assertEquals(order(Cps.shiftLeaf[Int, Ans, Ans]), List("rest", "body"))
  }

  test("the macro picks the lazy k for a body that calls k itself and answers a program") {
    val log = ListBuffer.empty[String]
    val c = Cps.shift[Int, Ans, Ans](k => { val p = k(1); log += "body"; p.flatMap(v => k(v)) })
      .map(v => { log += "rest"; v })
    val _ = !.run(Cps.run(c)((v: Int) => pure(v)))
    assertEquals(log.toList.take(2), List("body", "rest"))
  }

  test("a million nested program-answered bodies: forced by the Free fold, no nested run") {
    assertEquals(!.run(pureAns(1000000, Cps.programLeaf[Int, Ans, Ans])), 1000000)
  }

  test("a million nested, consumed by a RUNNING machine: it steps in and continues into each answer") {
    assertEquals(!.run(Shift.run[Int, Pure](dynAns(1000000))), 1000000)
  }
