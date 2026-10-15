package okay.std.tests

import okay.*
import okay.std.Static

/** the free selective on the root's words: effects known before the run, the same program run by `toProgram` */
class TestStatic extends munit.FunSuite {
  enum Fetch[+A]:
    case Get(k: Int) extends Fetch[Int]
  import Fetch.*

  given Answers[Fetch] with
    def handle[A](e: Fetch[A]): A = e match
      case Get(k) => k * 10

  test("leaves: every operation before the run; toProgram runs them") {
    val spine: Static[Fetch, Seq[Int]] = traverse(1 to 3)(i => Static.op(Get(i)))
    assertEquals(spine.leaves, Vector(Get(1), Get(2), Get(3)))
    assertEquals(spine.toProgram.runWith, Seq(10, 20, 30))
  }

  test("a 10 000-deep spine converted and run in constant stack") {
    val spine: Static[Fetch, Seq[Int]] = traverse(1 to 10000)(i => Static.op(Get(i)))
    assertEquals(spine.toProgram.runWith.length, 10000)
  }

  test("select runs its scrutinee and at most one side") {
    var asked = List.empty[Int]
    given Answers[Fetch] with
      def handle[A](e: Fetch[A]): A = e match
        case Get(k) => asked ::= k; k
    val sel = summon[Selective[[A] =>> Static[Fetch, A]]]
    val e: Static[Fetch, Either[Int, Int]] = Static.op(Get(1)).map(Left(_))
    val p: Static[Fetch, Int] = sel.select(e)(Static.op(Get(2)).map(two => (one: Int) => one + two))
    assertEquals(p.leaves.length, 2)
    assertEquals(p.toProgram.runWith, 3)
    assertEquals(asked.reverse, List(1, 2))
  }

  test("the tutorial's lines (§23, §24): leaves before the run, the same value run the ordinary way") {
    val keys = 1 to 3
    val plan = traverse(keys)(k => Static.op(Get(k)))   // the same traverse
    assertEquals(plan.leaves.length, 3)
    val same = plan.toProgram.runWith  // and the same value, run the ordinary way
    val ran = plan.toProgram.runWith         // the ordinary program, run the ordinary way
    assertEquals((same, ran), (Seq(10, 20, 30), Seq(10, 20, 30)))
  }
}
