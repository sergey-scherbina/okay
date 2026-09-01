package okay

import Condition.*
import Condition.Decision.*
import okay.Direct.*

/** conditions x direct (specs/condition.md, Direct style): the
 * signal is a call that may return — the Common Lisp reading */
class TestConditionDirect extends munit.FunSuite {

  test("signal.? resumes AT the mark; progress before it survives") {
    var steps = Vector.empty[String]
    val prog: Int ! Op = direct {
      steps :+= "before"
      val v = signal[Int]("how many?").?
      steps :+= s"after($v)"
      v + 1
    }
    val out = !.run(Condition.run[Int, Pure]((_, _) => Resume(41))(prog))
    assertEquals(out, 42)
    assertEquals(steps, Vector("before", "after(41)"))
  }

  test("within under .? — Invoke unwinds to the frame, between never resumes") {
    var trail = Vector.empty[String]
    val prog: String ! Op = direct {
      val a = within[String, Pure]("use-default")(direct {
        trail :+= "inside"
        val v = signal[String]("bad value").?
        trail :+= "never"
        v
      })(v => s"default:$v").?
      trail :+= "outside"
      a
    }
    val out = !.run(Condition.run[String, Pure] { (_, menu) =>
      assertEquals(menu, Vector("use-default"))
      Invoke("use-default", "42")
    }(prog))
    assertEquals(out, "default:42")
    assertEquals(trail, Vector("inside", "outside"))
  }

  test("the frame door: the body IS a direct block") {
    var trail = Vector.empty[String]
    val prog: String ! Op = direct {
      val a = frame[String, Pure]("skip") {
        trail :+= "inside"
        val v = signal[String]("bad").?
        v
      }(v => s"skipped:$v").?
      trail :+= "outside"
      a
    }
    val out = !.run(Condition.run[String, Pure]((_, _) => Invoke("skip", "x"))(prog))
    assertEquals(out, "skipped:x")
    assertEquals(trail, Vector("inside", "outside"))
  }

  test("a signal in a for-do loop: repair per element, mid-stream") {
    // the operator's story: malformed elements repaired by the
    // policy, the loop continues from each signal point
    val seen = collection.mutable.ListBuffer[Int]()
    val prog: Unit ! Op = direct {
      for x <- List(1, -2, 3) do
        val v = (if x < 0 then signal[Int](s"bad: $x") else pure[Op, Int](x)).?
        seen += v
    }
    !.run(Condition.run[Unit, Pure]((_, _) => Resume(0))(prog))
    assertEquals(seen.toList, List(1, 0, 3))
  }
}
