package okay

import Condition.*
import Condition.Decision.*
import okay.Direct.*

/** conditions x direct (specs/condition.md, Direct style): the
 * signal is a call that may return — the Common Lisp reading */
class TestConditionDirect extends munit.FunSuite {

  test("signal.reflect resumes AT the mark; progress before it survives") {
    var steps = Vector.empty[String]
    val prog: Int ! Op = direct {
      steps :+= "before"
      val v = signal[Int]("how many?").reflect
      steps :+= s"after($v)"
      v + 1
    }
    val out = !.run(Condition.run[Int, Pure]((_, _) => Resume(41))(prog))
    assertEquals(out, 42)
    assertEquals(steps, Vector("before", "after(41)"))
  }

  test("within under .reflect — Invoke unwinds to the frame, between never resumes") {
    var trail = Vector.empty[String]
    val prog: String ! Op = direct {
      val a = within[String, Pure]("use-default")(direct {
        trail :+= "inside"
        val v = signal[String]("bad value").reflect
        trail :+= "never"
        v
      })(v => s"default:$v").reflect
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
        val v = signal[String]("bad").reflect
        v
      }(v => s"skipped:$v").reflect
      trail :+= "outside"
      a
    }
    val out = !.run(Condition.run[String, Pure]((_, _) => Invoke("skip", "x"))(prog))
    assertEquals(out, "skipped:x")
    assertEquals(trail, Vector("inside", "outside"))
  }

  // pinned verbatim from docs/direct-style.md as a bare, unused-result
  // statement (its own REPL-echo style) — the discard warning is
  // silenced on this helper rather than reshaping the pinned text
  // (2026-09-25)
  @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
  private def docConditionDemo(prog: Int ! Op): Unit =
    !.run(Condition.run[Int, Pure]((_, _) => Resume(41))(prog))   // 42; before, after(41)

  test("docs/direct-style.md: a condition reads as a call that may return, marked with .!?") {
    var steps = Vector.empty[String]
    val prog: Int ! Op = direct {
      steps :+= "before"
      val v = signal[Int]("how many?").!?   // raise; Resume(41) lands HERE
      steps :+= s"after($v)"               // ... and this line runs
      v + 1
    }
    docConditionDemo(prog)
    assertEquals(!.run(Condition.run[Int, Pure]((_, _) => Resume(41))(prog)), 42)
    assertEquals(steps, Vector("before", "after(41)", "after(41)"))
  }

  test("docs/direct-style.md: the frame door, marked with .!?") {
    val prog: String ! Op = direct {
      val a = frame[String, Pure]("skip") {
        val v = signal[String]("bad").!?      // policy says Invoke("skip", x)
        v                                    // ...so this never runs
      }(v => s"skipped:$v").!?                // ...and the frame answers
      a
    }
    val out = !.run(Condition.run[String, Pure]((_, _) => Invoke("skip", "x"))(prog))
    assertEquals(out, "skipped:x")
  }

  test("a signal in a for-do loop: repair per element, mid-stream") {
    // the operator's story: malformed elements repaired by the
    // policy, the loop continues from each signal point
    val seen = collection.mutable.ListBuffer[Int]()
    val prog: Unit ! Op = direct {
      for x <- List(1, -2, 3) do
        val v = (if x < 0 then signal[Int](s"bad: $x") else pure[Op, Int](x)).reflect
        seen += v
    }
    !.run(Condition.run[Unit, Pure]((_, _) => Resume(0))(prog))
    assertEquals(seen.toList, List(1, 0, 3))
  }
}
