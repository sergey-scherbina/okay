package okay

import Condition.*
import Condition.Decision.*

/** typed signals (specs/condition.md): the answer type rides the
 * condition, and a wrong-typed resume stops compiling */
class TestConditionTyped extends munit.FunSuite {

  object HowMany extends Of[Int]
  object WhichName extends Of[String]

  test("a typed condition round-trips through the typed resume") {
    val prog: Int ! Op =
      for
        n <- HowMany.signal
        s <- WhichName.signal
      yield n + s.length
    val out = !.run(Condition.run[Int, Pure] {
      case (HowMany, _) => resume(HowMany)(40)
      case (WhichName, _) => resume(WhichName)("ab")
      case (_, _) => Fail
    }(prog))
    assertEquals(out, 42)
  }

  test("a wrong-typed resume is a compile error at the policy") {
    val e = compileErrors(
      "import okay.Condition.*; object HowMany extends Of[Int]; resume(HowMany)(\"not an int\") ")
    assert(e.nonEmpty)
    assert(e.contains("Int") || e.contains("String"), e)
  }

  test("Of IS an Answers instance: raiseC takes it without a given, and a bad resume is named") {
    val viaRaise: Int ! Op = raiseC(HowMany).map(_ + 1)
    val out = !.run(Condition.run[Int, Pure] {
      case (HowMany, _) => Resume(41)
      case _ => Fail
    }(viaRaise))
    assertEquals(out, 42)
    val bad = intercept[BadResume] {
      !.run(Condition.run[Int, Pure]((_, _) => Resume("not an int"))(HowMany.signal))
    }
    assert(bad.getMessage.contains("not an int"), bad.getMessage)
  }

  test("the typed edge works in direct blocks, restarts unchanged") {
    import okay.Direct.*
    val prog: String ! Op = direct {
      val a = within[String, Pure]("fallback")(direct {
        val n = HowMany.signal.reflect
        s"got:$n"
      })(v => s"fell:$v").reflect
      a
    }
    val resumed = !.run(Condition.run[String, Pure] {
      case (HowMany, _) => resume(HowMany)(7)
      case _ => Fail
    }(prog))
    assertEquals(resumed, "got:7")
    val invoked = !.run(Condition.run[String, Pure]((_, _) => Invoke("fallback", "x"))(prog))
    assertEquals(invoked, "fell:x")
  }
}
