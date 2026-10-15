package okay.std.tests

import okay.*
import okay.std.*

/** okay-std's one multi-prompt abort (`Escape`): a scope answers a leave to its own prompt, re-raises others past
 * its boundary, and a leave no scope answered fails by name */
class TestEscape extends munit.FunSuite {
  type W = Writer % String
  type R = Escape + W
  def say(s: String): Unit ! R = Writer.tell(s).at[R]
  def go[A](p: A ! R): (List[String], A) =
    val (w, a) = Writer.run(Escape.run[A, R](p)).run
    (w.toList, a)

  test("a body that does not leave answers its own value") {
    assertEquals(go(Escape.prompt[Int, R](_ => say("a").map(_ => 1))), (List("a"), 1))
  }

  test("a leave answers its scope with the value, the rest of the scope dropped") {
    val p = Escape.prompt[Int, R] { k =>
      for _ <- say("a"); x <- Escape.escape[Int, Int](k)(7).at[R]; _ <- say("never") yield x + 100
    }.flatMap(n => say("after").map(_ => n))
    assertEquals(go(p), (List("a", "after"), 7))
  }

  test("an inner scope is left ACROSS its boundary to an outer one") {
    val p = Escape.prompt[String, R] { outer =>
      Escape.prompt[Int, R] { _ =>
        for _ <- say("in"); x <- Escape.escape[Int, String](outer)("out").at[R] yield x
      }.flatMap(n => say("inner-continued").map(_ => n.toString))
    }
    assertEquals(go(p), (List("in"), "out"))
  }

  test("the ambient form leaves the NEAREST scope; a bound outer evidence crosses the inner one") {
    val nearest = Escape.prompted[Int, R](Escape.prompted[Int, R](Escape.leave[Int, Int](1).at[R]).map(_ + 10))
    assertEquals(go(nearest), (Nil, 11))
    val bound = Escape.prompted[Int, R] { (outer: Escape.Prompted[Int]) ?=>
      Escape.prompted[Int, R](Escape.leave[Int, Int](2)(using outer).at[R]).map(_ + 10)
    }
    assertEquals(go(bound), (Nil, 2))
  }

  test("a leave no scope answered fails by name at run") {
    var stray: Escape.Prompt[Int] | Null = null
    val p = Escape.prompt[Int, R] { k => stray = k; pure(0) }
      .flatMap(_ => Escape.escape[Int, Int](stray.nn)(1).at[R])
    intercept[Escape.NoPrompt](go(p))
  }

  test("a forged prompt or evidence does not compile") {
    assert(compileErrors("okay.std.Escape.Prompt[Int]()").nonEmpty)
    assert(compileErrors("""
      given okay.std.Escape.Prompted[Int] = okay.std.Escape.Prompted(null)
    """).nonEmpty)
  }
}
