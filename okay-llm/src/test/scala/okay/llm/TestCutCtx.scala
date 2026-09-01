package okay.llm

import okay.*
import okay.given

/** the capability door on Cut: guard/watched/violation with the
 * prompt ambient — behavior identical to the explicit forms */
class TestCutCtx extends munit.FunSuite {

  def tokens(ts: String*): Unit ! (Writer % String + Async) =
    ts.foldLeft(pure[Writer % String + Async, Unit](())):
      (m, t) => m.flatMap(_ => effect[Writer % String + Async, Unit](Writer(t)))

  def collect[A](p: Either[Cut.Violation, A] ! (Writer % String + Async))
  : (Seq[String], Either[Cut.Violation, A]) =
    val (out, a) = !.run(Writer.run(okay.Async.run[Either[Cut.Violation, A], Writer % String](p)))
    (out, a)

  test("guard { watched(...) } — the validator holds no prompt; a violation cuts") {
    val (out, ans) = collect(Cut.guard[Unit]:
      Cut.watched[Unit](tokens("ok", "ok", "BAD", "never"))(
        (i, t) => if t == "BAD" then Some(Cut.Violation("no-bad", i, t)) else None))
    assertEquals(out, Seq("ok", "ok"))
    assertEquals(ans, Left(Cut.Violation("no-bad", 2, "BAD")))
  }

  test("a passing stream under guard is identical to the unguarded run") {
    val (out, ans) = collect(Cut.guard[Unit]:
      Cut.watched[Unit](tokens("a", "b"))((_, _) => None))
    assertEquals(out, Seq("a", "b"))
    assertEquals(ans, Right(()))
  }
}
