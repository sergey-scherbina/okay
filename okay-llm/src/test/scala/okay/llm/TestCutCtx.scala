package okay.llm

import okay.{Async}
import okay.*
import okay.std.*
import okay.given
/** the capability door on Cut: guard/watched/violation with the
 * prompt ambient — behavior identical to the explicit forms */
class TestCutCtx extends munit.FunSuite {

  def tokens(ts: String*): Unit ! Writer % String + Async =
    ts.foldLeft(pure[Unit, Writer % String + Async](())):
      (m, t) => m.flatMap(_ => Writer.tell(t).at[Writer % String + Async])

  def collect[A](p: Either[Cut.Violation, A] ! Writer % String + Async)
  : (Seq[String], Either[Cut.Violation, A]) =
    val (out, a) = Writer.run[String, Either[Cut.Violation, A], Writer % String](okay.Async.run(p)).run
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
