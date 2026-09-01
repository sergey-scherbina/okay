package okay.llm

import okay.*
import okay.given
import okay.Condition.{Decision, Unhandled}
import Cut.*

/** The repair door (specs/condition.md x llm-streaming-cut): one
 * screened stream, the policy chooses per incident. */
class TestCutRepair extends munit.FunSuite {

  final class Counted(tokens: List[String]):
    var pulled = 0
    def source: Unit ! (Writer % String + Async) =
      def go(ts: List[String]): Unit ! (Writer % String + Async) = ts match
        case Nil => pure(())
        case t :: rest =>
          !.widen[Unit, Async, Writer % String](async { pulled += 1 }).flatMap(_ =>
            effect[Writer % String + Async, Unit](Writer(t))).flatMap(_ => go(rest))
      go(tokens)

  def collect[A](p: Either[Violation, A] ! (Writer % String + Async))
  : (Vector[String], Either[Violation, A]) =
    val (ts, a) = Async.run[(Seq[String], Either[Violation, A]), Pure](
      Writer.run[String, Either[Violation, A], Async](p)).runWith
    (ts.toVector, a)

  def screenedRun(src: Counted, policy: (Any, Vector[String]) => Decision)
  : (Vector[String], Either[Violation, String]) =
    collect(Cut.guard[String] {
      Condition.run[Unit, Writer % String + (Delim + Async)](policy)(
        screened[String](src.source)((i, t) =>
          if t == "BAD" then Some(Violation("no-bad", i, t)) else None))
        .map(_ => "done")
    })

  test("Resume replaces the token; the stream continues to the end") {
    val src = Counted(List("fine", "BAD", "also"))
    val (emitted, result) = screenedRun(src,
      { case (_: Violation, menu) =>
          assertEquals(menu, Vector("drop", "cut"))
          Decision.Resume("beep") })
    assertEquals(result, Right("done"))
    assertEquals(emitted, Vector("fine", "beep", "also"))
    assertEquals(src.pulled, 3)
  }

  test("drop makes the token vanish; the stream continues") {
    val src = Counted(List("fine", "BAD", "also"))
    val (emitted, result) = screenedRun(src,
      (_, _) => Decision.Invoke("drop", ()))
    assertEquals(result, Right("done"))
    assertEquals(emitted, Vector("fine", "also"))
  }

  test("cut falls back to the guard: Left, and the pull stops") {
    val src = Counted(List("fine", "BAD", "never"))
    val (emitted, result) = screenedRun(src,
      { case (v: Violation, _) => Decision.Invoke("cut", v)
        case _ => Decision.Fail })
    assertEquals(result, Left(Violation("no-bad", 1, "BAD")))
    assertEquals(emitted, Vector("fine"))
    assertEquals(src.pulled, 2, "the cut must stop the pull")
  }

  test("a clean stream never signals: the policy is never consulted") {
    val src = Counted(List("a", "b"))
    var consulted = 0
    val (emitted, result) = screenedRun(src,
      (_, _) => { consulted += 1; Decision.Fail })
    assertEquals(result, Right("done"))
    assertEquals(emitted, Vector("a", "b"))
    assertEquals(consulted, 0)
  }
}
