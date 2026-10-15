package okay.llm

import okay.{Async, async}
import okay.*
import okay.std.*
import okay.given
import Cut.*

/** specs/llm-agentic.md, llm-streaming-cut — one test per box
 * (the live probe is the TestLive pattern, env-gated elsewhere) */
class TestCut extends munit.FunSuite {

  /** a scripted token source that COUNTS its pulls — the witness for
   * "the cut stops the pull" */
  final class Counted(tokens: List[String]):
    var pulled = 0
    def source: Unit ! Writer % String + Async =
      def go(ts: List[String]): Unit ! Writer % String + Async = ts match
        case Nil => pure(())
        case t :: rest =>
          // the count is an Async op — DATA until run — so it ticks
          // when the token is pulled, not when the node is built
          !.widen[Unit, Async, Writer % String](async { pulled += 1 }).flatMap(_ =>
            Writer.tell(t).at[Writer % String + Async]).flatMap(_ => go(rest))
      go(tokens)

  def collect[A](p: Either[Violation, A] ! Writer % String + Async)
  : (Vector[String], Either[Violation, A]) =
    val (ts, a) = Async.run(
      Writer.run[String, Either[Violation, A], Writer % String + Async](p)).run
    (ts.toVector, a)

  test("a violating stream: Left names rule and position, no further pulls") {
    val src = Counted(List("fine", "fine", "FORBIDDEN", "never", "never"))
    val (emitted, result) = collect(guarded[String] { p =>
      checked(p, src.source)((i, t) =>
        if t == "FORBIDDEN" then Some(Violation("no-forbidden", i, t)) else None)
        .map(_ => "done")
    })
    assertEquals(result, Left(Violation("no-forbidden", 2, "FORBIDDEN")))
    assertEquals(emitted, Vector("fine", "fine"))   // the poisoned token never flowed
    assertEquals(src.pulled, 3, "the cut must stop the pull")
  }

  test("a passing stream is identical to the unguarded run") {
    val plain = Counted(List("a", "b", "c"))
    val (direct, _) = collect(
      plain.source.map[Either[Violation, Unit]](Right(_)))
    val guardedSrc = Counted(List("a", "b", "c"))
    val (through, result) = collect(guarded[Unit] { p =>
      checked(p, guardedSrc.source)((_, _) => None)
    })
    assertEquals(through, direct)
    assertEquals(result, Right(()))
    assertEquals(guardedSrc.pulled, plain.pulled)
  }

  test("nested guards: the inner violation aborts the INNER prompt; the outer continues") {
    val outer = guarded[String] { po =>
      // an inner, stricter guard INSIDE the outer one: its cut ends
      // the nearest guard, the inner
      guarded[Unit] { p2 =>
        checked(p2, Counted(List("ok", "BAD")).source)((i, t) =>
          if t == "BAD" then Some(Violation("inner", i, t)) else None)
      }.at[Writer % String + (Cutting + Async)].flatMap {
        case Left(v) =>
          // the inner scope cut; the OUTER stream continues past it
          checked(po, Counted(List("outer-goes-on")).source)((_, _) => None)
            .map(_ => s"recovered-from-${v.rule}")
        case Right(_) => pure("inner-passed")
      }
    }
    val (emitted, result) = collect(outer)
    assertEquals(result, Right("recovered-from-inner"))
    assertEquals(emitted, Vector("ok", "outer-goes-on"))
  }
}
