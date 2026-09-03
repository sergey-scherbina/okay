package okay.agent

import okay.given
import okay.{!, %, +, Async, Handler, Writer}
import okay.codec.Schema
import okay.llm.Structured

/**
 * Does the early stop stop anything? (specs/intent-classify.md)
 *
 * The live measurement found 0% saved in both regimes it could
 * produce, for opposite reasons, which leaves the mechanism itself
 * unexamined — and a live model cannot examine it, because steering a
 * 4B model into "valid JSON first, prose after" is its own research
 * project. A synthetic stream settles it in the default gate: the
 * source counts what it was actually asked for, so "the walk stopped
 * pulling" becomes an assertion instead of an inference.
 */
class TestCutStops extends munit.FunSuite {

  import IntentFixture.Meeting
  private given sMeeting: Schema[Meeting] = summon[Schema[Meeting]]
  private val mReading = Classify.reading[Meeting]

  /** a stream that says how far it was pulled */
  private def counted(pieces: List[String], emitted: java.util.concurrent.atomic.AtomicInteger)
  : Unit ! (Writer % String + Async) =
    import okay.!.*
    type F = Writer % String + Async
    pieces.foldRight(okay.pure(()): Unit ! F) { (p, rest) =>
      okay.!.widen[Unit, Writer % String, F](Writer.tell[String](p)).flatMap { _ =>
        emitted.incrementAndGet(): Unit
        rest
      }
    }

  test("the walk stops pulling once the value is complete") {
    val value = Reading(List(Span[Meeting]("x", "y",
      List(Alt(Meeting.MeetingProposal("meet"), Conf.High)))))
    val json = okay.codec.Json.write(value)(using mReading)
    // the value, then a great deal of prose nobody should pay for
    val pieces: List[String] = json.toList.map(_.toString) ++ List.fill(500)(" and then some commentary")
    val emitted = new java.util.concurrent.atomic.AtomicInteger(0)

    val cut = Structured.cut[Reading[Meeting]](counted(pieces, emitted))(
      using mReading, summon[Handler[Async]])

    assertEquals(cut.value, Some(value), "the value did not decode")
    assert(cut.stopped, "the walk ran to the end of the stream")
    assert(cut.tokens <= json.length + 1,
      s"the walk consumed ${cut.tokens} tokens for a ${json.length}-character value")
    assert(emitted.get() < pieces.length,
      s"the SOURCE was pulled to the end (${emitted.get()} of ${pieces.length}) — " +
      "nothing was actually avoided")
  }

  test("a value that never completes costs the whole stream — the safe direction") {
    val emitted = new java.util.concurrent.atomic.AtomicInteger(0)
    val pieces = List("prose, no json here", " and more of it", " and more")
    val cut = Structured.cut[Reading[Meeting]](counted(pieces, emitted))(
      using mReading, summon[Handler[Async]])
    assertEquals(cut.value, None)
    assert(!cut.stopped, "nothing completed, so nothing should have been declared complete")
    assertEquals(emitted.get(), pieces.length)
  }
}
