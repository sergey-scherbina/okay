package okay.intent

import okay.codec.Schema

/**
 * The selection rule, offline and deterministic
 * (tod-demonstrations-from-the-log). What the model does with the
 * demonstrations is measured live and recorded in the spec; what is
 * ours to assert is that the same log yields the same prompt, that a
 * class appears once, that a message about to be classified is never
 * shown as its own example, and that a log's bad replies are dropped
 * rather than inherited.
 */
class TestDemonstrations extends munit.FunSuite {

  enum Meeting derives Schema:
    case MeetingProposal(what: String)
    case MeetingRequest(what: String)
    case MeetingNotification(what: String)
    case NotAboutMeetings(what: String)

  private given sM: Schema[Meeting] = summon[Schema[Meeting]]
  private val sr = Classify.reading[Meeting]

  private val log: List[(String, Meeting)] = List(
    "Shall we meet Thursday?" -> Meeting.MeetingProposal("meet Thursday"),
    "Another proposal entirely" -> Meeting.MeetingProposal("second proposal"),
    "Send me the contract." -> Meeting.MeetingRequest("send contract"),
    "Payroll runs early." -> Meeting.MeetingNotification("payroll"),
    "What is the capital of Portugal?" -> Meeting.NotAboutMeetings("general knowledge"))

  test("one per class, in the taxonomy's own order, the first the log offers") {
    val ds = Demonstrations.perClass(log)
    assertEquals(ds.map(_._1), List(
      "Shall we meet Thursday?", "Send me the contract.",
      "Payroll runs early.", "What is the capital of Portugal?"))
    // the second Proposal is not a second demonstration
    assertEquals(ds.length, 4)
    assertEquals(Demonstrations.perClass(log), ds, "the same log yields the same prompt")
  }

  test("a limit takes the first k of that order; a message being classified is never its own example") {
    assertEquals(Demonstrations.perClass(log, limit = 2).map(_._1),
      List("Shall we meet Thursday?", "Send me the contract."))
    val ds = Demonstrations.perClass(log, exclude = Set("Shall we meet Thursday?"))
    assertEquals(ds.head._1, "Another proposal entirely", "the excluded message gave way to the next of its class")
    assertEquals(ds.length, 4)
    assertEquals(Demonstrations.perClass(log, exclude = log.map(_._1).toSet), Nil)
  }

  test("from a log of raw replies: decodable ones become demonstrations, the rest are dropped") {
    val good = Classify.prompt[Meeting]("x")   // not used, but the same schema pairing
    assert(good.nonEmpty)
    val reply = okay.codec.Json.write(Reading(List(Span(
      text = "Shall we meet Thursday?", why = "proposes a time",
      alts = List(Alt(Conf.High, Meeting.MeetingProposal("meet Thursday")))))))(using sr)
    val recorded = List(
      "Shall we meet Thursday?" -> reply,
      "torn" -> """{"spans":[{"text":"torn""",                  // undecodable
      "empty" -> """{"spans":[]}""")                             // decoded, no span
    val ds = Demonstrations.fromReplies[Meeting](recorded)(using sr)
    assertEquals(ds.map(_._1), List("Shall we meet Thursday?"))
    assertEquals(ds.head._2, Meeting.MeetingProposal("meet Thursday"))
  }

  test("what the prompt will show is what render prints, and the prompt carries it") {
    val ds = Demonstrations.perClass(log, limit = 1)
    val shown = Demonstrations.render(ds)
    assert(shown.contains("Shall we meet Thursday?"))
    assert(shown.contains("MeetingProposal"))
    assert(Classify.prompt[Meeting]("some message", ds).contains(shown.trim.take(30)))
  }
}
