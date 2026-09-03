package okay.agent

import okay.codec.{Json, Schema}

/** specs/intent-classify.md */
class TestClassify extends munit.FunSuite {

  // ---------------------------------------------------------------
  // a taxonomy that exercises what the spec claims: two levels, slots
  // that are required, slots that are optional, and a slot whose own
  // schema can refuse a value

  final case class When(iso: String)
  object When:
    private val shape = raw"\d{4}-\d{2}-\d{2}(T\d{2}:\d{2})?".r
    given Schema[When] = Schema.refine[When, String](
      s => if shape.matches(s) then Right(When(s)) else Left(s"not ISO-8601: '$s'"),
      _.iso)

  enum ProposalKind derives Schema:
    case NewSlot(when: When, who: List[String], where: Option[String])
    case Reschedule(to: When)

  enum RequestKind derives Schema:
    case Send(what: String)
    case Confirm(what: String)

  enum Meeting derives Schema:
    case Proposal(p: ProposalKind)
    case Request(r: RequestKind)
    case Notification(text: String)
    case Other(note: String)

  import Classify.given
  private val sReading = summon[Schema[Reading[Meeting]]]

  private def alt(i: Meeting, c: Conf) = Alt(i, c)
  private def span(t: String, a: Alt[Meeting]*) = Span(t, "because", a.toList)

  private val slot = Meeting.Proposal(ProposalKind.NewSlot(When("2026-09-10T14:00"), List("ann"), None))

  // ---------------------------------------------------------------
  // the taxonomy IS the type

  test("a nested sum labels a value by its path through the groups") {
    assertEquals(Classify.label(slot), "Proposal/NewSlot")
    assertEquals(Classify.label(slot, depth = 1), "Proposal")
    assertEquals(Classify.label(Meeting.Other("hi")), "Other")
  }

  test("the taxonomy shown to the model is generated from the schema") {
    val t = Classify.taxonomy[Meeting]
    // every case of every level, without any of them written here twice
    for c <- Seq("Proposal", "Request", "Notification", "Other",
                 "NewSlot", "Reschedule", "Send", "Confirm")
    do assert(t.contains(c), s"taxonomy does not mention $c")
  }

  test("a required slot is required and an optional slot is not") {
    val t = Classify.taxonomy[Meeting]
    // `where` is Option, so it must not appear in a required list;
    // `when` and `who` are not, so they must
    assert(t.contains("\"when\""))
    assert(t.contains("\"who\""))
    assert(t.contains("\"where\""))
    val requiredLists = raw""""required":\[[^\]]*\]""".r.findAllIn(t).toList.mkString(" ")
    assert(requiredLists.contains("\"when\""), "when should be required")
    assert(!requiredLists.contains("\"where\""), "where is optional and must not be required")
  }

  test("the prompt states the confidence vocabulary from the enum") {
    val p = Classify.prompt[Meeting]("hello")
    assert(p.contains("low, medium, high"))
    assert(p.contains("hello"))
  }

  // ---------------------------------------------------------------
  // the parser is the same value

  test("a well-formed reply round-trips") {
    val r = Reading(List(span("thu?", alt(slot, Conf.High))))
    val decoded = Classify.read[Meeting](Json.write(r)(using sReading))
    assertEquals(decoded, Right(r))
  }

  test("a label outside the taxonomy is a decode error, not an Other") {
    val ok = Json.write(Reading(List(span("x", alt(Meeting.Other("n"), Conf.High)))))(using sReading)
    val bogus = ok.replace("\"Other\"", "\"Refund\"")
    Classify.read[Meeting](bogus) match
      case Left(e) => assert(e.contains("Refund") || e.contains("case"), s"unhelpful error: $e")
      case Right(v) => fail(s"a label outside the taxonomy decoded: $v")
  }

  test("a slot that fails its own schema is a decode error") {
    val ok = Json.write(Reading(List(span("x", alt(slot, Conf.High)))))(using sReading)
    val bogus = ok.replace("2026-09-10T14:00", "next thursday")
    Classify.read[Meeting](bogus) match
      case Left(e) => assert(e.contains("ISO-8601"), s"error did not name the slot's rule: $e")
      case Right(v) => fail(s"an unparseable slot decoded: $v")
    }

  test("an unknown confidence is a decode error") {
    val ok = Json.write(Reading(List(span("x", alt(slot, Conf.High)))))(using sReading)
    val bogus = ok.replace("\"high\"", "\"very sure\"")
    assert(Classify.read[Meeting](bogus).isLeft)
  }

  // ---------------------------------------------------------------
  // two axes, two mechanisms

  test("two intents in one message are two spans; one uncertain intent is one span with alts") {
    val multi = Reading(List(
      span("charged twice", alt(Meeting.Request(RequestKind.Send("refund")), Conf.High)),
      span("app crashes", alt(Meeting.Other("bug"), Conf.High))))
    Classify.decide(multi) match
      case Classify.Decision.Act(spans) => assertEquals(spans.length, 2)
      case other => fail(s"expected two actionable spans, got $other")

    val ambiguous = Reading(List(span("move thursday?",
      alt(slot, Conf.Low), alt(Meeting.Request(RequestKind.Confirm("slot")), Conf.Low))))
    assertEquals(ambiguous.spans.length, 1)
    assertEquals(ambiguous.spans.head.alts.length, 2)
  }

  test("the gate asks when the best alt is below the floor, and shows the alternatives") {
    val r = Reading(List(span("move thursday?",
      alt(slot, Conf.Low), alt(Meeting.Request(RequestKind.Confirm("slot")), Conf.Low))))
    Classify.decide(r) match
      case Classify.Decision.Clarify(sp) => assertEquals(sp.alts.length, 2)
      case other => fail(s"expected a clarification, got $other")
  }

  test("one unsure span blocks the whole message, not just its own span") {
    val r = Reading(List(
      span("sure", alt(slot, Conf.High)),
      span("unsure", alt(Meeting.Other("?"), Conf.Low))))
    assert(Classify.decide(r).isInstanceOf[Classify.Decision.Clarify[?]])
  }

  test("nothing found is Empty, not an empty Act") {
    assertEquals(Classify.decide(Reading[Meeting](Nil)), Classify.Decision.Empty)
  }

  test("a span with no alternatives at all is a clarification") {
    val r = Reading(List(Span[Meeting]("odd", "no idea", Nil)))
    assert(Classify.decide(r).isInstanceOf[Classify.Decision.Clarify[?]])
  }
}
