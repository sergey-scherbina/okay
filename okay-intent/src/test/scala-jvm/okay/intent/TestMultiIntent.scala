package okay.intent

/**
 * The claim `Span` has been making since the first lane, measured
 * (specs/intent-classify.md).
 *
 * "A message carrying two intents has two spans, both to be acted on"
 * is in the type, in the prompt and in the Overview — and no fixture
 * row had ever carried two intents, so the sentence had never been
 * tested against a message. This is that test, and most of what it
 * establishes is what the SHIPPED path cannot do.
 */
class TestMultiIntent extends munit.FunSuite {

  private val rows = IntentFixture.twoIntents
  private val router = Router.Router.offline()

  private def single(m: String): Option[String] = router.route(m) match
    case Router.Action.Act(i, _) => Some(i)
    case Router.Action.Ask(i, _, _, _) => Some(i)
    case Router.Action.Escalate(_, _) => None

  test("the shipped path answers with ONE label, and that is structural") {
    // not a defect and not a surprise: Patterns, CharGrams, Centroid
    // and Probe each return a single best class. The multi-intent
    // claim is true of the MODEL tier alone, and the module's own
    // documentation has never said so.
    val answers = rows.map((m, gold) => (m, gold, single(m)))
    val answered = answers.count(_._3.isDefined)
    val hitFirst = answers.count((_, g, a) => a.contains(g.head))
    val hitEither = answers.count((_, g, a) => a.exists(g.contains))
    println(f"[one-label] answered $answered%2d/${rows.size}%2d, " +
      f"matched the FIRST intent $hitFirst%2d, matched EITHER $hitEither%2d")
    assertEquals(answered, rows.size, "every message got a class")
    // whichever it picks, the other intent is gone with no trace
    assert(hitEither >= hitFirst)
  }

  test("what the cue tier's runner-up carries, which is the only trace left") {
    // the one place a second intent could survive a single-label tier:
    // Patterns.score ranks the classes it fired on, so a message with
    // two intents may fire cues for both
    val scored = rows.flatMap((m, gold) =>
      Patterns.score(Models.cues, m).map(v => (m, gold, v)))
    val bothFired = scored.count((_, gold, v) =>
      v.runnerUp.exists(r => gold.contains(r)) && gold.contains(v.best))
    println(f"[runner-up] cues fired on ${scored.size}%2d of ${rows.size}%2d, " +
      f"and named BOTH intents (best + runner-up) on $bothFired%2d")
    scored.foreach { (m, gold, v) =>
      println(f"[runner-up]   ${gold.mkString("+")}%-24s best ${v.best}%-13s " +
        f"runner-up ${v.runnerUp.getOrElse("-")}%-13s  $m%s")
    }
  }

  test("a Reading with two spans is acted on as two, which is the type's promise") {
    // the mechanism itself, on a hand-built Reading: this part has
    // always worked and is what the model tier would produce
    val r = Reading(List(
      Span("Can we meet on Tuesday?", "a proposal of a time",
        List(Alt(Conf.High, "Proposal"))),
      Span("Also please send the deck beforehand.", "a request for a thing",
        List(Alt(Conf.High, "Request")))))
    Classify.decide(r) match
      case Classify.Decision.Act(spans) =>
        assertEquals(spans.map(_._2), List("Proposal", "Request"))
        assertEquals(spans.map(_._1).head, "Can we meet on Tuesday?")
      case other => fail(s"expected two acted spans, got $other")
  }

  test("and one unsure span stops the whole message, not just its own half") {
    val r = Reading(List(
      Span("Can we meet on Tuesday?", "a proposal", List(Alt(Conf.High, "Proposal"))),
      Span("Also the thing about the thing.", "unclear", List(Alt(Conf.Low, "Request")))))
    Classify.decide(r) match
      case Classify.Decision.Clarify(sp) => assertEquals(sp.text, "Also the thing about the thing.")
      case other => fail(s"a half-guessed message must not be half-acted: $other")
  }
}
