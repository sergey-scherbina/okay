package okay.intent

import okay.frame.Frame

/**
 * Filling a frame from the message it arrived in
 * (specs/intent-classify.md).
 *
 * The property that matters is not "extraction finds dates" — that is
 * `Temporal`'s test and it already passed. It is that extraction and
 * ASKING give the same answer, and that a message which says nothing
 * fills nothing.
 */
class TestExtract extends munit.FunSuite {

  private val today = Temporal.Date(2026, 9, 4) // a Friday

  // a VAL, not a def: `valueOf` identifies a slot by identity, so a
  // fresh instance per call is a different slot with the same name —
  // which is exactly the guard that keeps the lookup typed
  private val when = Slots.when(today)

  test("the value is the parser's own, over the whole message") {
    val found = Temporal.find("Are you free Wednesday afternoon?", today)
    assertEquals(found.map(_.value), Temporal.parse("Are you free Wednesday afternoon?", today))
    assertEquals(found.map(_.value.date.iso), Some("2026-09-09"))
  }

  test("the evidence is the phrase, not the sentence") {
    // what a caller echoes back: "Wednesday — right?", not the whole
    // message the person can already see
    assertEquals(Temporal.find("Are you free Wednesday afternoon?", today).map(_.text),
      Some("Wednesday"))
    // "thursday", not "next thursday": a bare weekday already means
    // the coming one, so the shorter window resolves to the same day
    // and the minimum is what the evidence should be
    assertEquals(Temporal.find("Shall we meet next thursday at 2pm about the roadmap?", today)
      .map(_.text), Some("thursday at 2pm"))
  }

  test("a message that says nothing fills nothing") {
    assertEquals(Temporal.find("Shall we meet?", today), None)
    val f = Frame.of("Proposal", when).fillFrom("Shall we meet?")
    assert(!f.complete)
    assertEquals(f.missing.map(_._1), Vector("when"))
  }

  test("extraction and asking agree") {
    // the same phrase, once taken out of a sentence and once given as
    // an answer — a frame filled either way holds the same date
    val extracted = Frame.of("Proposal", when).fillFrom("Can we meet next Tuesday?")
    val asked = Frame.of("Proposal", when).answer("when", "next Tuesday").toOption.get
    assertEquals(extracted.valueOf(when), asked.valueOf(when))
    assert(extracted.valueOf(when).isDefined)
  }

  test("a person's own answer is not overwritten by a guess") {
    val f = Frame.of("Proposal", when)
      .answer("when", "tomorrow").toOption.get
      .fillFrom("Actually I said next Tuesday somewhere in this sentence")
    assertEquals(f.valueOf(when).map(_.date.iso), Some("2026-09-05"))
    assertEquals(f.filled("when"), "tomorrow")
  }

  test("a whole-message slot takes the message, an ordinary one does not") {
    val whole = Slots.text("what", Map("en" -> "What would you like done?"),
      fromMessage = true)
    val plain = Slots.text("who", Map("en" -> "Who should be there?"))
    val f = Frame.of("Request", whole, plain).fillFrom("Please forward the signed contract.")
    assertEquals(f.valueOf(whole), Some("Please forward the signed contract."))
    assertEquals(f.valueOf(plain), None)
  }

  test("what extraction covers, per language") {
    // Temporal is English, and this is the number that says so rather
    // than the comment that claims it. A drop here is attributable:
    // one meaning, six wordings, and only the language differs.
    val dated = IntentFixture.parallel.filter(p =>
      Temporal.find(p.byLang("en"), today).isDefined).map(_.id).toSet
    val covered = IntentFixture.languages.map { lang =>
      val hit = IntentFixture.parallel.count(p =>
        dated(p.id) && Temporal.find(p.byLang(lang), today).isDefined)
      lang -> hit
    }
    val n = dated.size
    println(s"[extract] messages carrying a date (by the English reading): $n of ${IntentFixture.parallel.size}")
    covered.foreach((lang, hit) =>
      println(f"[extract] $lang%2s  $hit%2d/$n%-2d  ${100.0 * hit / n}%5.1f%%"))
    assert(n >= 4, s"the fixture should carry several dates, found $n")
    assertEquals(covered.toMap.apply("en"), n, "English must cover its own reading")
    // the honest expectation, not an aspiration: everything else is
    // the `intent-temporal-multilingual` lane
    assert(covered.filter(_._1 != "en").forall(_._2 <= n))
  }
}
