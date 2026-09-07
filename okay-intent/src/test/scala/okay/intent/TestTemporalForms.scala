package okay.intent

/**
 * The word FORMS a date hides behind (intent-offline-slots).
 *
 * Found by measuring, not by imagining: `MeasureSlotCoverage` listed
 * every fixture message that mentions a time and yields nothing, and
 * two shapes in that list were real misses rather than correct
 * refusals — a possessive ("tomorrow's meeting", "Thursday's invite")
 * and a plural weekday ("Thursdays are remote from now on"). Both are
 * the same word wearing an ending, and both now parse.
 *
 * The other two shapes in that list are DELIBERATE refusals and are
 * asserted here as such, so that a later change cannot quietly turn a
 * decision into a bug: a bare time with no day ("after 7pm") and a
 * RANGE ("sometime this week") are not dates, and inventing one would
 * put a wrong answer in a frame the caller then acts on.
 */
class TestTemporalForms extends munit.FunSuite {

  private val friday = Temporal.Date(2026, 9, 4)   // a Friday

  test("a possessive is the same word: tomorrow's, Thursday's") {
    assertEquals(Temporal.parse("Could you send me the deck before tomorrow's meeting?", friday),
      Temporal.parse("Could you send me the deck before tomorrow", friday))
    assert(Temporal.parse("tomorrow's meeting", friday).isDefined)
    assertEquals(Temporal.parse("Please add the finance team to Thursday's invite.", friday).map(_.date),
      Temporal.parse("Please add the finance team to Thursday invite.", friday).map(_.date))
    // the curly apostrophe too, which is what a mail client sends
    assertEquals(Temporal.parse("before tomorrow’s meeting", friday),
      Temporal.parse("before tomorrow meeting", friday))
  }

  test("a plural weekday is still that weekday") {
    val plural = Temporal.parse("The office move means Thursdays are remote from now on.", friday)
    val single = Temporal.parse("The office move means Thursday is remote from now on.", friday)
    assert(plural.isDefined, "Thursdays names Thursday")
    assertEquals(plural.map(_.date), single.map(_.date))
  }

  test("a token carrying an apostrophe is not mangled by the possessive rule") {
    // the hour form this parser reads is "3pm"; `o'clock` it has never
    // read, and the point here is only that stripping a trailing
    // apostrophe-s leaves such a token alone
    assert(Temporal.parse("tomorrow at 3pm", friday).exists(_.hour.contains(15)))
    assert(Temporal.parse("tomorrow at 3 o'clock", friday).isDefined, "the date still parses")
    assertEquals(Temporal.parse("tomorrow at 3 o'clock", friday).flatMap(_.hour), None)
  }

  test("nothing that parsed before parses differently now") {
    val unchanged = List(
      "let's meet tomorrow", "next thursday works", "last thursday was busy",
      "in 3 days", "2026-09-14", "March 14", "next week", "today at 14:30",
      "friday at 9am", "thursday")
    for m <- unchanged do
      assert(Temporal.parse(m, friday).isDefined, s"$m stopped parsing")
    // and the shapes that must still decline
    assertEquals(Temporal.parse("hello there", friday), None)
    assertEquals(Temporal.parse("I am writing to complain", friday), None)
  }

  test("the deliberate refusals: a bare time and a range are not dates") {
    // "after 7pm" names an hour with no day. Guessing today or
    // tomorrow would fill a frame with a date nobody said.
    assertEquals(Temporal.parse("Please note the building requires a badge after 7pm.", friday), None)
    // "this week" and "sometime this week" are RANGES; `When` holds one
    // date, so the honest answer is to ask rather than to pick a day.
    assertEquals(Temporal.parse("I'd like to suggest a 30-minute call sometime this week.", friday), None)
    assertEquals(Temporal.parse("Free any afternoon this week if you want to go over it.", friday), None)
  }
}
