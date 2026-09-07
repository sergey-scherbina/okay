package okay.intent

import okay.frame.Frame

/**
 * How much of a frame the OFFLINE path already fills
 * (intent-offline-slots; specs/intent-autonomy.md stage 7).
 *
 * The autonomy report measures the CLASS: what share of messages the
 * network-free tiers classify, and how well. It says nothing about
 * the slots, and a door that names the intent without a network and
 * then must ask a model for every slot is not autonomous — it has
 * moved the call, not removed it.
 *
 * So, before building the sequence labeller the plan names: what do
 * the extractors we already ship (`Temporal`, `Duration`, `People`,
 * `Amount`, and the plain-text slots that read themselves) actually
 * fill? Per slot, per language, and as a whole-frame number — the
 * one a caller feels, because a frame with one slot missing still
 * costs a question.
 *
 * Offline, no gateway. The assertions are structural; the numbers are
 * printed and recorded in the spec.
 */
class MeasureSlotCoverage extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val today = Temporal.Date(2026, 9, 7)

  // vals, not defs: a slot is identified by identity, so a fresh
  // instance per call is a different slot with the same name
  private val whenSlot = Slots.when(today)
  private def meetingFrame: Frame[String] =
    Frame("Proposal", Vector(whenSlot, Slots.duration, Slots.people))

  private def rowsOf(lang: String): List[(String, String)] =
    IntentFixture.inLanguage(lang)

  private def line(s: String): Unit = println(s)

  test("per slot, over the English fixture: what the extractors find without a network") {
    val rows = IntentFixture.labelled
    val slots: Vector[(String, okay.frame.Slot[?])] = Vector(("when", whenSlot), ("duration", Slots.duration), ("people", Slots.people))
    line("\n### offline slot extraction, English fixture (120 messages)")
    line("| slot | messages it fired on | share |")
    line("|---|---:|---:|")
    for (name, slot) <- slots do
      val hit = rows.count((m, _) => slot.extract(m).isDefined)
      line(f"| $name | $hit | ${100.0 * hit / rows.size}%.1f%% |")
    // the whole-frame number: a frame is only free if EVERY slot filled
    val full = rows.count { (m, _) =>
      val f = meetingFrame.fillFrom(m)
      f.missing.isEmpty
    }
    val partial = rows.count { (m, _) =>
      val f = meetingFrame.fillFrom(m)
      f.missing.nonEmpty && f.missing.length < 3
    }
    line(f"\n| whole frame (when + duration + people) | $full | ${100.0 * full / rows.size}%.1f%% |")
    line(f"| partly filled (1-2 of 3) | $partial | ${100.0 * partial / rows.size}%.1f%% |")
    line(f"| nothing at all | ${rows.size - full - partial} | ${100.0 * (rows.size - full - partial) / rows.size}%.1f%% |")
    assert(rows.nonEmpty)
  }

  test("per language: the same slots against the parallel set") {
    line("\n### offline slot extraction, per language (the parallel set)")
    line("| language | when | duration | people | any slot |")
    line("|---|---:|---:|---:|---:|")
    for lang <- IntentFixture.languages do
      val rows = rowsOf(lang)
      if rows.nonEmpty then
        val w = rows.count((m, _) => whenSlot.extract(m).isDefined)
        val d = rows.count((m, _) => Slots.duration.extract(m).isDefined)
        val p = rows.count((m, _) => Slots.people.extract(m).isDefined)
        val any = rows.count { (m, _) =>
          whenSlot.extract(m).isDefined || Slots.duration.extract(m).isDefined ||
            Slots.people.extract(m).isDefined }
        line(f"| $lang | ${100.0 * w / rows.size}%.0f%% | ${100.0 * d / rows.size}%.0f%% | " +
             f"${100.0 * p / rows.size}%.0f%% | ${100.0 * any / rows.size}%.0f%% |")
    assert(IntentFixture.languages.nonEmpty)
  }

  test("the honest denominator: how many messages CARRY each slot at all") {
    // a slot the message does not mention is not a miss — a frame that
    // asks for it is doing the right thing. So the coverage number
    // above is only readable beside this one: of the messages a human
    // would say carry a time, how many does the extractor find?
    val rows = IntentFixture.labelled
    val carriesTime = rows.count((m, _) =>
      Seq("today", "tomorrow", "monday", "tuesday", "wednesday", "thursday", "friday",
        "am", "pm", "o'clock", "next week", "this week").exists(w => m.toLowerCase.contains(w)))
    val found = rows.count((m, _) => whenSlot.extract(m).isDefined)
    line(f"\n### the denominator")
    line(f"| messages whose words suggest a time | $carriesTime |")
    line(f"| messages where the extractor found one | $found |")
    line(f"| ratio | ${if carriesTime == 0 then 0.0 else 100.0 * found / carriesTime}%.0f%% |")
    assert(carriesTime > 0, "the fixture should carry times")
  }

  /**
   * The same question for `duration` and `people`
   * (intent-slot-denominators): they fire on 7 and 1 messages of 120,
   * and until somebody counts how many messages CARRY them those two
   * numbers say nothing at all. The hint lists are deliberately WIDE
   * — a false alarm here costs a line of reading, a missed one costs
   * a wrong conclusion.
   */
  test("the denominators for duration and people, and every miss in full") {
    val durationHints = Seq("minute", "minutes", "min", "hour", "hours", "hr", "half an hour",
      "quarter of an hour", "30-minute", "45-minute", "hour-long", "all day", "all morning",
      "briefly", "quick", "long")
    val peopleHints = Seq("people", "person", "of us", "attendees", "participants", "guests",
      "everyone", "the team", "we are", "there are", "four", "five", "six", "seven", "eight",
      "two of", "three of", "headcount", "seats")
    for (name, hints, extract) <- Seq(
      ("duration", durationHints, (m: String) => Slots.duration.extract(m).isDefined),
      ("people", peopleHints, (m: String) => Slots.people.extract(m).isDefined)) do
      val rows = IntentFixture.labelled
      val suggest = rows.filter((m, _) => hints.exists(h => m.toLowerCase.contains(h)))
      val found = rows.count((m, _) => extract(m))
      val missed = suggest.filterNot((m, _) => extract(m))
      println(f"\n### $name: the denominator")
      println(f"| messages whose words suggest $name | ${suggest.length} |")
      println(f"| messages where the extractor found one | $found |")
      println(f"| of the suggested, found | ${suggest.length - missed.length} (${
        if suggest.isEmpty then 0.0 else 100.0 * (suggest.length - missed.length) / suggest.length}%.0f%%) |")
      println(s"### $name: every message that suggests one and yields nothing (${missed.length})")
      missed.foreach((m, _) => println(s"  - ${m.take(110)}"))
    assert(IntentFixture.labelled.nonEmpty)
  }

  /**
   * WHAT it misses, message by message. A recall of 66% is a number;
   * this is the thing to act on. If the misses are a handful of
   * unhandled wordings, extending `Temporal` is cheaper than any
   * learned labeller and needs no data at all — which is exactly the
   * decision this lane exists to make.
   */
  test("the misses, in full, so the next step is chosen on evidence") {
    val hints = Seq("today", "tomorrow", "monday", "tuesday", "wednesday", "thursday", "friday",
      "saturday", "sunday", "am", "pm", "o'clock", "next week", "this week", "morning",
      "afternoon", "evening", "noon", "midday", "tonight", "weekend")
    val missed = IntentFixture.labelled.filter { (m, _) =>
      val low = m.toLowerCase
      hints.exists(low.contains) && whenSlot.extract(m).isEmpty
    }
    println(s"\n### messages that mention a time and yield nothing (${missed.length})")
    missed.foreach((m, _) => println(s"  - ${m.take(110)}"))
    val found = IntentFixture.labelled.count((m, _) => whenSlot.extract(m).isDefined)
    println(s"\n(found on $found messages; the fixture has ${IntentFixture.labelled.size})")
    assert(missed.length >= 0)
  }
}
