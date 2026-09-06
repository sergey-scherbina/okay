package okay.intent

import okay.frame.{Frame, Found}

/** specs/intent-classify.md, intent-extract-duration */
class TestDuration extends munit.FunSuite {

  test("the shapes a meeting is asked for in, to the minute") {
    val cases = List(
      "30 minutes" -> 30, "45 min" -> 45, "20 mins" -> 20, "90m" -> 90, "45min" -> 45,
      "2 hours" -> 120, "1 hour" -> 60, "1.5 hours" -> 90, "1,5 h" -> 90, "2h" -> 120, "1h30" -> 90, "3 hrs" -> 180,
      "an hour" -> 60, "half an hour" -> 30, "a quarter of an hour" -> 15, "an hour and a half" -> 90,
      "two hours" -> 120, "twenty minutes" -> 20, "forty-five minutes" -> 45,
      "for 30 minutes" -> 30, "Can we meet next Tuesday for 30 minutes?" -> 30,
      "a 2-hour workshop" -> 120)
    val wrong = cases.filter((s, m) => !Duration.parse(s).contains(m)).map((s, m) => s"'$s' -> ${Duration.parse(s)} (expected $m)")
    assertEquals(wrong, Nil)
  }

  test("what it cannot read is None, not a guess") {
    for s <- List("a while", "all day", "a couple of hours", "soon", "", "5", "hours", "0 minutes", "48 hours") do
      assertEquals(Duration.parse(s), None, s"'$s'")
  }

  test("the evidence is the phrase, not the sentence, and the value is the whole message's") {
    val found = Duration.find("Shall we meet next thursday for about 45 minutes, in room 4?")
    assertEquals(found, Some(Found("45 minutes", 45)))
    assertEquals(Duration.find("Shall we meet?"), None)
    assertEquals(Duration.find("Half an hour on Friday would be ideal.").map(_.text), Some("Half an hour"))
  }

  test("a frame with when and duration fills both from one sentence") {
    val today = Temporal.Date(2026, 9, 4)
    val when = Slots.when(today)   // one descriptor value per exchange: valueOf matches by identity
    val f = Frame.of("Proposal", when, Slots.duration).fillFrom("Can we meet next Tuesday for 30 minutes?")
    assertEquals(f.valueOf(Slots.duration), Some(30))
    assertEquals(f.valueOf(when).map(_.date.iso), Some("2026-09-08"))
    assertEquals(f.remaining, 0)
    assertEquals(Slots.duration.show(90, "en"), "1h30")
    assertEquals(Slots.duration.show(120, "en"), "2h")
    assertEquals(Slots.duration.show(45, "en"), "45min")
  }
}
