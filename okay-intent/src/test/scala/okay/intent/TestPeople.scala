package okay.intent

import okay.frame.{Frame, Found}

/** specs/intent-classify.md, intent-extract-people */
class TestPeople extends munit.FunSuite {

  test("the fixture's own law: the book-room row counts four in every language") {
    val row = IntentFixture.parallel.find(_.id == "book-room").get
    val misses = row.byLang.toList.sortBy(_._1).filter((_, text) => !People.parse(text).contains(4))
      .map((lang, text) => s"$lang '$text' -> ${People.parse(text)}")
    assertEquals(misses, Nil)
  }

  test("the shapes, per language") {
    val cases = List(
      "4 people" -> 4, "four people" -> 4, "a room for four people" -> 4, "six of us" -> 6, "a team of 5" -> 5,
      "12 attendees" -> 12, "for 3 guests" -> 3, "twenty participants" -> 20,
      "quatre personnes" -> 4, "12 participants" -> 12, "vier Personen" -> 4, "für 6 Teilnehmer" -> 6,
      "cuatro personas" -> 4, "para 8 asistentes" -> 8, "на четверых" -> 4, "5 человек" -> 5, "трое участников" -> 3,
      "на чотирьох" -> 4, "6 осіб" -> 6, "dla czterech osób" -> 4, "trzy osoby" -> 3, "10 uczestników" -> 10,
      "4人用" -> 4, "6名" -> 6)
    val wrong = cases.filter((s, n) => !People.parse(s).contains(n)).map((s, n) => s"'$s' -> ${People.parse(s)} (expected $n)")
    assertEquals(wrong, Nil)
  }

  test("a number with nothing to count is not a count of people") {
    for s <- List("for 4", "room 4", "at 3pm", "in 30 minutes", "four", "0 people", "5000 people", "") do
      assertEquals(People.parse(s), None, s"'$s'")
  }

  test("the evidence is the phrase, and a frame fills when, duration and people from one sentence") {
    assertEquals(People.find("Could you book a room for four people on Tuesday?"), Some(Found("four people", 4)))
    val today = Temporal.Date(2026, 9, 4)
    val when = Slots.when(today)
    val f = Frame.of("Proposal", when, Slots.duration, Slots.people)
      .fillFrom("Can we meet next Tuesday for an hour, six of us?")
    assertEquals(f.valueOf(when).map(_.date.iso), Some("2026-09-08"))
    assertEquals(f.valueOf(Slots.duration), Some(60))
    assertEquals(f.valueOf(Slots.people), Some(6))
    assertEquals(f.remaining, 0)
  }
}
