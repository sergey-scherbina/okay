package okay.intent

/**
 * What the slot extractors must NOT find (intent-slot-denominators).
 *
 * Measuring the denominators for `duration` and `people` turned up no
 * gap to fix — and two refusals worth nailing down, because both are
 * what a naive improvement would break first:
 *
 *   "Could you dial in five minutes early to test the audio?"
 *       a number beside a people-ish word is NOT a headcount
 *   "Any chance you could take the minutes on Thursday?"
 *       "minutes" is meeting notes here, not a duration
 *
 * Someone will one day widen these extractors, see the coverage rise,
 * and ship exactly these two mistakes. This suite is the thing that
 * stops them, and it costs nothing to keep.
 */
class TestSlotRefusals extends munit.FunSuite {

  test("a number beside a people word is not a headcount") {
    assertEquals(People.find("Could you dial in five minutes early to test the audio?"), None)
    assertEquals(People.find("five minutes"), None)
    // and the shapes that ARE headcounts still are
    assert(People.find("dinner for four people").isDefined)
    assert(People.find("there will be six of us").isDefined)
  }

  test("'minutes' as meeting notes is not a duration") {
    assertEquals(Duration.find("Any chance you could take the minutes on Thursday?"), None)
    assertEquals(Duration.find("Minutes from the last meeting are attached."), None)
    // and a real duration still parses
    assert(Duration.find("a 30-minute call").isDefined)
    assert(Duration.find("let us book 45 minutes").isDefined)
  }

  test("a vague duration is refused rather than guessed") {
    // "quick", "briefly", "ran long" carry no number, and inventing
    // one would put a length in a frame that nobody said
    assertEquals(Duration.find("Would Wednesday at 3 work for a quick chat?"), None)
    assertEquals(Duration.find("The all-hands ran long, the Q and A was cut short."), None)
  }

  test("a word that merely CONTAINS a unit is not a unit") {
    // the misses list was full of these: reminder, chairing, through,
    // longer — a substring is not a token, and the tokeniser is what
    // keeps that true
    for m <- Seq("Reminder that the deadline is this Friday.",
                 "Would you mind chairing the review while I am away?",
                 "Can you confirm the room booking went through?",
                 "I am no longer the owner of this recurring session.") do
      assertEquals(Duration.find(m), None, m)
  }
}
