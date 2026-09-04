package okay.intent

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import okay.intent.Temporal.Date

/** specs/intent-classify.md — the temporal slot */
class TestTemporal extends munit.ScalaCheckSuite {

  // 2026-09-04 is a Friday; every relative case below is anchored to it
  private val friday = Date(2026, 9, 4)

  // ---------------------------------------------------------------
  // the calendar underneath, checked before anything built on it

  test("day zero and the day-of-week anchor are the known ones") {
    assertEquals(Temporal.toEpochDay(Date(1970, 1, 1)), 0L)
    assertEquals(Temporal.fromEpochDay(0), Date(1970, 1, 1))
    assertEquals(Temporal.dayOfWeek(Date(1970, 1, 1)), 3) // a Thursday
    assertEquals(Temporal.dayOfWeek(friday), 4)
  }

  test("the dates a hand-rolled calendar gets wrong") {
    // leap day in a leap year, and the century rule in both directions
    assertEquals(Temporal.plusDays(Date(2024, 2, 28), 1), Date(2024, 2, 29))
    assertEquals(Temporal.plusDays(Date(2023, 2, 28), 1), Date(2023, 3, 1))
    assertEquals(Temporal.plusDays(Date(1900, 2, 28), 1), Date(1900, 3, 1)) // not a leap year
    assertEquals(Temporal.plusDays(Date(2000, 2, 28), 1), Date(2000, 2, 29)) // but 2000 is
    assertEquals(Temporal.plusDays(Date(2026, 12, 31), 1), Date(2027, 1, 1))
  }

  property("epoch day round-trips for any date in range") {
    forAll(Gen.choose(-100000L, 100000L)) { (d: Long) =>
      Temporal.toEpochDay(Temporal.fromEpochDay(d)) == d
    }
  }

  property("day of week advances by one per day, and wraps") {
    forAll(Gen.choose(-10000L, 10000L)) { (d: Long) =>
      val a = Temporal.fromEpochDay(d)
      val b = Temporal.plusDays(a, 1)
      Temporal.dayOfWeek(b) == (Temporal.dayOfWeek(a) + 1) % 7
    }
  }

  // ---------------------------------------------------------------
  // the phrases

  private def on(p: String) = Temporal.parse(p, friday).map(_.iso)

  test("today, tomorrow, the day after, yesterday") {
    assertEquals(on("today"), Some("2026-09-04"))
    assertEquals(on("tomorrow"), Some("2026-09-05"))
    assertEquals(on("the day after tomorrow"), Some("2026-09-06"))
    assertEquals(on("yesterday"), Some("2026-09-03"))
  }

  test("a weekday means the coming one, and today's weekday means next week") {
    assertEquals(on("monday"), Some("2026-09-07"))
    assertEquals(on("next thursday"), Some("2026-09-10"))
    assertEquals(on("this tuesday"), Some("2026-09-08"))
    // Friday, said on a Friday, is not today — a meeting proposed for
    // "friday" on Friday morning is the next one
    assertEquals(on("friday"), Some("2026-09-11"))
  }

  test("last goes backwards") {
    assertEquals(on("last thursday"), Some("2026-09-03"))
    assertEquals(on("last friday"), Some("2026-08-28"))
  }

  test("counted days, forwards and back") {
    assertEquals(on("in 3 days"), Some("2026-09-07"))
    assertEquals(on("10 days from now"), Some("2026-09-14"))
    assertEquals(on("2 days ago"), Some("2026-09-02"))
    assertEquals(on("next week"), Some("2026-09-11"))
  }

  test("an explicit date wins, and a month-and-day takes the coming year") {
    assertEquals(on("2026-12-01"), Some("2026-12-01"))
    assertEquals(on("March 14"), Some("2027-03-14"))    // March has passed
    assertEquals(on("14 October"), Some("2026-10-14"))  // October has not
    assertEquals(on("meet on 2027-01-05 please"), Some("2027-01-05"))
  }

  test("a time rides along, in both spellings") {
    assertEquals(on("tomorrow at 2pm"), Some("2026-09-05T14:00"))
    assertEquals(on("tomorrow at 14:30"), Some("2026-09-05T14:30"))
    assertEquals(on("next thursday at 9am"), Some("2026-09-10T09:00"))
    assertEquals(on("monday at 12pm"), Some("2026-09-07T12:00"))
    assertEquals(on("monday at 12am"), Some("2026-09-07T00:00"))
  }

  test("what it refuses, on purpose") {
    // each of these is guessable and each guess would be acted on
    for p <- Seq("soon", "end of the month", "the 14th", "later this week",
                 "in a couple of days", "next month", "", "sometime")
    do assertEquals(Temporal.parse(p, friday), None, s"should not have guessed at '$p'")
  }

  property("a refusal is total: no input throws") {
    // `forAll(...).check()` inside a `test` block prints and returns —
    // it cannot fail the suite, which makes it scenery. A `property`
    // is the shape that actually reports.
    forAll(Gen.asciiPrintableStr) { (s: String) =>
      Temporal.parse(s, friday): Unit
      true
    }
  }

  // ---------------------------------------------------------------
  // and the point of it: the output satisfies the slot that refuses prose

  test("what it produces passes the schema that refuses 'next thursday'") {
    val shape = raw"\d{4}-\d{2}-\d{2}(T\d{2}:\d{2})?".r
    for p <- Seq("next thursday", "tomorrow at 2pm", "in 3 days", "14 October")
    do assert(Temporal.parse(p, friday).map(_.iso).exists(shape.matches),
      s"'$p' did not produce a value the slot accepts")
    assert(shape.matches("2026-09-10"))
    assert(!shape.matches("next thursday"))
  }
}
