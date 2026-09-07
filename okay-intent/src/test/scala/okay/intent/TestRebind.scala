package okay.intent

import okay.frame.{Frame, Rebound}

/**
 * The hazard `frame-rebind` was filed with, shown: "next Tuesday" is a
 * value relative to a day, and a restarted service that rebuilds its
 * `when` slot against TODAY re-derives a different date from the same
 * words. `Frame.rebind` re-reads and REPORTS; it does not decide.
 */
class TestRebind extends munit.FunSuite {

  test("a temporal answer rebound against a later reference day moves, and the move is reported") {
    val friday = Temporal.Date(2026, 9, 4)
    val when = Slots.when(friday)
    val f = Frame.of("Proposal", when).answer("when", "next Tuesday").toOption.get
    assertEquals(f.valueOf(when).map(_.date.iso), Some("2026-09-08"))

    // the process dies; the service comes back a week later
    val laterWhen = Slots.when(Temporal.Date(2026, 9, 11))
    val r = f.rebind(laterWhen)
    assertEquals(r.frame.valueOf(laterWhen).map(_.date.iso), Some("2026-09-15"))
    r.rederived match
      case Vector(Rebound.Change("when", "next Tuesday", before: Temporal.When, after: Temporal.When)) =>
        assertEquals(before.date.iso, "2026-09-08")
        assertEquals(after.date.iso, "2026-09-15")
      case other => fail(s"expected the date's move to be reported, got $other")
    assert(!r.clean)

    // rebuilt against the SAME day, nothing moves and the rebind is clean
    assert(f.rebind(Slots.when(friday)).clean)
  }
}
