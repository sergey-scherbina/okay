package okay.chat.leads

import okay.Csv
import okay.given
import okay.intent.Amount
import okay.chat.leads.Lead.{Category, Outcome, Urgency}
import java.nio.file.Files
import java.time.{Instant, ZoneOffset}

/** The ledger, end to end: capture, append, read back, add up. */
class TestLeads extends munit.FunSuite {

  val today: Instant = Instant.parse("2026-09-11T09:00:00Z")

  // --------------------------------------------------------- capture

  test("a category is read from a cue, in any of the three languages") {
    assertEquals(Capture.category("szukam mieszkania na Krzykach"), Category.Housing)
    assertEquals(Capture.category("ищу работу на полставки"), Category.Job)
    assertEquals(Capture.category("need a plumber tomorrow"), Category.Service)
    assertEquals(Capture.category("kupię rower dla dziecka"), Category.Goods)
    assertEquals(Capture.category("куда сходить в выходные"), Category.Leisure)
  }

  test("what no cue fires on stays Unknown, and no city is guessed") {
    assertEquals(Capture.category("dzień dobry"), Category.Unknown)
    assertEquals(Capture.city("dzień dobry"), None)
    assertEquals(Capture.budget("dzień dobry"), None)
  }

  test("a city is recognised only when it is named") {
    assertEquals(Capture.city("mieszkanie we Wrocławiu"), Some("Wrocław"))
    assertEquals(Capture.city("снять квартиру в Кракове"), Some("Kraków"))
    assertEquals(Capture.city("a flat in this city"), None)
  }

  test("a budget is the amount parser's, and urgency reads words then dates") {
    assertEquals(Capture.budget("do 3000 zł miesięcznie").map(_.value), Some(BigDecimal(3000)))
    assertEquals(Capture.urgency("pilne, na dziś", today), Urgency.Now)
    assertEquals(Capture.urgency("tomorrow if possible", today), Urgency.Week)
    assertEquals(Capture.urgency("нужно в этом месяце", today), Urgency.Month)
    assertEquals(Capture.urgency("po prostu pytam", today), Urgency.Unknown)
  }

  test("a later tier may fill a hole, never overwrite what was written") {
    val l = Capture.lead("szukam mieszkania, 2500 zł", "s1", "salt", today)
    val refined = Capture.refine(l, category = Some(Category.Job),
      city = Some("Wrocław"), budget = Some(Amount(BigDecimal(9999), "PLN")))
    assertEquals(refined.category, Category.Housing, "the cue had already decided")
    assertEquals(refined.budget.map(_.value), Some(BigDecimal(2500)), "the person had written a number")
    assertEquals(refined.city, Some("Wrocław"), "this one was a hole")
  }

  test("the session is a salted hash: stable, and not a person") {
    val a = Lead.pseudonym("conversation-42", "salt")
    assertEquals(a, Lead.pseudonym("conversation-42", "salt"))
    assertNotEquals(a, Lead.pseudonym("conversation-42", "другая соль"))
    assertNotEquals(a, Lead.pseudonym("conversation-43", "salt"))
    assert(!a.contains("conversation"), a)
  }

  // ---------------------------------------------------------- ledger

  def sample: Vector[Lead] = Vector(
    Lead(today, "a", Category.Housing, Some("Wrocław"), Some(Amount(BigDecimal(3000), "PLN")), Urgency.Week, Outcome.Open, Some("tg:@ann")),
    Lead(today.plusSeconds(60), "b", Category.Housing, Some("Wrocław"), Some(Amount(BigDecimal(2200), "PLN")), Urgency.Now, Outcome.Matched, Some("mail:b@x.pl")),
    Lead(today.plusSeconds(120), "c", Category.Job, Some("Wrocław"), None, Urgency.Month, Outcome.Open, None),
    Lead(today.plusSeconds(180), "a", Category.Service, Some("Kraków"), Some(Amount(BigDecimal(400), "PLN")), Urgency.Now, Outcome.Dropped, None),
    Lead(today.plusSeconds(240), "d", Category.Housing, None, Some(Amount(BigDecimal(4000), "PLN")), Urgency.Later, Outcome.Contacted, Some("tg:@d")))

  test("a lead survives the CSV round trip, commas and all") {
    val f = Files.createTempDirectory("ledger").resolve("leads.csv")
    val ledger = Ledger(f)
    ledger.appendAll(sample)
    val odd = sample.head.copy(session = "x", contact = Some("""ul. Długa 1, "Wrocław""""))
    ledger.append(odd)
    val (read, bad) = ledger.readEither()
    assertEquals(bad, Vector.empty)
    assertEquals(read, sample :+ odd)
    assertEquals(ledger.size[okay.Chunks], 6L)
    Files.deleteIfExists(f)
  }

  test("a row that cannot be read is reported, not dropped") {
    val f = Files.createTempDirectory("ledger").resolve("leads.csv")
    Files.writeString(f, Lead.header + "\n" + Csv.line(Vector("not-a-time", "a", "Housing", "", "", "", "Now", "Open", "")) + "\n")
    val (read, bad) = Ledger(f).readEither()
    assertEquals(read, Vector.empty)
    assertEquals(bad.size, 1)
    assert(bad.head.contains("not an instant"), bad.head)
    Files.deleteIfExists(f)
  }

  // ---------------------------------------------------------- demand

  test("the report: one pass, and the numbers a buyer asks for") {
    val r = Demand.report(sample)
    assertEquals(r.total, 5L)
    assertEquals(r.people, 4L, "four sessions, five requests")
    assertEquals(r.byCategory(Category.Housing), 3L)
    assertEquals(r.byCity("Wrocław"), 3L)
    assertEquals(r.byCity("—"), 1L, "an unknown city is counted as unknown, not dropped")
    assertEquals(r.byCategoryCity((Category.Housing, "Wrocław")), 2L)
    assertEquals(r.deliverable, 2L, "with a contact, and still live")
    assertEqualsDouble(r.conversion, 40.0, 0.001)
    assertEquals(r.unmet(2).head, ((Category.Housing, "Wrocław"), 2L))
  }

  test("a correction wins: the ledger is append-only and the last row is the truth") {
    val fixed = sample :+ sample.head.copy(outcome = Outcome.Contacted)
    assertEquals(Demand.report(fixed).total, 5L, "still five requests")
    assertEquals(Demand.report(fixed).byOutcome.getOrElse(Outcome.Open, 0L), 1L)
    assertEquals(Demand.report(fixed).byOutcome(Outcome.Contacted), 2L)
  }

  test("median budgets, per category, from a digest rather than a list") {
    val m = Demand.medianBudgets(sample)
    assertEquals(m(Category.Housing)._1, 3L)
    assertEqualsDouble(m(Category.Housing)._2, 3000.0, 1000.0)
    assert(!m.contains(Category.Job), "nobody in Job wrote a budget")
  }

  test("rolling demand is the group's window: a day in, the day that aged out subtracted") {
    // 1, 2, 3, 1, 2, 3, ... requests a day, each from its own session
    // (two rows of one session at one instant are one correction, not
    // two requests — which is the rule `latest` exists for)
    val days = (0 until 10).flatMap(i => (0 to i % 3).map(j =>
      sample.head.copy(session = s"s$i-$j", at = today.plusSeconds(86400L * i))))
    val rolling = Demand.report(days).rolling(3)
    assertEquals(rolling.size, 8, "ten days, a window of three")
    assertEqualsDouble(rolling.head._2, 6.0, 0.001, "days 0..2: 1 + 2 + 3")
    assertEquals(rolling.head._1, today.atZone(ZoneOffset.UTC).toLocalDate.plusDays(2))
    assertEqualsDouble(rolling.last._2, 6.0, 0.001, "days 7..9: 2 + 3 + 1")
    assertEqualsDouble(rolling(3)._2, 6.0, 0.001, "days 3..5: 1 + 2 + 3")
  }

  test("only what may be passed on is passed on") {
    val d = Demand.deliverable(sample)
    assertEquals(d.map(_.session).toSet, Set("a", "b"))
    assert(d.forall(_.contact.isDefined))
  }
}
