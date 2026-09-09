package okay.intent

import okay.intent.Temporal.{Date, Period}

/** specs/intent-classify.md, intent-periods-and-zl — a period beside the day */
class TestPeriod extends munit.FunSuite {

  // 2026-09-04 is a Friday; every case below is anchored to it
  private val friday = Date(2026, 9, 4)
  private def on(p: String) = Temporal.period(p, friday).map(_.iso)

  test("this week is Monday to Sunday, and next and last are the weeks beside it") {
    assertEquals(on("this week"), Some("2026-08-31/2026-09-06"))
    assertEquals(on("next week"), Some("2026-09-07/2026-09-13"))
    assertEquals(on("last week"), Some("2026-08-24/2026-08-30"))
    // said on a Friday, the weekend is the one two days away
    assertEquals(on("this weekend"), Some("2026-09-05/2026-09-06"))
    assertEquals(on("weekend"), Some("2026-09-05/2026-09-06"))
    assertEquals(on("next weekend"), Some("2026-09-12/2026-09-13"))
  }

  test("a month named without a day is the coming one, whole") {
    assertEquals(on("in September"), Some("2026-09-01/2026-09-30"))
    assertEquals(on("in October"), Some("2026-10-01/2026-10-31"))
    // August is over: the coming August is next year's
    assertEquals(on("in August"), Some("2027-08-01/2027-08-31"))
    // a day beside the month makes it a date, and a date is `parse`'s
    assertEquals(on("14 September"), None)
    assertEquals(on("September 14"), None)
    // ANY number beside it: an impossible day the day parser refuses on
    // purpose, and a range in one token, are not the bare month either
    assertEquals(on("с 12 по 40 сентября"), None)
    assertEquals(on("12-14 сентября"), None)
    assertEquals(on("od 12 do 14 września"), None)
    assertEquals(on("September 40"), None)
  }

  test("the six languages, and the weekend read before the week") {
    // ru
    assertEquals(on("на этой неделе"), Some("2026-08-31/2026-09-06"))
    assertEquals(on("на следующей неделе"), Some("2026-09-07/2026-09-13"))
    assertEquals(on("на прошлой неделе"), Some("2026-08-24/2026-08-30"))
    assertEquals(on("на выходных"), Some("2026-09-05/2026-09-06"))
    assertEquals(on("в сентябре"), Some("2026-09-01/2026-09-30"))
    // uk
    assertEquals(on("цього тижня"), Some("2026-08-31/2026-09-06"))
    assertEquals(on("наступного тижня"), Some("2026-09-07/2026-09-13"))
    assertEquals(on("на вихідних"), Some("2026-09-05/2026-09-06"))
    assertEquals(on("у вересні"), Some("2026-09-01/2026-09-30"))
    // pl
    assertEquals(on("w tym tygodniu"), Some("2026-08-31/2026-09-06"))
    assertEquals(on("w przyszłym tygodniu"), Some("2026-09-07/2026-09-13"))
    assertEquals(on("w weekend"), Some("2026-09-05/2026-09-06"))
    assertEquals(on("we wrześniu"), Some("2026-09-01/2026-09-30"))
    // fr / de / es — `week-end`, `Wochenende` and `fin de semana`
    assertEquals(on("cette semaine"), Some("2026-08-31/2026-09-06"))
    assertEquals(on("ce week-end"), Some("2026-09-05/2026-09-06"))
    assertEquals(on("diese Woche"), Some("2026-08-31/2026-09-06"))
    assertEquals(on("am Wochenende"), Some("2026-09-05/2026-09-06"))
    assertEquals(on("nächste Woche"), Some("2026-09-07/2026-09-13"))
    assertEquals(on("esta semana"), Some("2026-08-31/2026-09-06"))
    assertEquals(on("el fin de semana"), Some("2026-09-05/2026-09-06"))
  }

  test("a month alone is the month's own word, never a prefix — «майстер» is not May") {
    // found on a consumer's live log the evening the parser landed:
    // «майстер» starts with «май», «лютни» and «лютьер» with «лют»
    for p <- Seq("майстер", "лютни", "лютьер", "чиню лютни и мандолины за 120 зл", "lutnia", "marzenie",
                 "серп", "вереск", "жовтий", "квітка", "береза", "липа", "gruda", "майка", "мартышка") do
      assertEquals(on(p), None, p)
    // the month's own forms, in the case a sentence puts them in
    assertEquals(on("в лютому"), Some("2027-02-01/2027-02-28"))
    assertEquals(on("у травні"), Some("2027-05-01/2027-05-31"))
    assertEquals(on("w lutym"), Some("2027-02-01/2027-02-28"))
    assertEquals(on("w maju"), Some("2027-05-01/2027-05-31"))
    assertEquals(on("в мае"), Some("2027-05-01/2027-05-31"))
    assertEquals(on("октябрь"), Some("2026-10-01/2026-10-31"))
    // English «may» is a verb until a preposition makes it a month
    assertEquals(on("in May"), Some("2027-05-01/2027-05-31"))
    assertEquals(on("may I ask"), None)
    assertEquals(on("you may"), None)
    assertEquals(on("en janvier"), Some("2027-01-01/2027-01-31"))
    assertEquals(on("im Dezember"), Some("2026-12-01/2026-12-31"))
  }

  test("a day is not a period, and neither is a guess") {
    for p <- Seq("thursday", "next thursday", "tomorrow", "2026-09-14", "в пятницу", "soon", "later", "", "   ") do
      assertEquals(on(p), None, p)
    // and `parse` is untouched: «next week» is still a day there
    assertEquals(Temporal.parse("next week", friday).map(_.iso), Some("2026-09-11"))
  }

  test("the evidence is the shortest window that names the period") {
    // the SHORTEST window, as `find` gives «пятницу» for «в пятницу»:
    // minimal evidence for a value already decided over the whole
    // message. A consumer that wants the phrase widens over the
    // function words beside it; the parser does not guess at them
    val found = Temporal.findPeriod("что интересного на этой неделе во Вроцлаве", friday)
    assertEquals(found.map(_.text), Some("неделе"))
    assertEquals(found.map(_.value.iso), Some("2026-08-31/2026-09-06"))
    assertEquals(Temporal.findPeriod("co ciekawego w weekend w Gdańsku", friday).map(_.text), Some("weekend"))
    assertEquals(Temporal.findPeriod("нужен сантехник", friday), None)
    assert(Period(friday, friday).contains(friday))
    assert(!Period(Date(2026, 9, 5), Date(2026, 9, 6)).contains(friday))
  }
}
