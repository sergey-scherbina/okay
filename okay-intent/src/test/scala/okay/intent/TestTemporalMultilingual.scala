package okay.intent

/**
 * The temporal parser in the other seven languages
 * (specs/intent-classify.md, intent-temporal-multilingual).
 *
 * The law is the parallel fixture's own: one meaning, eight wordings.
 * A message that carries a date in English carries THE SAME date in
 * every other language, so the parser's answer must be the same
 * `When` — date and time — whatever the language. Coverage is then a
 * count, not a claim, and a drop is attributable to one wording.
 */
class TestTemporalMultilingual extends munit.FunSuite {

  private val today = Temporal.Date(2026, 9, 4) // a Friday, as TestExtract
  private def parse(s: String) = Temporal.parse(s, today)
  private def day(s: String) = parse(s).map(_.date.iso)

  test("one meaning, eight wordings, one date: every dated row agrees with its English reading") {
    val dated = IntentFixture.parallel.filter(p => parse(p.byLang("en")).isDefined)
    assert(dated.size >= 4, s"the fixture should carry several dates, found ${dated.size}")
    val misses = for
      p <- dated
      lang <- IntentFixture.languages if lang != "en"
      text <- p.byLang.get(lang)
      en = parse(p.byLang("en"))
      got = parse(text)
      if got != en
    yield (p.id, lang, text, en, got)
    println(s"\n[temporal] dated rows ${dated.size}; languages ${IntentFixture.languages.size}; disagreements ${misses.size}")
    misses.foreach((id, lang, text, en, got) => println(s"  $id/$lang  '$text'  en=$en  got=$got"))
    IntentFixture.languages.foreach { lang =>
      val hit = dated.count(p => p.byLang.get(lang).exists(t => parse(t) == parse(p.byLang("en"))))
      println(f"  $lang%2s  $hit%2d/${dated.size}%-2d")
    }
    assertEquals(misses.map(m => (m._1, m._2)), Nil, "a wording that reads a different date, or none")
  }

  test("the relative and counted forms, per language, against the same Friday") {
    val tomorrow = "2026-09-05"; val dayAfter = "2026-09-06"; val yesterday = "2026-09-03"
    val nextThursday = "2026-09-10"; val lastThursday = "2026-09-03"; val in3 = "2026-09-07"; val ago3 = "2026-09-01"
    val cases = List(
      "demain" -> tomorrow, "après-demain" -> dayAfter, "hier" -> yesterday, "jeudi prochain" -> nextThursday,
      "jeudi dernier" -> lastThursday, "dans 3 jours" -> in3, "il y a 3 jours" -> ago3, "la semaine prochaine" -> "2026-09-11",
      "morgen" -> tomorrow, "übermorgen" -> dayAfter, "gestern" -> yesterday, "nächsten Donnerstag" -> nextThursday,
      "letzten Donnerstag" -> lastThursday, "in 3 Tagen" -> in3, "vor 3 Tagen" -> ago3, "nächste Woche" -> "2026-09-11",
      "mañana" -> tomorrow, "pasado mañana" -> dayAfter, "ayer" -> yesterday, "el próximo jueves" -> nextThursday,
      "el jueves pasado" -> lastThursday, "en 3 días" -> in3, "hace 3 días" -> ago3, "la próxima semana" -> "2026-09-11",
      "el viernes por la mañana" -> "2026-09-11",
      "завтра" -> tomorrow, "послезавтра" -> dayAfter, "вчера" -> yesterday, "в следующий четверг" -> nextThursday,
      "в прошлый четверг" -> lastThursday, "через 3 дня" -> in3, "3 дня назад" -> ago3, "на следующей неделе" -> "2026-09-11",
      "завтра" -> tomorrow, "післязавтра" -> dayAfter, "вчора" -> yesterday, "наступного четверга" -> nextThursday,
      "минулого четверга" -> lastThursday, "через 3 дні" -> in3, "3 дні тому" -> ago3, "наступного тижня" -> "2026-09-11",
      "jutro" -> tomorrow, "pojutrze" -> dayAfter, "wczoraj" -> yesterday, "w przyszły czwartek" -> nextThursday,
      "w zeszły czwartek" -> lastThursday, "za 3 dni" -> in3, "3 dni temu" -> ago3, "w przyszłym tygodniu" -> "2026-09-11",
      "明日" -> tomorrow, "明後日" -> dayAfter, "昨日" -> yesterday, "来週の木曜日" -> nextThursday,
      "先週の木曜日" -> lastThursday, "3日後" -> in3, "3日前" -> ago3, "来週" -> "2026-09-11", "3月14日" -> "2027-03-14")
    val wrong = cases.filter((s, iso) => !day(s).contains(iso)).map((s, iso) => s"'$s' -> ${day(s)} (expected $iso)")
    assertEquals(wrong, Nil)
  }

  test("times: 15h, 15 Uhr, 15時, and hh:mm anywhere") {
    assertEquals(parse("mardi à 15h").map(_.iso), Some("2026-09-08T15:00"))
    assertEquals(parse("am Dienstag um 15 Uhr").map(_.iso), Some("2026-09-08T15:00"))
    assertEquals(parse("火曜日の15時に").map(_.iso), Some("2026-09-08T15:00"))
    assertEquals(parse("el martes a las 15:30").map(_.iso), Some("2026-09-08T15:30"))
    assertEquals(parse("14 mars à 9h30").map(_.iso), Some("2027-03-14T09:30"))
  }

  test("what no lexicon says is still None, and English is unchanged") {
    assertEquals(parse("bientôt"), None)
    assertEquals(parse("soon"), None)
    assertEquals(parse("Can we meet on Tuesday at 3pm?").map(_.iso), Some("2026-09-08T15:00"))
    assertEquals(parse("Shall we meet next thursday at 2pm?").map(_.iso), Some("2026-09-10T14:00"))
  }
}
