package okay.intent

/**
 * `Duration` in the fixture's other seven languages
 * (specs/intent-classify.md, intent-duration-multilingual).
 *
 * The parallel fixture has no duration rows, so the fixture is here:
 * six meanings, eight wordings each, dictionary facts rather than
 * corpus data. The law is `Temporal`'s: one meaning, one value,
 * whatever the wording.
 */
class TestDurationMultilingual extends munit.FunSuite {

  /** meaning -> (language -> wording) */
  private val fixture: List[(Int, Map[String, String])] = List(
    30 -> Map("en" -> "30 minutes", "fr" -> "30 minutes", "de" -> "30 Minuten", "es" -> "30 minutos",
      "ru" -> "30 минут", "uk" -> "30 хвилин", "pl" -> "30 minut", "ja" -> "30分"),
    60 -> Map("en" -> "an hour", "fr" -> "une heure", "de" -> "eine Stunde", "es" -> "una hora",
      "ru" -> "час", "uk" -> "одну годину", "pl" -> "godzinę", "ja" -> "1時間"),
    30 -> Map("en" -> "half an hour", "fr" -> "une demi-heure", "de" -> "eine halbe Stunde", "es" -> "media hora",
      "ru" -> "полчаса", "uk" -> "півгодини", "pl" -> "pół godziny", "ja" -> "半時間"),
    90 -> Map("en" -> "an hour and a half", "fr" -> "une heure et demie", "de" -> "anderthalb Stunden", "es" -> "una hora y media",
      "ru" -> "полтора часа", "uk" -> "півтори години", "pl" -> "półtorej godziny", "ja" -> "1時間半"),
    120 -> Map("en" -> "two hours", "fr" -> "deux heures", "de" -> "zwei Stunden", "es" -> "dos horas",
      "ru" -> "два часа", "uk" -> "дві години", "pl" -> "dwie godziny", "ja" -> "2時間"),
    15 -> Map("en" -> "a quarter of an hour", "fr" -> "un quart d'heure", "de" -> "eine Viertelstunde", "es" -> "un cuarto de hora",
      "ru" -> "четверть часа", "uk" -> "чверть години", "pl" -> "kwadrans", "ja" -> "15分"),
    45 -> Map("en" -> "forty-five minutes", "fr" -> "quarante-cinq minutes", "de" -> "fünfundvierzig Minuten", "es" -> "cuarenta y cinco minutos",
      "ru" -> "сорок пять минут", "uk" -> "сорок п'ять хвилин", "pl" -> "czterdzieści pięć minut", "ja" -> "45分"),
    150 -> Map("en" -> "2.5 hours", "fr" -> "deux heures et demie", "de" -> "zweieinhalb Stunden", "es" -> "dos horas y media",
      "ru" -> "два с половиной часа", "uk" -> "дві з половиною години", "pl" -> "dwie i pół godziny", "ja" -> "2時間半"))

  test("one meaning, eight wordings, one value") {
    val misses = for
      (minutes, byLang) <- fixture
      (lang, text) <- byLang.toList.sortBy(_._1)
      got = Duration.parse(text)
      if !got.contains(minutes)
    yield s"$lang '$text' -> $got (expected $minutes)"
    println(s"\n[duration] meanings ${fixture.size}; languages ${fixture.head._2.size}; misses ${misses.size}")
    misses.foreach(m => println("  " + m))
    assertEquals(misses, Nil)
  }

  test("inside a sentence, with the evidence as the phrase") {
    val cases = List(
      // the evidence is the SHORTEST window that reproduces the value:
      // "demi-heure" alone reads 30, so the article is not evidence
      "Pouvons-nous nous voir mardi pour une demi-heure ?" -> ("demi-heure", 30),
      "Können wir uns für 45 Minuten treffen?" -> ("45 Minuten", 45),
      "¿Podemos reunirnos dos horas el jueves?" -> ("dos horas", 120),
      "Созвонимся на полчаса в пятницу?" -> ("полчаса", 30),
      "Зустрінемося на дві години?" -> ("дві години", 120),
      "Spotkajmy się na pół godziny w czwartek." -> ("pół godziny", 30),
      "火曜日に30分だけ話せますか。" -> ("火曜日に30分だけ話せますか。", 30))
    val wrong = cases.filter((s, e) => !Duration.find(s).contains(okay.frame.Found(e._1, e._2)))
      .map((s, e) => s"'$s' -> ${Duration.find(s)} (expected $e)")
    assertEquals(wrong, Nil)
  }

  test("what no lexicon says is still None, and English is unchanged") {
    for s <- List("bientôt", "un moment", "eine Weile", "un rato", "скоро", "chwilę", "しばらく", "") do
      assertEquals(Duration.parse(s), None, s"'$s'")
    assertEquals(Duration.parse("an hour and a half"), Some(90))
    assertEquals(Duration.parse("Can we meet next Tuesday for 30 minutes?"), Some(30))
  }
}
