package okay.dlm

import munit.FunSuite
import java.nio.file.Files

class TestLanguage extends FunSuite:

  val alphabet = Alphabet.of("ru", "uk", "pl", "en").toOption.get
  val cues = Language.Cues(alphabet,
    words = Map(
      "ru" -> Set("что", "нужен", "привет", "сегодня"),
      "uk" -> Set("що", "потрібен", "привіт", "сьогодні"),
      "pl" -> Set("nie", "tak", "szukam", "cześć"),
      "en" -> Set("the", "need", "hello")),
    ignore = Some("(?iU)\\d[\\d\\s.,]*\\s*(?:zł|zl|pln|€|eur|\\$|usd)".r))

  test("letters decide where they appear; words split the languages sharing a script") {
    assertEquals(cues.of("привет, что сегодня?"), Some("ru"))
    assertEquals(cues.of("привіт, що сьогодні?"), Some("uk"))
    assertEquals(cues.of("ещё раз"), Some("ru"))          // ё is Russian's alone
    assertEquals(cues.of("szukam pracy"), Some("pl"))
    assertEquals(cues.of("I need the plumber"), Some("en"))
    assertEquals(cues.of("берусь 1"), None)                // no evidence: never a guess
    assertEquals(cues.of("привет, моя почта anna@example.org"), Some("ru"))   // an address is not Latin evidence
    assertEquals(cues.of("praca"), None)                   // Latin, and nothing decides
  }

  test("a price is not a language: «40 zł» carries no Polish") {
    assertEquals(cues.of("вход 40 zł"), None)
    assertEquals(cues.of("вход 40 zł сегодня"), Some("ru"))
  }

  val phrases = Map(
    "ru" -> Vector("мне нужен сантехник", "ищу работу программистом", "что сегодня вечером во Вроцлаве"),
    "pl" -> Vector("potrzebuję hydraulika", "szukam pracy jako programista", "co dziś wieczorem we Wrocławiu"),
    "en" -> Vector("I need a plumber", "looking for a job as a programmer", "what is on tonight in Wrocław"))

  test("the trigram detector places a long sentence and refuses a short one") {
    val d = Language.Detector.of(phrases, alphabet = alphabet)
    assert(d.nonEmpty)
    assertEquals(d.of("szukam pracy jako programista scala"), Some("pl"))
    assertEquals(d.of("ищу работу программистом в Кракове"), Some("ru"))
    assertEquals(d.of("ok 1"), None)
    assertEquals(d.scores("szukam pracy jako programista").head._1, "pl")
  }

  test("a strong match overrides the length rule; a cross-alphabet winner is refused") {
    val d = Language.Detector.of(phrases, alphabet = alphabet, minLetters = 40)
    assertEquals(d.of("potrzebuję hydraulika"), Some("pl"))   // short, but in the file
    // a detector whose only evidence is Cyrillic cannot call a Latin message anything
    val ruOnly = Language.Detector.of(phrases.filter(_._1 == "ru"), alphabet = alphabet)
    assertEquals(ruOnly.of("szukam pracy jako programista scala"), None)
    // …and with no alphabet it would, which is what the gate is for
    val ungated = Language.Detector.of(phrases.filter(_._1 == "ru"), floor = 0f)
    assertEquals(ungated.of("szukam pracy jako programista scala"), Some("ru"))
  }

  test("the profiles as an artifact: JSON and checkpoint, the same numbers") {
    val rows = Language.Profiles.rows(phrases)
    assertEquals(rows.length, 9)
    val back = Language.Profiles.parse(Language.Profiles.print(rows))
    assertEquals(back.map(_._1), rows.map(_._1))
    assertEquals(back.map(_._2), rows.map(_._2))
    val dir = Files.createTempDirectory("dlm")
    val json = dir.resolve("lang.vec.json")
    Language.Profiles.write(json, rows)
    val l = Checkpoint.read(Checkpoint.binaryOf(json), Some((Language.Profiles.encoder, 64))).toOption.get
    assertEquals(Language.Profiles.ofCheckpoint(l).map(_._1), rows.map(_._1))
    val d = Language.Detector(Language.Profiles.ofCheckpoint(l), alphabet = alphabet)
    assertEquals(d.of("szukam pracy jako programista scala"), Some("pl"))
    assertEquals(Language.Profiles.resource("/no-such.vec.json"), None)
  }
