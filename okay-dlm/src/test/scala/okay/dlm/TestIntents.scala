package okay.dlm

import munit.FunSuite

class TestIntents extends FunSuite:

  val raw = """{"intents": [
    {"name": "need", "rules": ["(?iU)\\b(?:нужен)\\b"],
     "examples": {"ru": ["нужен сантехник"], "pl": ["potrzebuję hydraulika"]},
     "slots": [{"name": "what", "pattern": "нужен\\s+(.+)", "fallback": true}],
     "help": {"ru": {"say": "нужен сантехник", "does": "ищет мастера"}}, "rank": 10},
    {"name": "accept", "rules": ["(?iU)\\bберусь\\s+(\\d+)"], "semantic": false,
     "slots": [{"name": "deal", "pattern": "(\\d+)"}], "require": ["deal"], "ask": "какую сделку?",
     "internal": "carries a number"},
    {"name": "other", "examples": ["привет", "спасибо"]}
  ]}"""

  test("the authored file parses into intents, phrasings by language, slots and decisions") {
    val set = Intents.parse(raw).toOption.get
    assertEquals(set.names, Vector("need", "accept", "other"))
    val need = set.byName("need").get
    assertEquals(need.byLang.keySet, Set("ru", "pl"))
    assertEquals(need.examples, Vector("potrzebuję hydraulika", "нужен сантехник"))
    assertEquals(need.slots.head.fallback, true)
    assertEquals(need.rank, 10)
    assertEquals(need.help("ru").does, "ищет мастера")
    val accept = set.byName("accept").get
    assert(!accept.semantic)
    assertEquals(accept.ask, Map("*" -> "какую сделку?"))
    assertEquals(accept.internal, Some("carries a number"))
    // a bare array is legal and means "language not declared"
    assertEquals(set.byName("other").get.byLang, Map("*" -> Vector("привет", "спасибо")))
    // what the vector layer compiles: only the intents that opted in
    assertEquals(set.rows.map(_._1).distinct, Vector("need", "other"))
    assertEquals(set.byLang("pl"), Vector("potrzebuję hydraulika"))
  }

  test("a valid set validates to nothing") {
    assertEquals(Intents.validate(Intents.parse(raw).toOption.get), Vector.empty)
  }

  test("malformations are data: a bad regex, an undeclared slot, a duplicate, a dead intent, a mixed script") {
    val bad = Intents(Vector(
      Intent("a", rules = Vector("(?iU)\\b(unclosed")),
      Intent("a", require = Vector("x"), ask = Map("*" -> "?")),
      Intent("b", semantic = false),
      Intent("c", rules = Vector("(?iU)\\b(?:наprawiam)\\b"))))
    val found = Intents.validate(bad)
    assert(found.exists(_.contains("bad regex")), found.toString)
    assert(found.exists(_.contains("requires undeclared slot 'x'")), found.toString)
    assert(found.exists(_.contains("declared 2 times")), found.toString)
    assert(found.exists(_.contains("nothing can ever route to it")), found.toString)
    assert(found.exists(_.contains("mixes two scripts")), found.toString)
  }

  test("a word in two scripts is a typo, in any pair of scripts") {
    assert(Script.mixed("наprawiam"))
    assert(Script.mixed("wοrk"))          // a Greek omicron inside a Latin word
    assert(!Script.mixed("naprawiam"))
    assert(!Script.mixed("исправляю"))
    assert(!Script.mixed("x2"))
    assertEquals(Script.dominant("Wrocław 2026"), Some(java.lang.Character.UnicodeScript.LATIN))
    assertEquals(Script.dominant("40 zł"), Some(java.lang.Character.UnicodeScript.LATIN))
    assertEquals(Script.dominant("123 ?"), None)
  }

  test("an alphabet narrows a word to the languages it could be written in") {
    val a = Alphabet.of("ru", "uk", "pl", "en").toOption.get
    assertEquals(a.languagesOf("снимаю"), Set("ru", "uk"))   // nothing decides
    assertEquals(a.languagesOf("знімаю"), Set("uk"))         // і is Ukrainian's alone
    assertEquals(a.languagesOf("ищешь"), Set("ru", "uk"))
    assertEquals(a.languagesOf("ещё"), Set("ru"))
    assertEquals(a.languagesOf("szukam"), Set("pl", "en"))
    assertEquals(a.languagesOf("proszę"), Set("pl"))
    assertEquals(a.languagesOf("42"), Set("ru", "uk", "pl", "en"))
    assert(a.agrees("pl", "szukam pracy"))
    assert(!a.agrees("pl", "ищу работу"))
    // Latin rides inside any other script: an address does not make a
    // Russian sentence Latin, and a Russian word makes a sentence Cyrillic
    assert(a.agrees("ru", "сценарий deal seeker=anna@example.org provider=bob@example.org"))
    assert(!a.agrees("en", "моя почта anna@example.org"))
    assertEquals(Script.native("моя почта anna@example.org"), Some(java.lang.Character.UnicodeScript.CYRILLIC))
    assertEquals(Script.native("anna@example.org"), Some(java.lang.Character.UnicodeScript.LATIN))
    assert(Alphabet.none.agrees("pl", "ищу работу"))
    assertEquals(Alphabet.of("ru", "xx").left.map(_.contains("xx")), Left(true))
  }

  test("THE MENU IS SELECTED BY THE LIBRARY: internal out, rank then name in order, the help cell of the language asked") {
    val set = Intents.parse(raw).toOption.get
    assertEquals(set.menu("ru").map(_._1), Vector("need"), "accept is internal; other was never described")
    assertEquals(set.menu("ru").head._2, Intent.Help("нужен сантехник", "ищет мастера"))
    assertEquals(set.menu("pl"), Vector.empty, "described in no other language, so offered in none")
  }

  test("the order is rank then NAME, so a build that moves a rule does not move the menu") {
    def one(name: String, rank: Int) =
      Intent(name, help = Map("en" -> Intent.Help(name, s"does $name")), rank = rank)
    val set = Intents(Vector(one("zulu", 10), one("alpha", 10), one("mid", 5),
      Intent("hidden", help = Map("en" -> Intent.Help("h", "h")), internal = Some("carries a number"))))
    assertEquals(set.menu("en").map(_._1), Vector("mid", "alpha", "zulu"))
  }

  test("menuHoles names what a language describes nowhere — a menu that shrinks in silence is the failure") {
    val set = Intents(Vector(
      Intent("a", help = Map("en" -> Intent.Help("a", "a"), "ru" -> Intent.Help("а", "а"))),
      Intent("b", help = Map("en" -> Intent.Help("b", "b"))),
      Intent("c", internal = Some("not offered"))))
    assertEquals(set.menuHoles(Set("en", "ru", "pl")), Map("ru" -> Vector("b"), "pl" -> Vector("a", "b")))
    assertEquals(set.menuHoles(Set("en")), Map.empty[String, Vector[String]], "nothing missing is no entry")
  }
