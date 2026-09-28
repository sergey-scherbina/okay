package okay.dlm

import munit.FunSuite

/** the rule layer, the typo layer, the vector layer and the memory
 * band, in the order they decide */
class TestRouter extends FunSuite:

  val need = Intent("need", rules = Vector("(?iU)\\b(?:нужен|нужна|ищу)\\b"),
    byLang = Map("ru" -> Vector("мне нужен сантехник", "ищу электрика на завтра", "кто починит холодильник")),
    slots = Vector(Slot("what", "(?iU)(?:нужен|нужна|ищу)\\s+(.+)", fallback = true)))
  val offer = Intent("offer", rules = Vector("(?iU)\\b(?:умею|могу|предлагаю)\\b"),
    byLang = Map("ru" -> Vector("умею писать программы на Scala", "могу починить стиральную машину", "предлагаю услуги электрика")),
    slots = Vector(Slot("what", "(?iU)(?:умею|могу|предлагаю)\\s+(.+)", fallback = true)))
  val accept = Intent("accept", rules = Vector("(?iU)\\b(?:берусь|accept)\\s+(\\d+)"),
    slots = Vector(Slot("deal", "(?iU)(?:берусь|accept)\\s+(\\d+)")),
    require = Vector("deal"), ask = Map("ru" -> "какую сделку?"), semantic = false)
  val intents = Intents(Vector(offer, need, accept))
  val rules = Router(intents)

  test("the rule whose match is EARLIEST wins, not the one authored first") {
    // offer sits first in the file, need is said first in the sentence
    rules.route("Мне нужна работа. Я умею программировать") match
      case Route.Fires("need", slots, Support.Exact(Some(rule))) =>
        assert(rule.contains("нужен"), rule)
        assertEquals(slots("what"), "работа. Я умею программировать")
      case other => fail(s"$other")
  }

  test("a trigger decides wherever it sits in the sentence") {
    assertEquals(rules.route("Я программист, ищу работу").named, Some("need"))
  }

  test("a required slot that is missing is Missing, not Fires and not Unclear") {
    assertEquals(rules.route("берусь"), Route.Unclear(Vector.empty, 0f))
    rules.route("берусь 12") match
      case Route.Fires("accept", slots, _) => assertEquals(slots("deal"), "12")
      case other => fail(s"$other")
  }

  test("a typo of a trigger word fires by the fuzzy layer, one edit away") {
    rules.route("ищю сантехника") match
      case Route.Fires("need", _, Support.Typo(1)) => ()
      case other => fail(s"$other")
  }

  test("an exact-format rule contributes no fuzzy vocabulary — a slip on a deal number is never corrected") {
    assertEquals(rules.fuzzyVocab.get("accept"), None)
    assertEquals(rules.fuzzyVocab("need").sorted, Vector("ищу", "нужен", "нужна"))
  }

  test("too little text for the vector layer is Unclear, and asking is the honest answer") {
    assertEquals(rules.route("для работы"), Route.Unclear(Vector.empty, 0f))
  }

  test("without exemplars the vector layer is not live and every unrouted message is Unclear") {
    assert(!rules.semantic)
    assertEquals(rules.route("у меня сломался холодильник, кто поможет?"), Route.Unclear(Vector.empty, 0f))
  }

  test("with exemplars over an encoder, an authored phrasing routes to its own intent with a probability") {
    val embed = okay.rag.Vectors.hashing(256)
    val ex = Exemplars.compile(intents.rows, embed, "hashing-256")
    val r = Router(intents, Some(ex), Some(embed), margin = 0.2f)
    assert(r.semantic)
    r.route("кто починит холодильник") match
      case Route.Fires("need", _, Support.Semantic(p, Some("offer"))) => assert(p > 0.5f, p.toString)
      case other => fail(s"$other")
    // and the numbers are visible, best first
    assertEquals(r.scores("кто починит холодильник").map(_._1).headOption, Some("need"))
  }

  test("`noticed` reports every intent a rule matched, in reading order, while `route` acts on the first") {
    val seen = rules.noticed("Мне нужна работа. Я умею программировать").map(_._1)
    assertEquals(seen, Vector("need", "offer"))
  }

  test("a lesson the person taught decides BEFORE the rules, and the slots come from the sentence at hand") {
    val m = Memory.learn(Memory.empty, "ann", "мои заявки", "offer", 7L)
    rules.route("мои заявки", memory = m, who = "ann") match
      case Route.Fires("offer", slots, Support.Remembered(7L, 1.0f)) => assertEquals(slots("what"), "мои заявки")
      case other => fail(s"$other")
    // one typo away is the same lesson, a little less near
    rules.route("мои заявкы", memory = m, who = "ann") match
      case Route.Fires("offer", _, Support.Remembered(7L, near)) => assertEqualsFloat(near, 0.8f, 0.001f)
      case other => fail(s"$other")
    // and somebody else's sentence is nobody's lesson
    assertEquals(rules.route("мои заявки", memory = m, who = "bob"), Route.Unclear(Vector.empty, 0f))
  }

  test("`command` is an exact-argument intent fired by rule, and nothing else") {
    assertEquals(rules.command("берусь 3"), Some("accept"))
    assertEquals(rules.command("нужен сантехник"), None)   // semantic intent: not a command
    assertEquals(rules.command("берусь"), None)            // Missing, not Fires
  }

  test("typo tolerance never crosses a language when an alphabet is given") {
    val shoot = Intent("shoot", rules = Vector("(?iU)\\b(?:съёмка)\\b"))   // ъ and ё: Russian's alone
    val rent = Intent("rent", rules = Vector("(?iU)\\b(?:знімаю)\\b"))     // і: Ukrainian's alone
    val set = Intents(Vector(shoot, rent))
    val isolated = Router(set, alphabet = Alphabet.of("ru", "uk").toOption.get)
    assertEquals(isolated.route("зн1маю квартиру", lang = Some("uk")).named, Some("rent"))
    // a Ukrainian conversation is not matched against a Russian trigger
    assertEquals(isolated.route("съемка кино", lang = Some("uk")), Route.Unclear(Vector.empty, 0f))
    assertEquals(isolated.route("съемка кино", lang = Some("ru")).named, Some("shoot"))
    // without an alphabet nothing is isolated, and the nearest wins
    assertEquals(Router(set).route("съемка кино", lang = Some("uk")).named, Some("shoot"))
  }
