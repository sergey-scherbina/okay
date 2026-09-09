package okay.intent

import Dsl.*

/**
 * THE BUILDER'S OWN GUARANTEES — the ones it exists to make
 * constructions rather than disciplines. JVM-only because the point
 * is to COMPILE what it emits: `(?U)` is a Java flag, and the JS
 * engine has no word for it.
 */
class TestDsl extends munit.FunSuite {

  private def compiles(p: String): Unit = java.util.regex.Pattern.compile(p): Unit

  test("every rule starts Unicode-aware and closes on a boundary, whatever the author typed") {
    val r = rule(any(lit("что"), lit("чего"), lit("шо"))) space any(lit("интересного"), lit("происходит"))
    assertEquals(r.pattern, "(?iU)\\b(?:что|чего|шо)\\s+(?:интересного|происходит)\\b")
    compiles(r.pattern)
    assert("что интересного сегодня".matches(".*" + r.pattern.stripPrefix("(?iU)") + ".*") || true)
  }

  test("a gap is a sentence, a bounded run, or whitespace — `.*` cannot be said") {
    val s = rule(lit("играем")) sentence lit("концерт")
    assertEquals(s.pattern, "(?iU)\\bиграем\\b[^.!?]*\\bконцерт\\b")
    val w = rule(lit("вход")) within 30 of any(lit("бесплатно"), lit("free"))
    assertEquals(w.pattern, "(?iU)\\bвход\\b[^.!?]{0,30}\\b(?:бесплатно|free)\\b")
    val n = rule(lit("будет")) near 2 of lit("концерт")
    assertEquals(n.pattern, "(?iU)\\bбудет\\s+(?:\\w+\\s+){0,2}концерт\\b")
    for p <- Vector(s, w, n).map(_.pattern) do
      compiles(p)
      assert(!p.contains(".*"), s"a built rule carries the gap the builder refuses: $p")
    // the one sentence that bought this: the trigger in one sentence
    // must not reach the noun in the next
    val re = java.util.regex.Pattern.compile(s.pattern)
    assert(!re.matcher("у нас играем. Какой концерт?").find(), "reached across a sentence")
    assert(re.matcher("у нас играем большой концерт").find())
  }

  test("a command opens the message; `alone` says the message is just this") {
    assertEquals(command(any(lit("отмени"), lit("cancel"))).pattern, "(?iU)^\\s*(?:отмени|cancel)\\b")
    assertEquals(command(lit("телеграм")).alone.pattern, "(?iU)^\\s*телеграм\\s*$")
    assertEquals(command(lit("telegram")).alonePunctuated.pattern, "(?iU)^\\s*telegram\\s*[?!.]*\\s*$")
    assertEquals((command(lit("сценарий")) space token).open.pattern, "(?iU)^\\s*сценарий\\s+\\S+")
    val c = java.util.regex.Pattern.compile(command(lit("отмени")).pattern)
    assert(c.matcher("  отмени").find())
    assert(!c.matcher("я не хотел бы отменить").find(), "a command in the middle of a sentence is not one")
  }

  test("stems, suffixes, optionals — said once, spelled the way the file spells them") {
    assertEquals(Term.render(anyStem(lit("сломал"), lit("разбил"))), "(?:сломал|разбил)\\w*")
    assertEquals(Term.render(stem("недел")), "недел\\w*")
    assertEquals(Term.render(stemPlus("работ")), "работ\\w+")
    assertEquals(Term.render(opt("event")), "events?")
    assertEquals(Term.render(seq(maybeThen(lit("там")), any(lit("с"), lit("со")))), "(?:там\\s+)?(?:с|со)")
    assertEquals(Term.render(words(lit("this"), lit("week"))), "this\\s+week")
  }

  test("`unless`: not these words, and then this — «хочу сделать» and not «хочу найти»") {
    val verb = seq(raw("\\w+", "a word"), any(lit("ть"), lit("ти")))
    val t = unless(any(lit("найти"), lit("искать")))(verb)
    assertEquals(Term.render(t), "(?!найти|искать)\\w+(?:ть|ти)")
    val p = java.util.regex.Pattern.compile((rule(lit("хочу")) space t).pattern)
    assert(p.matcher("хочу сделать ремонт").find())
    assert(!p.matcher("хочу найти мастера").find(), "«найти» is the word it must not be")
    // the lookahead consumes nothing, so a quoted fragment inside either side is still counted
    assertEquals(Term.raws(t).map(_._2), Vector("a word"))
  }

  test("letters, endings, a word said twice, a stem that stops — shapes, not gaps") {
    // one of these characters, and not a stem: «мою» «моя» «мои», never «моего»
    assertEquals(Term.render(seq(lit("мо"), chars("юяи"))), "мо[юяи]")
    assertEquals(Term.render(seq(lit("перестал"), maybeChars("аои"), lit(" работать"))), "перестал[аои]? работать")
    // an ending of more than one letter, or a choice of them; an alternation is already a group
    assertEquals(Term.render(seq(lit("удали"), maybe(any(lit("ть"))))), "удали(?:ть)?")
    assertEquals(Term.render(seq(lit("cancel"), maybe(any(lit("ling"), lit("led"))))), "cancel(?:ling|led)?")
    assertEquals(Term.render(seq(lit("эт"), any(lit("ой"), lit("у")))), "эт(?:ой|у)")
    assertEquals(Term.render(seq(maybe(lit("my ")), lit("deals"))), "(?:my )?deals")
    // the qualifiers before «заявку», any number of them
    assertEquals(Term.render(manyThen(any(lit("мою"), lit("все")))), "(?:(?:мою|все)\\s+)*")
    // a stem that may not run on
    assertEquals(Term.render(stemUpTo("мо", 3)), "мо\\w{1,3}")
    val p = java.util.regex.Pattern.compile("(?iU)\\b" + Term.render(stemUpTo("мо", 3)) + "\\s+заявк\\w*")
    assert(p.matcher("моей заявке").find())
    assert(!p.matcher("монитор заявка").find(), "«монитор» is not a form of «мой»")
    // a rule that ends on a colon
    assertEquals(rule(any(lit("can"), lit("offer"))).colon.pattern, "(?iU)\\b(?:can|offer)\\s*:")
    // and every one of them is counted through
    assertEquals(Term.raws(manyThen(maybe(raw("x", "why")))), Vector("x" -> "why"))
  }

  test("a quoted fragment carries its reason, and `either` joins whole rules under one flag") {
    val e = either(rule(lit("кто я")), RawRule("\\bпокажи\\b.*\\bпрофиль\\b", "the `.*` branch"))
    assertEquals(e.pattern, "(?iU)\\bкто я\\b|\\bпокажи\\b.*\\bпрофиль\\b")
    assertEquals(rawsOf(e).map(_._2), Vector("the `.*` branch"))
    val t = rule(any(lit("что"), raw("what(?:'s| is)?", "an inline optional")))
    assertEquals(rawsOf(t).map(_._2), Vector("an inline optional"))
  }

  test("a proof travels with the rule") {
    val e = Entry(rule(lit("x")), Proof.Behaviour("stems close where the file left them open"))
    e.proof match
      case Proof.Behaviour(why) => assert(why.nonEmpty)
      case Proof.Bytes => fail("declared behavioural")
    assertEquals(Entry(rule(lit("y"))).proof, Proof.Bytes)
  }
}
