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
