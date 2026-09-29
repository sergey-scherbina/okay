package okay.dlm

import okay.testkit.Munit

class TestRuleKeywords extends Munit.Diagnosed:

  private def hits(keywords: Vector[String], text: String): Boolean =
    note(s"rules $keywords against «$text»")
    keywords.exists(_.r.findFirstIn(text).nonEmpty)

  test("a plain word is a whole word, any case, any script") {
    val rs = Rule.keywords("bug", "ошибка")
    assert(hits(rs, "Found a Bug today"))
    assert(!hits(rs, "open the debugger"))
    assert(hits(rs, "Ошибка при входе"))
  }

  test("a trailing * is a word prefix") {
    val rs = Rule.keywords("payout*", "crash*")
    assert(hits(rs, "payouts are late"))
    assert(hits(rs, "one payout"))
    assert(hits(rs, "it crashes"))
    assert(!hits(rs, "repayout"))
  }

  test("a keyword with spaces is a phrase over any whitespace") {
    val rs = Rule.keywords("charged twice")
    assert(hits(rs, "I was CHARGED  twice"))
    assert(!hits(rs, "charged once, twice billed"))
  }

  test("metacharacters are literal") {
    val rs = Rule.keywords("c++")
    assert(hits(rs, "I write c++ code"))
    assert(!hits(rs, "I write c code"))
  }

  test("plain and prefix words compile to the shape the typo layer mines") {
    val rs = Rule.keywords("pricing", "upgrade", "invoice*", "charged twice")
    assertEquals(rs.flatMap(Fuzzy.literalTriggers).toSet, Set("pricing", "upgrade", "invoice"))
    val model = Dlm.rules(Intents(Vector(Intent("sales", rules = Rule.keywords("pricing", "upgrade"), semantic = false))))
    val route = model.router.route("what is your pricng?")
    onFailure(s"route: $route")
    assertEquals(route, Route.Fires("sales", Map.empty, Support.Typo(1)))
  }

  test("a blank keyword is refused by name") {
    val e = intercept[IllegalArgumentException](Rule.keywords("ok", "  "))
    assert(e.getMessage.contains("\"  \""), e.getMessage)
    intercept[IllegalArgumentException](Rule.keywords("*"))
  }

  test("intents JSON takes keywords beside rules") {
    val parsed = Intents.parse("""{"intents":[{"name":"billing","rules":["(?i)\\brefund\\b"],"keywords":["invoice*","charged twice"]}]}""")
    val billing = parsed.toOption.flatMap(_.byName("billing"))
    onFailure(s"parsed: $parsed")
    assertEquals(billing.map(_.rules), Some(Vector("(?i)\\brefund\\b") ++ Rule.keywords("invoice*", "charged twice")))
  }
