package okay.script

import okay.script.api.{Inline, Lang}

/** specs/site-framework.md stage 3: every language on the element,
 * the client picks — beside the per-request road, not instead of it.
 */
class TestInlineI18n extends munit.FunSuite:

  private val langs = Inline.Langs(Vector("pl", "en", "uk"), pinned = Some("pl"))

  test("attrs carry every language given, and skip one with no text") {
    val a = Inline.attrs("pl" -> "Skrócenie spodni", "en" -> "Shortening", "uk" -> "")
    assertEquals(a, """ data-pl="Skrócenie spodni" data-en="Shortening"""")
    // a quote in a translation is an attribute value, not a way out
    assert(Inline.attrs("en" -> """a "quote"""").contains("&quot;"))
  }

  test("the element renders the DEFAULT language's text, so the page is right before any script runs") {
    val el = Inline.span(langs, "pl" -> "Wymiana zamka", "en" -> "Zipper replacement")
    assert(el.startsWith("<span data-pl="), el)
    assert(el.endsWith(">Wymiana zamka</span>"), el)
    // ...and with the default missing it falls back to what there is,
    // rather than rendering an empty element
    val partial = Inline.span(langs, "en" -> "Only English")
    assert(partial.endsWith(">Only English</span>"), partial)
    // a language the page does not offer is not carried at all
    assert(!Inline.span(langs, "pl" -> "x", "de" -> "y").contains("data-de"), Inline.span(langs, "pl" -> "x", "de" -> "y"))
  }

  test("a placeholder is an attribute, so it gets its own carrier") {
    val p = Inline.placeholder("pl" -> "Twoje imię", "en" -> "Your name")
    assertEquals(p, """ data-ph-pl="Twoje imię" data-ph-en="Your name"""")
  }

  test("the switcher marks the current language and calls the script's own setter") {
    val s = Inline.switcher(langs)
    assert(s.contains("""<button type="button" data-l="pl" class="on" onclick="setLang('pl')">PL</button>"""), s)
    assert(s.contains("""data-l="uk" onclick="setLang('uk')">UK"""), s)
  }

  test("the script offers exactly the page's languages and agrees with the server about the cookie") {
    val js = Inline.script(langs)
    assert(js.contains("var sup=['pl','en','uk'], dflt='pl';"), js)
    // the SAME cookie the per-request road reads, so the two roads
    // cannot disagree about what the visitor chose
    assert(js.contains(Lang.Cookie), js)
    assert(js.contains("localStorage") && js.contains("navigator.language"), js)
    assert(js.contains("textContent") && js.contains("data-ph-"), js)
  }

  test("the default is read at USE time, so a `val langs` in a declare block cannot freeze one visitor's language") {
    // a page holds this as a val: it is built ONCE per compile
    val held = Inline.Langs.of("pl", "en", "uk")
    Lang.scoped.where("uk"):
      assertEquals(held.default, "uk")
    Lang.scoped.where("en"):
      assertEquals(held.default, "en")        // the same value, a new request
    Lang.scoped.where("de"):
      assertEquals(held.default, "pl")        // a language the page does not offer
    Lang.scoped.where("uk"):
      assertEquals(held.pinned, None)
      assertEquals(Inline.Langs(Vector("pl", "en"), pinned = Some("en")).default, "en")
  }
