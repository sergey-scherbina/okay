package okay.frame

/**
 * The form itself, with no classifier and no conversation over it
 * (specs/conversation.md).
 *
 * Every slot here is a toy, deliberately: this module knows nothing
 * about dates, intents or journals, and a test that reached for one
 * would be testing a different module.
 */
class TestFrame extends munit.FunSuite {

  private val where = Slot[String]("where",
    Map("en" -> "Where?", "pl" -> "Gdzie?"),
    s => Option.when(s.trim.nonEmpty)(s.trim))

  private val count = Slot[Int]("count",
    Map("en" -> "How many?", "pl" -> "Ile?"),
    _.trim.toIntOption,
    extract = s => "\\b(\\d+)\\b".r.findFirstMatchIn(s)
      .flatMap(m => m.group(1).toIntOption.map(Found(m.group(1), _))))

  private def form = Frame.of("repair", where, count)

  test("a frame knows what is still to ask, and how much of it") {
    assertEquals(form.missing.map(_._1), Vector("where", "count"))
    assertEquals(form.remaining, 2)
    assert(!form.complete)
    val one = form.answer("where", "Wrocław").toOption.get
    assertEquals(one.remaining, 1)
    assertEquals(one.missing.map(_._2), Vector("How many?"))
  }

  test("the value comes back at the type the slot promised") {
    val f = form.answer("count", "3").toOption.get
    assertEquals(f.valueOf(count), Some(3))
    assertEquals(f.filled("count"), "3")
  }

  test("a slot that is not this slot gets nothing, however alike") {
    // what makes the lookup typed: a same-named slot of another type
    // must not be handed a value this one's parser produced
    val twin = Slot[Boolean]("count", Map("en" -> "How many?"), _ => Some(true))
    val f = form.answer("count", "3").toOption.get
    assertEquals(f.valueOf(twin), None)
  }

  test("an unreadable answer is not stored, and says what to ask again") {
    assertEquals(form.answer("count", "a few"), Left("How many?"))
    assertEquals(form.answer("nowhere", "x"), Left("no slot named nowhere"))
  }

  test("the language is the frame's, and no method takes another") {
    val pl = form.in("pl")
    assertEquals(pl.missing.map(_._2), Vector("Gdzie?", "Ile?"))
    assertEquals(pl.answer("count", "a few"), Left("Ile?"))
    // answering, extracting and filling all leave it alone
    assertEquals(pl.answer("where", "Wrocław").toOption.get.fillFrom("2 rooms").lang, "pl")
  }

  test("a language a slot cannot speak is visible before it ships") {
    assertEquals(form.in("pl").untranslated, Vector.empty)
    assertEquals(form.in("de").untranslated, Vector("where", "count"))
    // and it still asks, in the fallback, rather than failing a person
    assertEquals(form.in("de").missing.map(_._2), Vector("Where?", "How many?"))
  }

  test("an answer may answer more than was asked, and unread words are kept") {
    val f = form.take("where", "Wrocław, 2 rooms")
    assertEquals(f.valueOf(where), Some("Wrocław, 2 rooms"))
    assertEquals(f.valueOf(count), Some(2))
    assert(f.complete)

    val bad = form.take("count", "a few")
    assertEquals(bad.valueOf(count), None)
    assertEquals(bad.said("count"), Some("a few"))
    assertEquals(bad.remaining, 2)
  }

  test("a person's own answer is never overwritten by an extractor") {
    val f = form.answer("count", "3").toOption.get.fillFrom("9 rooms")
    assertEquals(f.valueOf(count), Some(3))
  }

  test("reading an answer clears the words that could not be read") {
    val f = form.take("count", "a few").take("count", "4")
    assertEquals(f.valueOf(count), Some(4))
    assertEquals(f.said("count"), None)
  }

  test("the words a parser cannot read are still what the person said") {
    // the consumer's case, from a live domain: a price slot parses
    // money, and "negotiable" is a perfectly good thing for a listing
    // to say. It is content, not a failure — a read-back built from
    // `filled` alone loses it.
    val f = form.take("where", "Wrocław").take("count", "negotiable")
    assertEquals(f.filled, Map("where" -> "Wrocław"))
    assertEquals(f.words, Map("where" -> "Wrocław", "count" -> "negotiable"))
    // and the typed reader still refuses to invent one
    assertEquals(f.valueOf(count), None)
  }

  test("a slot answered later is in `words` once, as the answer") {
    val f = form.take("count", "a few").take("count", "4")
    assertEquals(f.words("count"), "4")
    assertEquals(f.words.size, 1)
  }
}

