package okay.intent

import okay.rag.{Embedding, Token, Tokens, embedding}

/** specs/intent-spans.md — the pure tier, over vectors nobody encoded */
class TestSpans extends munit.FunSuite {

  // a four-dimensional world: axis 0 is "place", axis 1 is "time",
  // axis 2 is "carrier noise", axis 3 is "anything else"
  private def v(xs: Double*): Embedding = embedding(xs.map(_.toFloat).toArray)
  private val place = v(1, 0, 0, 0)
  private val time = v(0, 1, 0, 0)
  private val noise = v(0, 0, 1, 0)
  private val other = v(0, 0, 0, 1)

  /** a fake encoder: every word gets the vector its spelling says */
  private def lexicon(word: String): Embedding = word.toLowerCase.filter(_.isLetter) match
    case "wrocław" | "warsaw" | "kyiv" | "вроцлаве" | "варшаве" => place
    case "tomorrow" | "saturday" | "завтра" | "субботу" => time
    case "carrier" | "нужен" | "сантехник" => noise
    case _ => other
  private val fake: Tokens = text =>
    Spans.words(text).map(w => Token(w.text, w.start, w.end, lexicon(w.text)))

  test("inContext keeps exactly the phrase's tokens out of the carrier") {
    val got = Spans.inContext(fake, "нужен сантехник {}", "во Вроцлаве")
    assertEquals(got.length, 2)
    assertEquals(got, Vector(other, place))
    // no carrier: the phrase alone
    assertEquals(Spans.inContext(fake, "no hole here", "завтра"), Vector(time))
  }

  test("train pools phrases to unit vectors and a centroid, and a slot with nothing is absent") {
    val m = Spans.train(Seq(
      "place" -> Vector(place, place), "place" -> Vector(v(2, 0, 0, 0)),
      "time" -> Vector(time), "empty" -> Vector.empty))
    assertEquals(m.slots, Vector("place", "time"))
    assertEquals(m.phrases("place").length, 2)
    assertEqualsDouble(okay.rag.Vectors.cosine(m.centroids("place"), place), 1.0, 1e-6)
    assert(!m.centroids.contains("empty"), "an empty slot must not be a zero vector")
  }

  test("a window may open on a preposition and may not close on one, nor be made of them") {
    val spans = Spans.windows("что интересного во Вроцлаве").map(_.text)
    assert(spans.contains("во Вроцлаве"), spans)
    assert(!spans.contains("во"), "a bare preposition is not a place")
    assert(!spans.contains("интересного во"), "a span does not end on a function word")
    assert(spans.contains("Вроцлаве"))
    // the rule is a parameter: with no function words, «во» is a window
    assert(Spans.windows("во Вроцлаве", function = Set.empty).map(_.text).contains("во"))
  }

  test("find answers the best window per slot, only above the threshold, with both scores") {
    val m = Spans.train(Seq("place" -> Vector(place), "time" -> Vector(time)))
    val text = "нужен сантехник во Вроцлаве завтра"
    val found = Spans.find(fake(text), text, m, threshold = 0.6)
    assertEquals(found.map(s => s.slot -> s.text), Vector("place" -> "Вроцлаве", "time" -> "завтра"))
    // the span's characters are the text's own
    for s <- found do assertEquals(text.substring(s.start, s.end), s.text)
    assert(found.forall(s => s.score >= 0.6 && s.nearest >= 0.6))
    // a threshold nothing clears answers nothing, rather than the least bad window
    assertEquals(Spans.find(fake("нужен сантехник"), "нужен сантехник", m, threshold = 0.6), Vector.empty)
  }

  test("the model survives save and load") {
    val m = Spans.train(Seq("place" -> Vector(place, v(0.9, 0.1, 0, 0)), "time" -> Vector(time)))
    val back = Fitted.load(Fitted.save(m))
    assertEquals(back.slots, m.slots)
    for slot <- m.slots do
      assertEquals(back.phrases(slot), m.phrases(slot))
      assertEqualsDouble(okay.rag.Vectors.cosine(back.centroids(slot), m.centroids(slot)), 1.0, 1e-6)
  }
}
