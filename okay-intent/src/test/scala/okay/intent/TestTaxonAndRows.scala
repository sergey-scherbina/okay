package okay.intent

import okay.codec.{Json, Schema}
import okay.rag.{Embedding, embedding}

/**
 * One taxonomy, and a fit that knows its languages
 * (specs/intent-classify.md) — requests 1 and 2 of the consumer seven.
 */
class TestTaxonAndRows extends munit.FunSuite {

  import IntentFixture.Meeting
  private given sM: Schema[Meeting] = summon[Schema[Meeting]]

  // ---------------------------------------------------------------
  // (1) one taxonomy value, two doors

  test("a taxonomy read from a Schema names the same classes the model tier sees") {
    val t = Taxon.of[Meeting]
    assertEquals(t.classes.sorted,
      Vector("MeetingNotification", "MeetingProposal", "MeetingRequest", "NotAboutMeetings"))
    // and it is the SAME list Classify.label produces, which is the
    // whole point of the two tiers sharing one value
    val one = Meeting.MeetingProposal("x")
    assert(t.has(Classify.label(one)), s"${Classify.label(one)} is not in ${t.classes}")
  }

  test("a taxonomy parsed from data reaches the same place, which a Schema cannot") {
    // this is what a distilled corpus can produce: classes as strings
    val t = Taxon.parsed(
      Seq("Proposal", "Request"),
      Seq("shall we meet" -> "Proposal", "please send it" -> "Request"))
    assertEquals(t.classes, Vector("Proposal", "Request"))
    assertEquals(t.phrasings, Vector("shall we meet" -> "Proposal", "please send it" -> "Request"))
    assertEquals(t.examplesFor("Proposal"), Vector("shall we meet"))
  }

  test("a taxonomy round-trips as data, so it can be edited without a compiler") {
    val t = Taxon.parsed(Seq("A", "B"), Seq("x" -> "A"))
    val back = Json.decode(summon[Schema[Taxon]])(Json.parseValue(Json.write(t)))
    assertEquals(back, Right(t))
  }

  test("an unknown label is refused, not quietly turned into a class") {
    val t = Taxon.parsed(Seq("A", "B"))
    assertEquals(t.check(Seq("A", "B", "A")), Right(()))
    t.check(Seq("A", "Bb", "C")) match
      case Left(msg) =>
        assert(msg.contains("Bb") && msg.contains("C"), msg)
      case Right(_) => fail("a typo became a class")
  }

  // ---------------------------------------------------------------
  // (2) language as a key

  private def vec(x: Double): Embedding = embedding(Array(x.toFloat, 0.1f))
  private def rows(lang: String, n: Int, cls: String => String): Seq[Row] =
    (0 until n).map(i => Row(s"$lang-$i", vec(if i % 2 == 0 then 1.0 else -1.0),
      cls(s"$i"), lang))

  test("a language with enough rows gets its own model; a thin one borrows the pooled") {
    val data = rows("en", 40, i => if i.toInt % 2 == 0 then "A" else "B") ++
               rows("ru", 6, i => if i.toInt % 2 == 0 then "A" else "B")
    val fitted = ByLanguage.fit(data)(rs => rs.map(_.cls).distinct.sorted)
    assert(fitted.isOwn("en"), "40 rows is above the threshold")
    assert(!fitted.isOwn("ru"), "6 rows must not become a model of its own")
    assertEquals(fitted("ru"), fitted.pooled, "a thin language borrows rather than guesses")
    assertEquals(fitted("de"), fitted.pooled, "an unseen language borrows too")
  }

  test("the threshold is the learning curve's, and it is a policy a caller can move") {
    val data = rows("en", 10, _ => "A")
    assert(!ByLanguage.fit(data)(identity).isOwn("en"))
    assert(ByLanguage.fit(data, minRows = 5)(identity).isOwn("en"))
  }

  test("an untagged corpus behaves exactly as it did before languages existed") {
    val data = (0 until 40).map(i => Row(s"m$i", vec(1.0), if i % 2 == 0 then "A" else "B"))
    val fitted = ByLanguage.fit(data)(rs => rs.length)
    assertEquals(fitted.fittedFor, Set.empty[String])
    assertEquals(fitted("anything"), 40, "everything falls through to the pooled model")
  }
}
