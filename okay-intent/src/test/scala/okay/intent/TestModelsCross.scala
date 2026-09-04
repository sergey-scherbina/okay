package okay.intent

/**
 * Loading the shipped model, on every platform it is built for
 * (specs/intent-classify.md).
 *
 * The point of the artifact being a generated SOURCE rather than a
 * classpath resource is that this suite passes on Scala.js too.
 */
class TestModelsCross extends munit.FunSuite {

  test("the shipped model loads with no network and no fitting") {
    val m = Models.meeting
    assertEquals(m.classes, Vector("Notification", "Other", "Proposal", "Request"))
    assertEquals(m.dim, 1024)
  }

  test("it answers, and the cue tier answers first") {
    assertEquals(CharGrams.classify(Models.meeting, "Shall we meet on Tuesday?", 0.0),
      Some("Proposal"))
    assertEquals(Patterns.classify(Models.cues, "Could you send me the agenda?"),
      Some("Request"))
  }

  test("a caller's own corpus takes the same road, and survives it") {
    val rows = (0 until 20).map(i =>
      if i % 2 == 0 then (s"could you send the thing $i", "Request")
      else (s"shall we meet on day $i", "Proposal"))
    val mine = Fit.grams(rows, dim = 256)
    val back = Fit.grams(Fit.save(mine)).getOrElse(fail("it did not load"))
    assertEquals(back.classes, mine.classes)
    for m <- Seq("could you send the report", "shall we meet on friday") do
      assertEquals(CharGrams.score(back, m).map(_.best), CharGrams.score(mine, m).map(_.best))
  }

  test("a model that is not one says so rather than throwing") {
    assert(Fit.grams("""{"nope":1}""").isLeft)
    assert(Fit.probe("not json at all").isLeft)
  }
}
