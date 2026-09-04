package okay.intent

/** specs/intent-classify.md — the conditions a row was taken under */
class TestConditions extends munit.FunSuite {

  test("a row cannot be written without its terms") {
    val c = Conditions(Conditions.SmallEmbedder, Conditions.Classify, 60, 60)
    val row = Conditions.line(c, "probe", "88.3%")
    for term <- Seq("Qwen3-Embedding-0.6B", "classify-instruction", "train=60", "test=60",
                    "IntentFixture.labelled")
    do assert(row.contains(term), s"a row without $term: $row")
  }

  test("two framings of the same measurement are visibly different rows") {
    // the exact confusion that cost a retraction: same embedder, same
    // split, same number — and the rows must not read alike
    val bare = Conditions.line(
      Conditions(Conditions.SmallEmbedder, Conditions.Bare, 60, 60), "centroid", "90.0%")
    val framed = Conditions.line(
      Conditions(Conditions.SmallEmbedder, Conditions.Classify, 60, 60), "centroid", "85.0%")
    assertNotEquals(bare, framed)
    assert(bare.contains("framing=bare"))
    assert(framed.contains("framing=classify-instruction"))
  }

  test("extra conditions ride along, and an absent one leaves no empty bracket") {
    val plain = Conditions(Conditions.LargeEmbedder, Conditions.Bare, 40, 20).header
    assert(!plain.contains("  ]"), plain)
    val withExtra = Conditions(Conditions.LargeEmbedder, Conditions.Bare, 40, 20,
      extra = "distilled=40").header
    assert(withExtra.contains("distilled=40"), withExtra)
  }
}
