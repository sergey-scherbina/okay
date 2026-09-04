package okay.intent

/**
 * The shipped model, and every number the doc comment claims about it
 * (specs/intent-classify.md).
 *
 * JVM-scoped only because `MakeModel` is: the artifact must be
 * REPRODUCIBLE, and proving that means re-fitting it here. The loading
 * side is cross-platform and `TestModelsCross` covers it.
 */
class TestModels extends munit.FunSuite {

  private val heldOut: Seq[(String, String)] =
    IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

  test("the shipped artifact is exactly what the generator produces") {
    // it is a committed blob, so the thing that makes it trustworthy
    // is that anyone can re-derive it: same corpus, same fit, same
    // bytes. If this fails, someone hand-edited the generated file or
    // the fixture moved — rerun MakeModel and read the diff.
    assertEquals(Fit.save(MakeModel.model), MeetingModel.json)
  }

  test("the corpus the model was fitted on is disjoint from the one it is scored on") {
    val fitted = MakeModel.corpus.map(_._1).toSet
    assert(heldOut.forall((m, _) => !fitted.contains(m)),
      "the shipped model would be scored on its own training data")
    assertEquals(MakeModel.corpus.size, 60)
    assertEquals(heldOut.size, 60)
  }

  test("61.7% alone on held-out English, which is why it is not offered alone") {
    val hit = heldOut.count((m, gold) => CharGrams.score(Models.meeting, m).exists(_.best == gold))
    assertEquals(hit, 37, s"the doc comment claims 61.7%, this is ${100.0 * hit / heldOut.size}")
  }

  test("76.7% at full coverage behind the cues, with no network at all") {
    // the number a caller gets from `Models` plus `Patterns` and
    // nothing else: no gateway, no embedder, no fitting at startup
    val right = heldOut.count { (m, gold) =>
      Patterns.classify(Models.cues, m, floor = 0.4)
        .orElse(CharGrams.score(Models.meeting, m).map(_.best))
        .contains(gold)
    }
    assertEquals(right, 46, s"the doc comment claims 76.7%, this is ${100.0 * right / heldOut.size}")
    // and the split the comment quotes: the cues answer half of it
    val fired = heldOut.flatMap((m, gold) =>
      Patterns.classify(Models.cues, m, floor = 0.4).map(_ -> gold))
    assertEquals(fired.size, 32)
    assertEquals(fired.count((got, gold) => got == gold), 29)
  }
}
