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

  // --- what the aggregate hides (intent-per-class-not-aggregate) ---

  private def confusionOf(f: String => Option[String]): Eval.Confusion =
    heldOut.foldLeft(Eval.Confusion()) { case (m, (msg, gold)) =>
      f(msg).fold(m)(pred => m.observe(gold, pred))
    }

  test("the shipped number, per class and against the majority baseline") {
    // A consumer's finding, and it lands on this file: they filled a
    // corpus hole, one class reached 137 of 184 rows, and their
    // HEADLINE ACCURACY ROSE from 95.8% to 96.2% while a class died,
    // because accuracy on an imbalanced corpus rewards predicting the
    // biggest one. 76.7% is an aggregate with the same exposure.
    val m = confusionOf(msg =>
      Patterns.classify(Models.cues, msg, floor = 0.4)
        .orElse(CharGrams.score(Models.meeting, msg).map(_.best)))
    val r = Eval.report(m)
    println(f"[balance] majority baseline ${100.0 * m.majorityBaseline}%.1f%%, " +
      m.balance.toVector.sorted.map((c, sh) => f"$c%s ${100.0 * sh}%.0f%%").mkString(", "))
    r.perClass.toVector.sortBy(_._1).foreach { (c, sc) =>
      println(f"[class] $c%-13s P ${sc.precision}%.2f  R ${sc.recall}%.2f  F1 ${sc.f1}%.2f  n=${m.support(c)}%2d")
    }
    println(f"[macro] macro F1 ${r.macroF1}%.3f, worst class ${r.worst.map(_._1).getOrElse("-")}%s")

    // the corpus is close to balanced, so the aggregate is not being
    // carried by one class — asserted, because it is the premise
    // under which 76.7% means anything
    assert(m.majorityBaseline < 0.40,
      f"the majority class is ${100.0 * m.majorityBaseline}%.0f%% of held-out rows; " +
        "an accuracy quoted over that is a claim about the biggest class")
    // and no class may collapse, which is what a mean would hide
    val floor = r.perClass.values.map(_.f1).min
    assert(floor >= 0.50,
      f"a class fell to F1 $floor%.2f (${r.worst.map(_._1).getOrElse("?")}%s) — " +
        "the total can rise while this falls, which is the failure this asserts against")
  }

  test("the cue tier alone, per class, since it is the half that is trusted") {
    val m = confusionOf(msg => Patterns.classify(Models.cues, msg, floor = 0.4))
    val r = Eval.report(m)
    r.perClass.toVector.sortBy(_._1).foreach { (c, sc) =>
      println(f"[cues] $c%-13s P ${sc.precision}%.2f  R ${sc.recall}%.2f  F1 ${sc.f1}%.2f  n=${m.support(c)}%2d")
    }
    // where it fires it is trusted, so its PRECISION is the property
    // to hold rather than its coverage
    val worstPrecision = r.perClass.values.map(_.precision).min
    println(f"[cues] answered ${m.total}%2d of ${heldOut.size}%2d, worst precision $worstPrecision%.2f")
    assert(worstPrecision >= 0.70, f"a cue class dropped to precision $worstPrecision%.2f")
  }
}

