package okay.agent

import okay.rag.{Embedding, embedding}

/**
 * The calibration logic, in the default gate with synthetic vectors
 * (specs/intent-classify.md).
 *
 * The live suite measures what the classifier achieves; this one pins
 * the property that took three attempts to get right — that a promise
 * is only made when the sample can carry it.
 */
class TestNoModelCalibration extends munit.FunSuite {

  /** two classes, separable along one axis, so the probe learns
   * something and the interesting variable is the calibration */
  private def vec(x: Double, noise: Double = 0.0): Embedding =
    embedding(Array(x.toFloat, noise.toFloat, 0.1f))

  private def rows(n: Int, clean: Boolean): Seq[(String, Embedding, String)] =
    (0 until n).map { i =>
      val a = i % 2 == 0
      val label = if a then "A" else "B"
      // when `clean` is false, every fifth row is mislabelled, which
      // is what manufactures calibration errors
      val flipped = !clean && i % 5 == 0
      val x = if a != flipped then 1.0 else -1.0
      (s"row-$i", vec(x, (i % 7) * 0.01), label)
    }

  test("no promise is made from a sample that cannot carry one") {
    val fit = rows(40, clean = true)
    val cal = rows(20, clean = true)     // ~no errors at all
    val m = NoModel.fit(fit, cal)
    assertEquals(m.promise, None,
      "a bound at 95% needs 19 calibration errors; a clean sample has none to spend")
    assertEquals(m.errorsNeeded, 19)
  }

  test("a promise appears once there are enough errors to support it") {
    val fit = rows(40, clean = true)
    val cal = rows(200, clean = false)   // ~40 errors, past the 19 needed
    val m = NoModel.fit(fit, cal, targetAccuracy = 0.95)
    assert(m.calibrationErrors >= m.errorsNeeded,
      s"expected enough errors to bound: ${m.calibrationErrors} of ${m.errorsNeeded}")
    assertEquals(m.promise, Some(0.95))
  }

  test("a looser error rate needs fewer errors, and says so") {
    val fit = rows(40, clean = true)
    val cal = rows(60, clean = false)
    val strict = NoModel.fit(fit, cal, targetAccuracy = 0.95)
    val loose = NoModel.fit(fit, cal, targetAccuracy = 0.8)
    assert(loose.errorsNeeded < strict.errorsNeeded,
      s"${loose.errorsNeeded} should be below ${strict.errorsNeeded}")
    assertEquals(loose.errorsNeeded, 4)
  }

  test("the default blend is no blend, and it is a decision not an omission") {
    val fit = rows(20, clean = true)
    val cal = rows(20, clean = true)
    assertEquals(NoModel.fit(fit, cal).patternWeight, 0.0)
    // a caller with data passes a grid, and then it is fitted
    val withGrid = NoModel.fit(fit, cal, weights = Seq(0.0, 0.5))
    assert(Seq(0.0, 0.5).contains(withGrid.patternWeight))
  }

  test("classify answers exactly when the confidence clears the threshold") {
    // the CONTRACT, not an outcome: which points clear a threshold
    // depends on the calibration data, and asserting that a
    // hand-picked vector is answered would test the fixture rather
    // than the rule
    val fit = rows(40, clean = true)
    val cal = rows(40, clean = false)
    val m = NoModel.fit(fit, cal)
    for probe <- Seq(vec(1.0), vec(-1.0), vec(0.0), vec(0.2)) do
      val forced = NoModel.force(m, "x", probe)
      val answered = NoModel.classify(m, "x", probe)
      assert(forced.isDefined, "forcing must always produce a class")
      answered match
        case Some(v) =>
          assert(v.confidence >= m.threshold, s"answered below the threshold: $v")
          assertEquals(Some(v.best), forced, "answering and forcing must agree on the class")
        case None => ()   // deferred, which is a legitimate answer
  }

  test("a threshold of zero answers everything, which is the degenerate case") {
    val fit = rows(40, clean = true)
    val cal = rows(20, clean = true)     // no errors at all -> threshold 0
    val m = NoModel.fit(fit, cal)
    assertEquals(m.threshold, 0.0)
    assert(NoModel.classify(m, "x", vec(1.0)).isDefined,
      "with nothing to be careful about, the tier must not be careful")
  }
}
