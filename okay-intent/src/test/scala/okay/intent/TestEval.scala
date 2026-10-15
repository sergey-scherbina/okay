package okay.intent

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import okay.Monoid

/** specs/intent-classify.md — the evaluation half */
class TestEval extends munit.ScalaCheckSuite {

  private val label: Gen[String] = Gen.oneOf("Proposal", "Request", "Notification", "Other")
  private val pair: Gen[(String, String)] = for g <- label; p <- label yield (g, p)
  private val matrix: Gen[Eval.Confusion] =
    Gen.listOf(pair).map(_.foldLeft(Eval.Confusion())((m, gp) => m.observe(gp._1, gp._2)))

  private val M = summon[Monoid[Eval.Confusion]]

  property("a confusion matrix is a monoid — identity") {
    forAll(matrix) { m =>
      M.combine(M.empty, m) == m && M.combine(m, M.empty) == m
    }
  }

  property("a confusion matrix is a monoid — associativity") {
    forAll(matrix, matrix, matrix) { (a, b, c) =>
      M.combine(M.combine(a, b), c) == M.combine(a, M.combine(b, c))
    }
  }

  property("merging partial runs equals folding the whole") {
    forAll(Gen.listOf(pair), Gen.listOf(pair)) { (xs, ys) =>
      val whole = Eval.confusion.run(xs ++ ys)
      val parts = Eval.report(M.combine(
        xs.foldLeft(Eval.confusion.init)(Eval.confusion.add),
        ys.foldLeft(Eval.confusion.init)(Eval.confusion.add)))
      whole == parts
    }
  }

  test("per-class precision, recall and F1 are the textbook numbers") {
    // A: 2 correct, 1 missed (called B); B: 1 correct, 1 wrong (was A)
    val m = Eval.confusion.run(List(("A", "A"), ("A", "A"), ("A", "B"), ("B", "B")))
    val a = m.perClass("A")
    val b = m.perClass("B")
    assertEqualsDouble(a.precision, 1.0, 1e-9)          // nothing else was called A
    assertEqualsDouble(a.recall, 2.0 / 3.0, 1e-9)       // one of three A's escaped
    assertEqualsDouble(b.precision, 0.5, 1e-9)          // two called B, one right
    assertEqualsDouble(b.recall, 1.0, 1e-9)
    assertEqualsDouble(m.macroF1, (0.8 + 2.0 / 3.0) / 2, 1e-9)
  }

  test("a class the classifier invented is still a class") {
    val m = Eval.confusion.run(List(("A", "A"), ("A", "Z")))
    assert(m.perClass.contains("Z"), "a predicted-only label must be scored, not dropped")
    assertEqualsDouble(m.perClass("Z").precision, 0.0, 1e-9)
  }

  test("the promotion rule is empty for an identical report") {
    val r = Eval.confusion.run(List(("A", "A"), ("B", "B")))
    assertEquals(Eval.regressions(r, r), Nil)
  }

  test("one confusion damages BOTH classes it involves") {
    // B called A costs B its recall and A its precision — a rule that
    // named only the missed class would let half the damage through
    val base = Eval.confusion.run(List(("A", "A"), ("A", "A"), ("B", "B"), ("B", "B")))
    val worse = Eval.confusion.run(List(("A", "A"), ("A", "A"), ("B", "A"), ("B", "B")))
    assertEquals(Eval.regressions(base, worse), List("A", "B"))
  }

  test("the rule names exactly the class that fell when the damage lands on one") {
    // the mistaken prediction is a label the baseline never had, so
    // only B loses anything
    val base = Eval.confusion.run(List(("A", "A"), ("A", "A"), ("B", "B"), ("B", "B")))
    val worse = Eval.confusion.run(List(("A", "A"), ("A", "A"), ("B", "Z"), ("B", "B")))
    assertEquals(Eval.regressions(base, worse), List("B"))
  }

  test("a class that vanished from the candidate counts as a fall to zero") {
    val base = Eval.confusion.run(List(("A", "A"), ("B", "B")))
    val gone = Eval.confusion.run(List(("A", "A")))
    assertEquals(Eval.regressions(base, gone), List("B"))
  }

  test("a fall inside the tolerance is not a regression") {
    val base = Eval.confusion.run(List.fill(200)(("A", "A")) ++ List.fill(100)(("B", "B")))
    val slightly = Eval.confusion.run(
      List.fill(199)(("A", "A")) ++ List(("A", "B")) ++ List.fill(100)(("B", "B")))
    assertEquals(Eval.regressions(base, slightly, tolerance = 0.05), Nil)
  }

  test("macro F1 hides a class that is entirely missing — the matrix does not") {
    // the lane's own first measurement, in miniature: Other never
    // predicted, absorbed by the positive classes (spec, Results)
    val m = Eval.confusion.run(
      List.fill(6)(("Request", "Request")) ++
      List.fill(6)(("Notification", "Notification")) ++
      List.fill(3)(("Proposal", "Proposal")) ++ List.fill(3)(("Proposal", "Request")) ++
      List.fill(2)(("Other", "Request")) ++ List.fill(4)(("Other", "Notification")))
    assertEqualsDouble(m.perClass("Other").recall, 0.0, 1e-9)
    assert(m.macroF1 > 0.4, "the aggregate still reads as mediocre-but-working")
    assert(m.macroF1 < 0.7, "which is exactly why the aggregate is not enough")
  }
}
