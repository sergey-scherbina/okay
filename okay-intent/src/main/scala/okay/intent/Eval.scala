package okay.intent

import okay.{Aggregator, Monoid}

/**
 * Classifier evaluation (specs/intent-classify.md).
 *
 * A confusion matrix is a MONOID, so evaluation is a fold: it
 * distributes, partial runs merge, and a growing fixture is an append
 * rather than a rerun — the same property `Postings` has for the same
 * reason.
 *
 * Two things here are not decoration. `Report` is per class, because
 * macro F1 alone hides the class you keep missing: the lane's own
 * first measurement scored 0.615 macro F1 while `Other` had recall
 * 0.17, and only the matrix said so. And `regressions` makes the
 * promotion rule EXECUTABLE — "promote only if no single class falls
 * more than two points" is a function returning the offenders, not a
 * paragraph someone is supposed to remember.
 */
object Eval {

  /** counts keyed by (gold, predicted) */
  final case class Confusion(cells: Map[(String, String), Int] = Map.empty):
    def observe(gold: String, pred: String): Confusion =
      Confusion(cells.updated((gold, pred), cells.getOrElse((gold, pred), 0) + 1))
    def count(gold: String, pred: String): Int = cells.getOrElse((gold, pred), 0)
    /** every class named on either side — a class the classifier
     * invented is as much a class as one it never reached */
    def classes: Vector[String] =
      cells.keys.flatMap((g, p) => Seq(g, p)).toVector.distinct.sorted
    def total: Int = cells.values.sum

    /** how many rows a class actually HAS — the number an average
     * hides. A class with four rows and a class with a hundred move a
     * mean by very different amounts, and only one of them can carry
     * a claim. */
    def support(gold: String): Int =
      cells.collect { case ((g, _), n) if g == gold => n }.sum

    /** each class's share of the corpus */
    def balance: Map[String, Double] =
      val t = total
      if t == 0 then Map.empty
      else classes.map(c => c -> support(c).toDouble / t).filter(_._2 > 0.0).toMap

    /**
     * What a classifier that always guesses the biggest class would
     * score — which is the number an accuracy has to BEAT before it
     * means anything.
     *
     * A consumer measured the failure this exposes: they filled a
     * corpus hole, one class grew to 137 of 184 rows, a probe leaned
     * to the majority, and their headline accuracy ROSE from 95.8% to
     * 96.2% while a class died. On an imbalanced corpus accuracy
     * rewards predicting the biggest class, and the aggregate cannot
     * tell you that is what happened.
     */
    def majorityBaseline: Double =
      if total == 0 then 0.0 else balance.values.maxOption.getOrElse(0.0)

  object Confusion:
    /** matrices combine cellwise. In the companion, so the implicit
     * scope of Confusion finds it without an import. */
    given Monoid[Confusion] with
      def empty: Confusion = Confusion()
      def combine(a: Confusion, b: Confusion): Confusion =
        Confusion(b.cells.foldLeft(a.cells) { case (m, (k, v)) =>
          m.updated(k, m.getOrElse(k, 0) + v)
        })

  final case class ClassScore(precision: Double, recall: Double, f1: Double)

  final case class Report(perClass: Map[String, ClassScore], macroF1: Double):
    def f1(c: String): Double = perClass.get(c).map(_.f1).getOrElse(0.0)

    /** the class that is doing worst, which is the one a mean hides
     * and the one a regression shows up in first */
    def worst: Option[(String, ClassScore)] = perClass.minByOption(_._2.f1)

  /** the scores a matrix implies */
  def report(m: Confusion): Report =
    val cs = m.classes
    val scores = cs.map { c =>
      val tp = m.count(c, c)
      val fp = cs.filter(_ != c).map(g => m.count(g, c)).sum
      val fn = cs.filter(_ != c).map(p => m.count(c, p)).sum
      val pr = if tp + fp == 0 then 0.0 else tp.toDouble / (tp + fp)
      val rc = if tp + fn == 0 then 0.0 else tp.toDouble / (tp + fn)
      val f1 = if pr + rc == 0.0 then 0.0 else 2 * pr * rc / (pr + rc)
      (c, ClassScore(pr, rc, f1))
    }
    val macro_ = if scores.isEmpty then 0.0 else scores.map(_._2.f1).sum / scores.length
    Report(scores.toMap, macro_)

  /**
   * One streaming pass over (gold, predicted) pairs.
   *
   * Feed it only REAL labels. A caller's own sentinel ("undecodable",
   * "timed out") becomes a predicted-only class with F1 0 — correct by
   * the rule above, and wrong for a marker nobody is classifying — and
   * macro F1 then moves with the sentinel's rate instead of with the
   * classification. Measured: two runs whose per-class scores were
   * identical reported 0.916 and 0.748 because they differed by one
   * such row (intent-other-collapse). Count sentinels separately and
   * report them beside the score, never inside it.
   */
  val confusion: Aggregator[(String, String), Confusion, Report] =
    new Aggregator[(String, String), Confusion, Report]:
      def init: Confusion = Confusion()
      def add(acc: Confusion, in: (String, String)): Confusion = acc.observe(in._1, in._2)
      def merge(a: Confusion, b: Confusion): Confusion = summon[Monoid[Confusion]].combine(a, b)
      def present(acc: Confusion): Report = report(acc)

  /**
   * The promotion rule, executable: the classes whose F1 fell by more
   * than `tolerance` against the baseline. Empty means promotable.
   *
   * A class the candidate dropped entirely counts as a fall to zero,
   * which is the whole point — silence is the failure mode this rule
   * exists to catch.
   */
  def regressions(baseline: Report, candidate: Report,
                  tolerance: Double = 0.02): List[String] =
    baseline.perClass.toList
      .filter((c, s) => s.f1 - candidate.f1(c) > tolerance)
      .map(_._1)
      .sorted
}
