package okay.intent

/**
 * A refit that would kill a class REFUSES, and says which one
 * (specs/intent-autonomy.md §2.5, intent-refit-gate).
 *
 * Fitting is cheap and looks safe: rows in, model out, the headline
 * moves up. The failure this guards is one a consumer of this module
 * already lived — their corpus grew unevenly, one class reached 137
 * of 184 rows, their headline accuracy rose from 95.8% to 96.2%, and
 * a class died on the way. No aggregate says that, and nobody reads a
 * per-class table they were not handed.
 *
 * So the door is not `fit`, it is `propose`: fit the candidate, score
 * BOTH the candidate and the incumbent on the same held-out rows,
 * and answer a `Verdict` that carries every class's before and after.
 * `Refused` is data with the offending class named, never an
 * exception and never a silent write — the caller decides whether to
 * publish, and a caller that ignores the verdict has to do so in
 * writing.
 *
 * TWO RULES, both from measurements in specs/intent-classify.md:
 *   - THE LAW: no class may fall below `floor` F1 (0.50 shipped) —
 *     the same rule `Models`' own suite asserts, so a refit cannot
 *     hand the suite a model it will reject.
 *   - NO CLASS MAY DROP by more than `slip` (0.10 shipped) against
 *     the incumbent, even while staying above the law, because a
 *     class sliding 0.85 → 0.55 over three refits passes the law
 *     every time and is dead at the end.
 *
 * The whole harvest programme writes into this path, which is why the
 * guard exists before the data does.
 */
object Refit {

  /** what a refit is allowed to do, in one value a caller can print */
  final case class Rules(floor: Double = 0.50, slip: Double = 0.10)

  final case class ClassScore(cls: String, before: Double, after: Double):
    def delta: Double = after - before
    def show: String = f"$cls%-14s ${before}%.2f -> ${after}%.2f  ${delta}%+.2f"

  enum Verdict:
    /** publish it: every class is above the law and none slipped */
    case Accepted(model: CharGrams.Trained, scores: Vector[ClassScore], total: Double)
    /** do not publish: `why` names the class and the rule it broke */
    case Refused(why: String, scores: Vector[ClassScore], total: Double)

  /** per-class F1 of one classifier over the held-out rows */
  private def scoreOf(classify: String => Option[String],
                      heldOut: Seq[(String, String)]): (Map[String, Double], Double) =
    val confusion = heldOut.foldLeft(Eval.Confusion()) { case (acc, (m, gold)) =>
      classify(m).fold(acc)(p => acc.observe(gold, p)) }
    val report = Eval.report(confusion)
    val right = heldOut.count((m, gold) => classify(m).contains(gold))
    (report.perClass.map((c, s) => c -> s.f1), if heldOut.isEmpty then 0.0 else right.toDouble / heldOut.size)

  /**
   * Fit `rows` and decide whether the result may replace `incumbent`,
   * judged on `heldOut` — which must not overlap `rows`, and the
   * caller is the one who knows that (a corpus and its held-out half
   * come from the same place).
   *
   * `incumbent` absent means the first fit: the law still applies, so
   * a first model that cannot name a class is refused too.
   */
  def propose(rows: Seq[(String, String)],
              heldOut: Seq[(String, String)],
              incumbent: Option[CharGrams.Trained] = None,
              rules: Rules = Rules(),
              dim: Int = 4096, low: Int = 2, high: Int = 3): Verdict =
    val candidate = Fit.grams(rows, dim, low, high)
    val (after, total) = scoreOf(m => CharGrams.score(candidate, m).map(_.best), heldOut)
    val (before, _) = incumbent
      .map(t => scoreOf(m => CharGrams.score(t, m).map(_.best), heldOut))
      .getOrElse((Map.empty[String, Double], 0.0))
    val classes = (before.keySet ++ after.keySet).toVector.sorted
    val scores = classes.map(c => ClassScore(c, before.getOrElse(c, 0.0), after.getOrElse(c, 0.0)))

    // the law first: a class the model cannot name at all is the
    // failure this exists for, and it is not a matter of degree
    val broke = scores.find(_.after < rules.floor)
    // then the slide: above the law but falling, which three refits
    // turn into the same death with none of them looking wrong
    val slid = scores.find(s => incumbent.isDefined && s.before - s.after > rules.slip)
    (broke, slid) match
      case (Some(s), _) =>
        Verdict.Refused(f"${s.cls} would be at F1 ${s.after}%.2f, below the floor ${rules.floor}%.2f", scores, total)
      case (_, Some(s)) =>
        Verdict.Refused(f"${s.cls} would slip ${s.before}%.2f -> ${s.after}%.2f, more than ${rules.slip}%.2f", scores, total)
      case _ => Verdict.Accepted(candidate, scores, total)

  /** the before/after table, printed the way a refit should report
   * itself — a caller that publishes without reading this is choosing
   * to, rather than never having been told */
  def report(v: Verdict): String =
    val (head, scores, total) = v match
      case Verdict.Accepted(_, s, t) => ("ACCEPTED", s, t)
      case Verdict.Refused(why, s, t) => (s"REFUSED: $why", s, t)
    val rows = scores.map("  " + _.show).mkString("\n")
    f"$head%s%ntotal on held-out: ${100.0 * total}%.1f%%%n$rows"
}
