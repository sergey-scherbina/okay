package okay.intent

/**
 * Cues induced from the corpus (specs/intent-classify.md,
 * intent-rule-induction).
 *
 * `Patterns` is the trusted tier: hand-written phrases, right nine
 * times in ten where they fire, and firing on half the messages —
 * because its coverage grows with someone's patience. This grows it
 * with the corpus instead, the way RIPPER does: a rule is a
 * conjunction of literals — a word, an adjacent pair, or either at the
 * start of the message — GROWN on two thirds of the training rows by
 * the literal that gains most information about the class, and
 * PRUNED on the other third by dropping trailing literals while the
 * rule's precision there does not fall; kept if it still covers two
 * rows at the precision floor, then the rows it covers are set aside
 * and the next rule is grown, class by class, rarest first.
 *
 * Deterministic: the split is by row index, candidates are sorted,
 * ties go to the earlier literal. Zero network, zero model: a
 * `Trained` is a vector of rules a person can read, and the tier
 * answers the way `Patterns` does — the fired rules vote, the margin
 * says how divided the vote was, and `classify` defers below it.
 *
 * MEASURED at sixty rows (intent-rule-induction, 2026-09-07): the
 * hand-written cues fire on 53.3% of the held-out messages and are
 * right 90.6% of the time; the induced ones reach that coverage only
 * at a 0.8 floor and 67.7% precision, or 85.7% precision at 11.7%
 * coverage with the 0.9 floor that is the default — a cue tier's
 * property is being right where it fires, so the default keeps that
 * and says how little it covers. At sixty rows induction buys one of
 * the two, not both; the corpus, not the algorithm, is the limit.
 */
object Induced {

  /** every literal must be in the message's features */
  final case class Rule(cls: String, literals: List[String]):
    def fires(fs: Set[String]): Boolean = literals.forall(fs.contains)
    def show: String = s"${literals.mkString(" & ")} -> $cls"

  final case class Trained(taxon: Taxon, rules: Vector[Rule]):
    def silent: Vector[String] = taxon.classes.filterNot(rules.map(_.cls).toSet)

  final case class Verdict(best: String, score: Double, margin: Double,
                           runnerUp: Option[String], fired: Vector[Rule])

  private def tokens(text: String): List[String] =
    text.toLowerCase.replaceAll("[^\\p{L}\\p{N}' ]", " ").split("\\s+").filter(_.nonEmpty).toList

  /** the literals a message offers: its words, its adjacent pairs,
   * and the first of each marked `^` — position is a cue too
   * (`Patterns.Cue.atStart`: "please send" opening a message is a
   * request; mentioned inside one it is not) */
  def features(text: String): Set[String] =
    val ts = tokens(text)
    val pairs = ts.sliding(2).collect { case List(a, b) => s"$a $b" }.toList
    val starts = ts.headOption.map("^" + _).toList ++ pairs.headOption.map("^" + _).toList
    (ts ++ pairs ++ starts).toSet

  private def log2(x: Double): Double = math.log(x) / math.log(2.0)

  def train(labelled: Seq[(String, String)], minSupport: Int = 2, minPrecision: Double = 0.9): Trained =
    val classes = labelled.map(_._2).distinct.sorted.toVector
    val rows = labelled.toVector.zipWithIndex.map { case ((t, c), i) => (features(t), c, i) }
    val grow = rows.filter(_._3 % 3 != 2)
    val prune = rows.filter(_._3 % 3 == 2)
    // rarest class first, as RIPPER orders them; ties by name
    val order = classes.sortBy(c => (labelled.count(_._2 == c), c))
    val rules = Vector.newBuilder[Rule]
    for cls <- order do
      var positives = grow.filter(_._2 == cls).map(_._1)
      val negatives = grow.filter(_._2 != cls).map(_._1)
      var go = positives.nonEmpty
      while go do
        // grow: candidates are literals in at least two of the remaining positives
        val candidates = positives.flatten.groupBy(identity).collect { case (l, ls) if ls.size >= 2 => l }.toVector.sorted
        var literals = List.empty[String]
        var pos = positives
        var neg = negatives
        var growing = candidates.nonEmpty
        while growing && neg.nonEmpty do
          val p0 = pos.size.toDouble
          val n0 = neg.size.toDouble
          val scored = candidates.filterNot(literals.contains).map { l =>
            val p1 = pos.count(_.contains(l)).toDouble
            val n1 = neg.count(_.contains(l)).toDouble
            // a literal that would leave the rule standing on fewer
            // than `minSupport` rows is not a literal, it is a row's name
            val gain = if p1 < minSupport then Double.NegativeInfinity
                       else p1 * (log2(p1 / (p1 + n1)) - log2(p0 / (p0 + n0)))
            (gain, l)
          }
          scored.filter(_._1 > 0).sortBy((g, l) => (-g, l)).headOption match
            case Some((_, l)) =>
              literals = literals :+ l
              pos = pos.filter(_.contains(l))
              neg = neg.filter(_.contains(l))
            case None => growing = false
        if literals.isEmpty then go = false
        else
          // prune: drop trailing literals while precision on the pruning set does not fall
          def precisionOn(ls: List[String], set: Vector[(Set[String], String, Int)]): (Double, Int) =
            val covered = set.filter(r => ls.forall(r._1.contains))
            val p = covered.count(_._2 == cls)
            (if covered.isEmpty then 0.0 else p.toDouble / covered.size, p)
          var kept = literals
          var prec = precisionOn(kept, prune)._1
          var pruning = kept.length > 1
          while pruning do
            val shorter = kept.init
            val (p2, _) = precisionOn(shorter, prune)
            // the pruning set is a third of a small corpus and often
            // says nothing about a specific rule (precision 0 of 0);
            // a shorter rule must not fall below the floor on the rows
            // it was grown on either, or an uncovered pruning set
            // would prune every rule down to its first word
            val (g2, _) = precisionOn(shorter, grow)
            if p2 >= prec && g2 >= minPrecision then { kept = shorter; prec = p2; pruning = kept.length > 1 }
            else pruning = false
          // KEEP OR STOP is judged on the whole training half, not on the
          // pruning third alone: at sixty rows the pruning set holds five
          // of a class, and one contrary row there was rejecting a rule
          // that was right on every one of the thirty it was grown on
          // ("we" for Proposal: 4/4 grown, 1/2 pruned). RIPPER's MDL
          // stopping is for corpora with a description length to
          // minimise; this one has a precision floor and a support floor.
          val (ap, as) = precisionOn(kept, rows)
          val ok = ap >= minPrecision && as >= minSupport
          if ok then
            rules += Rule(cls, kept)
            positives = positives.filterNot(r => kept.forall(r.contains))
            go = positives.nonEmpty
          else go = false
    Trained(Taxon.parsed(classes), rules.result())

  /** fitted against a taxonomy the caller declares */
  def against(taxon: Taxon, labelled: Seq[(String, String)], minSupport: Int = 2,
              minPrecision: Double = 0.9): Either[String, Trained] =
    taxon.check(labelled.map(_._2)).map(_ => train(labelled, minSupport, minPrecision).copy(taxon = taxon))

  def score(t: Trained, message: String): Option[Verdict] =
    val fs = features(message)
    val fired = t.rules.filter(_.fires(fs))
    if fired.isEmpty then None
    else
      val byClass = fired.groupBy(_.cls).map((c, rs) => c -> rs.size.toDouble)
      val ranked = byClass.toSeq.sortBy((c, s) => (-s, c))
      val (best, s0) = ranked.head
      val total = ranked.map(_._2).sum
      val margin = if total <= 0.0 then 0.0 else (s0 - ranked.lift(1).map(_._2).getOrElse(0.0)) / total
      Some(Verdict(best, s0, margin, ranked.lift(1).map(_._1), fired))

  def classify(t: Trained, message: String, floor: Double = 0.3): Option[String] =
    score(t, message).filter(_.margin >= floor).map(_.best)
}
