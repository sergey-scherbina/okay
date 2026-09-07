package okay.intent

/**
 * Stage 1 of the autonomy programme (specs/intent-autonomy.md): does
 * combining the offline tiers by learned AGREEMENT beat taking the
 * first that fires?
 *
 * The tiers are already labelling functions; what this suite adds is
 * DIVERSITY, because agreement between two labelers says very little
 * and the door ships two. Six offline labelers, none of which touches
 * a network:
 *
 *   cues        the hand-written phrases (`Patterns.meeting`)
 *   induced     cues grown from the corpus (`Induced`)
 *   grams23     char n-grams at the shipped window
 *   grams45     the same fit at a different window — a different
 *               labeler, not a worse one: it makes different mistakes
 *   words       a word TF-IDF head
 *   prefix      the gram model over the message's first clause only
 *
 * `Agreement.estimate` never sees a label: it weighs each labeler by
 * how often it matches the weighted consensus of the others, on the
 * HELD-OUT messages themselves. That is the property that matters for
 * the programme — the weights come from unlabelled traffic, so a
 * deployment can re-estimate them on its own log.
 *
 * The criterion was in the claim before any number: beat the cascade
 * on the autonomy curve at equal coverage, without breaking the
 * per-class law. The suite prints the comparison; the assertions are
 * that every labeler ran and that the estimator saw no gold labels.
 */
class MeasureLabelModel extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val train: Seq[(String, String)] =
    IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
  private val heldOut: Seq[(String, String)] =
    IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

  // ---- the labelers, all offline, all fitted on the same half ----
  private lazy val induced = Induced.train(train)
  private lazy val grams23 = CharGrams.train(train, dim = 4096, low = 2, high = 3)
  private lazy val grams45 = CharGrams.train(train, dim = 4096, low = 4, high = 5)
  private lazy val words = WordTfIdf.train(train)

  private def firstClause(m: String): String =
    val cut = m.indexWhere(c => c == ',' || c == '.' || c == ';' || c == '?')
    if cut > 8 then m.take(cut) else m

  private def votesFor(m: String): Vector[Agreement.Vote] =
    val out = Vector.newBuilder[Agreement.Vote]
    Patterns.score(Models.cues, m).foreach(v => out += Agreement.Vote("cues", v.best, v.score.min(1.0)))
    Induced.score(induced, m).foreach(v => out += Agreement.Vote("induced", v.best, v.score.min(1.0)))
    CharGrams.score(grams23, m).foreach(v => out += Agreement.Vote("grams23", v.best, v.probability))
    CharGrams.score(grams45, m).foreach(v => out += Agreement.Vote("grams45", v.best, v.probability))
    WordTfIdf.score(words, m).foreach(v => out += Agreement.Vote("words", v.best, v.probability))
    CharGrams.score(grams23, firstClause(m)).foreach(v => out += Agreement.Vote("prefix", v.best, v.probability))
    out.result()

  /** the shipped door: the first tier that fires */
  private def cascade(m: String): Option[String] =
    Patterns.classify(Models.cues, m, floor = 0.4)
      .orElse(CharGrams.score(Models.meeting, m).map(_.best))

  /** prints the row and answers the triple; most callers only want the
   * print, so `show` is the one that discards it — an unused value
   * warning at twelve call sites is noise, and silencing it at each
   * would be worse than naming the intent once */
  private def show(name: String, door: String => Option[String]): Unit =
    report(name, door): Unit

  private def report(name: String, door: String => Option[String]): (Double, Double, Double) =
    val answers = heldOut.map((m, gold) => (gold, door(m)))
    val answered = answers.count(_._2.isDefined)
    val right = answers.count((gold, got) => got.contains(gold))
    val cov = 100.0 * answered / heldOut.size
    val prec = if answered == 0 then 0.0 else 100.0 * right / answered
    val c = answers.foldLeft(Eval.Confusion()) { case (acc, (gold, got)) =>
      got.fold(acc)(p => acc.observe(gold, p)) }
    val r = Eval.report(c)
    val worst = IntentFixture.classes.flatMap(cls => r.perClass.get(cls).map(_.f1)).minOption.getOrElse(0.0)
    println(f"| $name | $cov%.1f%% | $prec%.1f%% | ${100.0 * right / heldOut.size}%.1f%% | $worst%.2f |")
    (cov, prec, worst)

  test("the weights, learned from agreement on unlabelled held-out messages") {
    val votes = heldOut.map((m, _) => votesFor(m))
    val w = Agreement.estimate(votes)
    println(s"\n[weights from agreement alone, no labels seen]\n${w.show}\nconverged in ${w.passes} passes")
    // every labeler must actually have voted somewhere, or the
    // comparison below is measuring a labeler that does not exist
    val spoke = votes.flatten.map(_.labeler).distinct.sorted
    assertEquals(spoke, List("cues", "grams23", "grams45", "induced", "prefix", "words"))
    // and the estimator's input carries no gold label anywhere
    assert(votes.flatten.forall(v => IntentFixture.classes.contains(v.intent)))
  }

  test("agreement against the cascade, on the autonomy curve") {
    val votes = heldOut.map((m, _) => votesFor(m))
    val w = Agreement.estimate(votes)
    println("\n| door | coverage | precision | right of all | worst class F1 |")
    println("|---|---:|---:|---:|---:|")
    val base = report("cascade (shipped)", cascade)
    show("agreement, no floor", m => Agreement.decide(votesFor(m), w).map(_._1))
    for floor <- List(0.05, 0.1, 0.2, 0.3) do
      show(f"agreement, margin >= $floor%.2f",
        m => Agreement.decide(votesFor(m), w).filter(_._2 >= floor).map(_._1))
    // the same, with the cues' answer trusted outright (they are the
    // precision tier, and the door has always treated them so)
    show("cues first, then agreement",
      m => Patterns.classify(Models.cues, m, floor = 0.4)
        .orElse(Agreement.decide(votesFor(m), w).map(_._1)))
    assert(base._1 > 0.0)
  }

  /** the votes of a NAMED subset, for the independence question */
  private def votesOf(m: String, keep: Set[String]): Vector[Agreement.Vote] =
    votesFor(m).filter(v => keep.contains(v.labeler))

  /** weights from the TRAIN half's labels — not label-free, but we DO
   * have that half, and a precision-weighted vote is the baseline any
   * agreement estimator must beat before it earns its subtlety */
  private lazy val byPrecision: Agreement.Weights =
    val names = train.flatMap((m, _) => votesFor(m).map(_.labeler)).distinct
    val w = names.map { n =>
      val rows = train.flatMap((m, gold) => votesFor(m).find(_.labeler == n).map(v => (v.intent, gold)))
      val acc = if rows.isEmpty then Agreement.floor else rows.count((got, gold) => got == gold).toDouble / rows.size
      n -> math.max(Agreement.floor, acc)
    }.toMap
    Agreement.Weights(w, 0)

  test("is the loss correlation? drop the labelers that are the same model twice") {
    // prefix IS grams23 on a substring, and grams45 is the same family
    // at another window: three of six votes come from one model, so a
    // majority they form is not evidence, it is an echo
    val independent = Set("cues", "induced", "grams23", "words")
    val votes = heldOut.map((m, _) => votesOf(m, independent))
    val w = Agreement.estimate(votes)
    println(s"\n[weights, independent subset]\n${w.show}")
    println("\n| door | coverage | precision | right of all | worst class F1 |")
    println("|---|---:|---:|---:|---:|")
    show("cascade (shipped)", cascade)
    show("agreement over 4 independent", m => Agreement.decide(votesOf(m, independent), w).map(_._1))
    for floor <- List(0.1, 0.2) do
      show(f"the same, margin >= $floor%.2f",
        m => Agreement.decide(votesOf(m, independent), w).filter(_._2 >= floor).map(_._1))
    assert(w.byLabeler.size == 4)
  }

  test("and against the honest baseline: weights from the train half's own labels") {
    println(s"\n[weights from labelled train half]\n${byPrecision.show}")
    println("\n| door | coverage | precision | right of all | worst class F1 |")
    println("|---|---:|---:|---:|---:|")
    show("cascade (shipped)", cascade)
    show("precision-weighted, all six", m => Agreement.decide(votesFor(m), byPrecision).map(_._1))
    val independent = Set("cues", "induced", "grams23", "words")
    show("precision-weighted, independent 4", m => Agreement.decide(votesOf(m, independent), byPrecision).map(_._1))
    for floor <- List(0.1, 0.2, 0.3) do
      show(f"precision-weighted 4, margin >= $floor%.2f",
        m => Agreement.decide(votesOf(m, independent), byPrecision).filter(_._2 >= floor).map(_._1))
    assert(byPrecision.byLabeler.nonEmpty)
  }

  /**
   * 60 held-out messages cannot tell three points from noise, and the
   * gap this lane is arguing about is three points. Eight random
   * splits, every labeler REFITTED on each split's train half (the
   * shipped artifact cannot be scored on a fresh split — its rows
   * would leak), agreement weights re-estimated from that split's
   * unlabelled held-out votes.
   */
  test("does the difference survive resampling") {
    val rows = IntentFixture.labelled
    // the labelers rebuilt per split ARE the independent four; the
    // echoes (grams45, prefix) are simply not built here
    val runs = (1 to 8).map { seed =>
      val shuffled = scala.util.Random(seed.toLong * 7919).shuffle(rows)
      val (tr, te) = shuffled.splitAt(rows.length / 2)
      val ind = Induced.train(tr)
      val g23 = CharGrams.train(tr, dim = 4096, low = 2, high = 3)
      val wds = WordTfIdf.train(tr)
      def votes(m: String): Vector[Agreement.Vote] =
        val out = Vector.newBuilder[Agreement.Vote]
        Patterns.score(Models.cues, m).foreach(v => out += Agreement.Vote("cues", v.best, v.score.min(1.0)))
        Induced.score(ind, m).foreach(v => out += Agreement.Vote("induced", v.best, v.score.min(1.0)))
        CharGrams.score(g23, m).foreach(v => out += Agreement.Vote("grams23", v.best, v.probability))
        WordTfIdf.score(wds, m).foreach(v => out += Agreement.Vote("words", v.best, v.probability))
        out.result()
      val w = Agreement.estimate(te.map((m, _) => votes(m)))
      def casc(m: String): Option[String] =
        Patterns.classify(Models.cues, m, floor = 0.4).orElse(CharGrams.score(g23, m).map(_.best))
      def scoreOf(door: String => Option[String]): (Double, Double, Double) =
        val ans = te.map((m, gold) => (gold, door(m)))
        val answered = ans.count(_._2.isDefined)
        val right = ans.count((gold, got) => got.contains(gold))
        val c = ans.foldLeft(Eval.Confusion()) { case (acc, (gold, got)) => got.fold(acc)(p => acc.observe(gold, p)) }
        val r = Eval.report(c)
        val worst = IntentFixture.classes.flatMap(cls => r.perClass.get(cls).map(_.f1)).minOption.getOrElse(0.0)
        (100.0 * answered / te.size, if answered == 0 then 0.0 else 100.0 * right / answered, worst)
      // the cascade at a matched coverage: its own margin floor tuned
      // to answer about as much as the agreement door does
      val agree = scoreOf(m => Agreement.decide(votes(m), w).filter(_._2 >= 0.20).map(_._1))
      val cascadeAt = scoreOf(m => Patterns.classify(Models.cues, m, floor = 0.4)
        .orElse(CharGrams.score(g23, m).filter(_.margin >= 0.20).map(_.best)))
      val cascadeFull = scoreOf(casc)
      (cascadeFull, cascadeAt, agree)
    }
    def mean(xs: Seq[Double]) = xs.sum / xs.size
    def sd(xs: Seq[Double]) = { val m = mean(xs); math.sqrt(xs.map(x => (x - m) * (x - m)).sum / xs.size) }
    println("\n[8 random splits, everything refitted per split]")
    println("| door | coverage | precision | worst class F1 |")
    println("|---|---:|---:|---:|")
    for (name, pick) <- List[(String, ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double)) => (Double, Double, Double))](
      ("cascade, full coverage", (a, _, _) => a),
      ("cascade, grams margin 0.20", (_, b, _) => b),
      ("agreement over 4, margin 0.20", (_, _, c) => c)) do
      val got = runs.map(r => pick(r._1, r._2, r._3))
      println(f"| $name | ${mean(got.map(_._1))}%.1f%% (sd ${sd(got.map(_._1))}%.1f) | ${mean(got.map(_._2))}%.1f%% (sd ${sd(got.map(_._2))}%.1f) | ${mean(got.map(_._3))}%.2f |")
    val wins = runs.count(r => r._3._2 > r._2._2)
    println(s"\nagreement beats the matched cascade on $wins of 8 splits (precision among answered)")
    assert(runs.length == 8)
  }

  test("what each labeler is worth alone, so the combination is judged against its parts") {
    println("\n| labeler alone | coverage | precision | right of all | worst class F1 |")
    println("|---|---:|---:|---:|---:|")
    show("cues", m => Patterns.classify(Models.cues, m, floor = 0.4))
    show("induced", m => Induced.classify(induced, m, floor = 0.0))
    show("grams23", m => CharGrams.score(grams23, m).map(_.best))
    show("grams45", m => CharGrams.score(grams45, m).map(_.best))
    show("words", m => WordTfIdf.score(words, m).map(_.best))
    show("prefix", m => CharGrams.score(grams23, firstClause(m)).map(_.best))
    assert(heldOut.nonEmpty)
  }
}
