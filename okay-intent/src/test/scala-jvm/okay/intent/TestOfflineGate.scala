package okay.intent

/**
 * The offline door cannot say "none of these"
 * (intent-offline-other). Its worst number by far is `Other`: recall
 * 0.40 at F1 0.52, meaning more than half of the traffic that is not
 * about meetings lands in a meeting class with a confident face.
 *
 * The MODEL path closed that same hole twice, and the way it did is
 * the hypothesis here: what worked was not a better four-way answer,
 * it was a SEPARATE BINARY QUESTION asked first (specs/intent-classify.md,
 * the gate). Asked to choose among positive classes a classifier
 * chooses one; asked "is this in the domain at all" it is not offered
 * that choice. The offline door has never had that step. A margin
 * floor on the four-way model was tried and measured not to separate
 * (Router.Floors' own comment), which is a different mechanism and
 * not evidence against this one.
 *
 * So: a second `CharGrams` model fitted on the BINARY task over the
 * same rows, run before the four-way tier. It needs no network and no
 * new corpus — the labels are already there, folded to in/out.
 *
 * The criterion was stated in the claim before the first number was
 * seen: Other recall >= 0.60, total accuracy down no more than one
 * point, every class F1 >= 0.50. This suite prints the comparison and
 * asserts the criterion ONLY as a recorded verdict, never as a
 * threshold on a model someone might refit — the assertions here are
 * that the experiment ran on disjoint data and that the shipped door
 * is unchanged by it.
 */
class TestOfflineGate extends munit.FunSuite {

  /** the same split the shipped model uses: even rows held out, odd
   * rows fitted — so the gate is trained on exactly what the four-way
   * model was trained on, and scored on what it was scored on */
  private val train: Seq[(String, String)] =
    IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
  private val heldOut: Seq[(String, String)] =
    IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

  private val IN = "InDomain"
  private val OUT = "Other"

  /** the binary view of the same rows: three positive classes fold to
   * one, `Other` stays itself */
  private def binary(rows: Seq[(String, String)]): Seq[(String, String)] =
    rows.map((m, gold) => (m, if gold == "Other" then OUT else IN))

  private lazy val gate: CharGrams.Trained = CharGrams.train(binary(train))

  /** the same question, with the classes BALANCED: the imbalanced fit
   * collapsed to the majority answer, which is the textbook failure
   * for a rare class and worth separating from "the representation
   * cannot see it" */
  private lazy val balanced: CharGrams.Trained =
    val rows = binary(train)
    val out = rows.filter(_._2 == OUT)
    val in = rows.filter(_._2 == IN).take(out.length)
    CharGrams.train(in ++ out)

  /** p(OUT), whether or not OUT wins: a rare class can rank its own
   * rows first while never being the argmax, and that is a usable
   * detector where an argmax is not */
  private def pOut(t: CharGrams.Trained, m: String): Double =
    CharGrams.score(t, m) match
      case Some(v) if v.best == OUT => v.probability
      case Some(v) if v.runnerUp.contains(OUT) => v.probability - v.margin
      case _ => 0.0

  private def fourWay(m: String): Option[String] =
    Patterns.classify(Models.cues, m, floor = 0.4)
      .orElse(CharGrams.score(Models.meeting, m).map(_.best))

  /** the door as shipped */
  private def shipped(m: String): Option[String] = fourWay(m)

  /** the door with the gate in front, by ARGMAX — the first shape
   * tried, kept because its failure is the finding */
  private def gated(floor: Double)(m: String): Option[String] =
    CharGrams.score(gate, m) match
      case Some(v) if v.best == OUT && v.margin >= floor => Some("Other")
      case _ => fourWay(m)

  /** the door with the gate in front, by THRESHOLD on p(OUT): the
   * argmax cannot express a class it never wins, but the score can
   * rank it (AUC 0.843 on the same rows), and a threshold is how a
   * ranking becomes a decision */
  private def gatedBy(t: Double)(m: String): Option[String] =
    if pOut(gate, m) >= t then Some("Other") else fourWay(m)

  private def confusionOf(f: String => Option[String]): Eval.Confusion =
    heldOut.foldLeft(Eval.Confusion()) { case (acc, (msg, gold)) =>
      f(msg).fold(acc)(pred => acc.observe(gold, pred))
    }

  private def report(name: String, f: String => Option[String]): (Double, Double, Double) =
    val c = confusionOf(f)
    val r = Eval.report(c)
    val right = heldOut.count((m, gold) => f(m).contains(gold))
    val total = 100.0 * right / heldOut.size
    val other = r.perClass.get("Other")
    println(f"\n[$name] total $total%.1f%%   macro F1 ${r.macroF1}%.3f")
    for cls <- IntentFixture.classes do
      r.perClass.get(cls).foreach(s =>
        println(f"  $cls%-13s P=${s.precision}%.2f R=${s.recall}%.2f F1=${s.f1}%.2f"))
    val worst = IntentFixture.classes.flatMap(cls => r.perClass.get(cls).map(_.f1)).minOption.getOrElse(0.0)
    (total, other.map(_.recall).getOrElse(0.0), worst)

  test("the gate is fitted and scored on disjoint halves, like the shipped model") {
    assert(train.map(_._1).toSet.intersect(heldOut.map(_._1).toSet).isEmpty)
    assertEquals(train.size, 60)
    assertEquals(heldOut.size, 60)
    // the binary task is not degenerate: both answers are represented
    val counts = binary(train).groupBy(_._2).view.mapValues(_.size).toMap
    assert(counts.getOrElse(IN, 0) >= 30 && counts.getOrElse(OUT, 0) >= 10, counts.toString)
  }

  test("the binary question on its own: can a gram model tell in-domain from not") {
    val b = binary(heldOut)
    val right = b.count((m, gold) => CharGrams.score(gate, m).exists(_.best == gold))
    val outRows = b.filter(_._2 == OUT)
    val outHit = outRows.count((m, _) => CharGrams.score(gate, m).exists(_.best == OUT))
    println(f"\n[gate alone] ${100.0 * right / b.size}%.1f%% binary accuracy over ${b.size} messages")
    println(f"  out-of-domain recall ${100.0 * outHit / outRows.size}%.1f%% over ${outRows.size} rows")
    assert(right > 0)
  }

  test("is it the imbalance or the representation") {
    val b = binary(heldOut)
    val outRows = b.filter(_._2 == OUT)
    for (name, t) <- List("imbalanced" -> gate, "balanced 15/15" -> balanced) do
      val right = b.count((m, gold) => CharGrams.score(t, m).exists(_.best == gold))
      val outHit = outRows.count((m, _) => CharGrams.score(t, m).exists(_.best == OUT))
      println(f"\n[$name] binary accuracy ${100.0 * right / b.size}%.1f%%, " +
              f"OUT recall ${100.0 * outHit / outRows.size}%.1f%% over ${outRows.size} rows")
      // and the ranking question: does p(OUT) separate at all, argmax aside
      val scored = b.map((m, gold) => (pOut(t, m), gold == OUT))
      val ranked = scored.sortBy(-_._1)
      val top15 = ranked.take(outRows.size).count(_._2)
      println(f"  p(OUT) ranking: ${top15}/${outRows.size} of the true out-of-domain rows in the top ${outRows.size}")
      val auc =
        val pos = scored.filter(_._2).map(_._1)
        val neg = scored.filterNot(_._2).map(_._1)
        if pos.isEmpty || neg.isEmpty then 0.0
        else pos.map(pv => neg.count(nv => pv > nv) + 0.5 * neg.count(nv => pv == nv)).sum / (pos.size * neg.size)
      println(f"  p(OUT) AUC $auc%.3f   (0.5 is chance)")
    assert(outRows.nonEmpty)
  }

  /**
   * One split of 60 held-out messages cannot tell a one-message
   * difference from noise, and the gain the sweep showed at 0.20-0.30
   * IS one message. So the same comparison over several random
   * splits, refitting BOTH models each time — the four-way one too,
   * because scoring the shipped artifact against a new split would
   * leak its training rows.
   */
  test("does the gain survive resampling, or is it one message") {
    val rows = IntentFixture.labelled
    val thresholds = List(0.30, 0.20, 0.10, 0.05)
    val runs = (1 to 8).map { seed =>
      val shuffled = scala.util.Random(seed.toLong * 7919).shuffle(rows)
      val (tr, te) = shuffled.splitAt(rows.length / 2)
      val four = CharGrams.train(tr)
      val g = CharGrams.train(binary(tr))
      def four4(m: String): Option[String] =
        Patterns.classify(Models.cues, m, floor = 0.4).orElse(CharGrams.score(four, m).map(_.best))
      def scoreOf(f: String => Option[String]): (Double, Double) =
        val right = te.count((m, gold) => f(m).contains(gold))
        val outRows = te.filter(_._2 == "Other")
        val outHit = outRows.count((m, _) => f(m).contains("Other"))
        (100.0 * right / te.size, if outRows.isEmpty then 0.0 else outHit.toDouble / outRows.size)
      val base = scoreOf(four4)
      val gatedAt = thresholds.map { t =>
        def door(m: String): Option[String] =
          if pOut(g, m) >= t then Some("Other") else four4(m)
        t -> scoreOf(door)
      }
      (base, gatedAt)
    }
    def mean(xs: Seq[Double]): Double = xs.sum / xs.size
    def sd(xs: Seq[Double]): Double =
      val m = mean(xs); math.sqrt(xs.map(x => (x - m) * (x - m)).sum / xs.size)
    val baseTotals = runs.map(_._1._1)
    val baseOthers = runs.map(_._1._2)
    println(f"\n[8 random splits, both tiers refitted each time]")
    println(f"  no gate        total ${mean(baseTotals)}%.1f%% (sd ${sd(baseTotals)}%.1f)   Other recall ${mean(baseOthers)}%.2f (sd ${sd(baseOthers)}%.2f)")
    for t <- thresholds do
      val tot = runs.map(_._2.toMap.apply(t)._1)
      val oth = runs.map(_._2.toMap.apply(t)._2)
      val wins = runs.count(r => r._2.toMap.apply(t)._1 > r._1._1)
      println(f"  gate @ $t%.2f    total ${mean(tot)}%.1f%% (sd ${sd(tot)}%.1f)   Other recall ${mean(oth)}%.2f (sd ${sd(oth)}%.2f)   " +
              f"better on $wins%d/8 splits   delta ${mean(tot) - mean(baseTotals)}%+.1f pt")
    assert(runs.length == 8)
  }

  test("the criterion, decided on the numbers: Other recall >= 0.60, total within 1 point, no class below F1 0.50") {
    val (baseTotal, baseOther, baseWorst) = report("shipped (cues + 4-way)", shipped)
    val results = List(0.0, 0.2).map { floor =>
      val (t, o, w) = report(f"gated by argmax, margin floor $floor%.2f", gated(floor))
      (floor, t, o, w)
    } ++ List(0.40, 0.35, 0.30, 0.25, 0.20, 0.15, 0.10, 0.05).map { th =>
      val (t, o, w) = report(f"gated by p(OUT) >= $th%.2f", gatedBy(th))
      (th, t, o, w)
    }
    println("\n| door | total | Other recall | worst class F1 |")
    println("|---|---:|---:|---:|")
    println(f"| shipped | $baseTotal%.1f%% | $baseOther%.2f | $baseWorst%.2f |")
    results.foreach((fl, t, o, w) => println(f"| gate @ $fl%.2f | $t%.1f%% | $o%.2f | $w%.2f |"))
    val passing = results.filter((_, t, o, w) => o >= 0.60 && t >= baseTotal - 1.0 && w >= 0.50)
    println(if passing.isEmpty then "\nVERDICT: no floor meets the criterion — the offline gate is DECLINED"
            else f"\nVERDICT: ${passing.length} floor(s) meet it; best ${passing.maxBy(_._2)}")
    // ours to assert: the experiment ran over the whole held-out set
    // and the shipped door is untouched by it
    assertEquals(heldOut.count((m, _) => shipped(m).isDefined), 60)
    assertEquals(baseTotal, 80.0, "the shipped door still scores what the doc claims")
  }
}
