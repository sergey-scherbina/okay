package okay.intent

/**
 * Splitting `Other` into names it can be learned under
 * (specs/intent-classify.md).
 *
 * `intent-other-is-a-bin` settled two things and left one open:
 * `Other` is ONE incoherent cloud rather than two clusters, and
 * abstaining instead of learning it costs twenty points — but an
 * incoherent bin can still be learned when its members are
 * individually distinctive, which is an argument for NAMES rather
 * than for a threshold.
 *
 * The number that decides it is not overall accuracy, which a split
 * can move for uninteresting reasons. It is UNION RECALL: of the
 * held-out messages that are not about meetings, how many land in a
 * non-meeting class AT ALL. In the shipped composite that is 0.47,
 * which means more than half of them are routed into a meeting
 * intent.
 */
class TestSplitOther extends munit.FunSuite {

  private def halves[A](rows: List[A]) =
    rows.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  private val (trainFlat, testFlat) = halves(IntentFixture.labelled)
  private val (trainSplit, testSplit) = halves(IntentFixture.labelledSplit)

  private def fit(rows: List[(String, String)]) = CharGrams.train(rows, dim = 1024)

  private def predict(t: CharGrams.Trained, m: String): Option[String] =
    CharGrams.score(t, m).map(_.best)

  test("split against unsplit, folded back to the same question") {
    val flat = fit(trainFlat)
    val split = fit(trainSplit)

    def accuracy(f: String => Option[String]) =
      testFlat.count((m, gold) => f(m).contains(gold)).toDouble / testFlat.size

    val flatAcc = accuracy(m => predict(flat, m))
    val splitAcc = accuracy(m => predict(split, m).map(IntentFixture.unsplit))

    // the number the lane exists for
    def unionRecall(f: String => Option[String]) =
      val others = testFlat.filter(_._2 == "Other")
      others.count((m, _) => f(m).contains("Other")).toDouble / others.size

    val flatRec = unionRecall(m => predict(flat, m))
    val splitRec = unionRecall(m => predict(split, m).map(IntentFixture.unsplit))

    println(f"[fold] unsplit  accuracy ${100 * flatAcc}%5.1f%%  Other recall ${100 * flatRec}%5.1f%%")
    println(f"[fold] split    accuracy ${100 * splitAcc}%5.1f%%  Other recall ${100 * splitRec}%5.1f%%")

    val r = Eval.report(testSplit.foldLeft(Eval.Confusion()) { case (acc, (m, gold)) =>
      predict(split, m).fold(acc)(p => acc.observe(gold, p))
    })
    r.perClass.toVector.sortBy(_._1).foreach((c, sc) =>
      println(f"[split] $c%-13s P ${sc.precision}%.2f  R ${sc.recall}%.2f  F1 ${sc.f1}%.2f"))
  }

  test("the same question through the shipped composite, cues and all") {
    // the tier order a caller actually gets, with the model refitted
    // both ways underneath it
    val flat = fit(trainFlat)
    val split = fit(trainSplit)

    def composite(t: CharGrams.Trained, fold: Boolean)(m: String): Option[String] =
      Patterns.classify(Models.cues, m, floor = 0.4)
        .orElse(predict(t, m).map(p => if fold then IntentFixture.unsplit(p) else p))

    val others = testFlat.filter(_._2 == "Other")
    def recall(f: String => Option[String]) =
      others.count((m, _) => f(m).contains("Other")).toDouble / others.size
    def acc(f: String => Option[String]) =
      testFlat.count((m, g) => f(m).contains(g)).toDouble / testFlat.size

    val a0 = acc(composite(flat, fold = false))
    val r0 = recall(composite(flat, fold = false))
    val a1 = acc(composite(split, fold = true))
    val r1 = recall(composite(split, fold = true))
    println(f"[composite] unsplit ${100 * a0}%5.1f%%  Other recall ${100 * r0}%5.1f%%")
    println(f"[composite] split   ${100 * a1}%5.1f%%  Other recall ${100 * r1}%5.1f%%")
    println(f"[composite] delta   ${100 * (a1 - a0)}%+5.1f pt accuracy, ${100 * (r1 - r0)}%+5.1f pt recall")
  }

  test("which messages the split rescues, and which it loses") {
    val flat = fit(trainFlat)
    val split = fit(trainSplit)
    def one(t: CharGrams.Trained, fold: Boolean)(m: String) =
      Patterns.classify(Models.cues, m, floor = 0.4)
        .orElse(predict(t, m).map(p => if fold then IntentFixture.unsplit(p) else p))
    testFlat.foreach { (m, gold) =>
      val a = one(flat, false)(m).contains(gold)
      val b = one(split, true)(m).contains(gold)
      if a != b then
        println(s"[${if b then "rescued" else "lost   "}] [$gold] $m")
    }
  }

  test("a coarser split, in case three names was simply too many") {
    // Social (11 rows) against everything else in the bin (19), so
    // each new class has roughly twice what the three-way gave them.
    // If this collapses too, the finding is about ROW COUNT and not
    // about how the bin is carved.
    def coarse(l: String, m: String) =
      if l != "Other" then l
      else if IntentFixture.otherGroup(m) == "Social" then "Social" else "Trouble"
    val two = fit(trainFlat.map((m, l) => m -> coarse(l, m)))
    val back = (p: String) => if p == "Social" || p == "Trouble" then "Other" else p

    val others = testFlat.filter(_._2 == "Other")
    val recall = others.count((m, _) => predict(two, m).map(back).contains("Other")).toDouble / others.size
    val acc = testFlat.count((m, g) => predict(two, m).map(back).contains(g)).toDouble / testFlat.size
    val produced = testFlat.flatMap((m, _) => predict(two, m)).distinct.sorted
    println(f"[two-way] accuracy ${100 * acc}%5.1f%%  Other recall ${100 * recall}%5.1f%%")
    println(s"[two-way] classes it ever predicts: ${produced.mkString(", ")}")
  }

  test("how many rows each class has, which is the number that explains the rest") {
    val flat = trainFlat.groupBy(_._2).view.mapValues(_.size).toMap
    val split = trainSplit.groupBy(_._2).view.mapValues(_.size).toMap
    println(s"[rows] unsplit ${flat.toVector.sortBy(_._1).mkString(", ")}")
    println(s"[rows] split   ${split.toVector.sortBy(_._1).mkString(", ")}")
  }
}

