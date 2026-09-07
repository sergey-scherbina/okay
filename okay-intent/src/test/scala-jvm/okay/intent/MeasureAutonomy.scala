package okay.intent

/**
 * The two numbers the autonomy programme is judged by
 * (specs/intent-classify.md, "The autonomy programme"), printed for
 * the first time.
 *
 * "Works without a model" is not one number. It is:
 *
 *   AUTONOMY RATE     the share of messages answered with NO network
 *                     at all, and the precision among those answers
 *   HANDED-OVER SHARE the rest, which must reach a model or a person
 *
 * A door that answers everything at 80% and a door that answers 53%
 * at 91% are different products, and the aggregate this line has been
 * quoting for months (80.0% at full coverage) hides the choice. This
 * suite prints the curve so a caller can read off the answer to the
 * only question they actually ask: "at the precision I need, how much
 * can I do without a network?"
 *
 * Offline, no gateway, no fitting at startup — the shipped tiers as a
 * caller gets them. The assertions are structural (the tiers cover
 * the fixture, the shipped numbers are still what the docs claim);
 * the curve itself is printed and recorded in the spec, never
 * asserted as a threshold.
 */
class MeasureAutonomy extends munit.FunSuite {

  private val heldOut: Seq[(String, String)] =
    IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

  private val rows = scala.collection.mutable.ArrayBuffer.empty[String]
  private def line(s: String): Unit = { rows += s; println(s) }

  /** answered / right / by-class, for a door that may decline */
  private def measure(name: String, door: String => Option[String]): (Double, Double) =
    val answers = heldOut.map((m, gold) => (gold, door(m)))
    val answered = answers.count(_._2.isDefined)
    val right = answers.count((gold, got) => got.contains(gold))
    val coverage = 100.0 * answered / heldOut.size
    val precision = if answered == 0 then 0.0 else 100.0 * right / answered
    val recallOfAll = 100.0 * right / heldOut.size
    line(f"| $name | $coverage%.1f%% | $precision%.1f%% | $recallOfAll%.1f%% | ${100.0 - coverage}%.1f%% |")
    (coverage, precision)

  test("the autonomy curve: what the network-free door answers, and how well") {
    line("\n### autonomy, held-out English (60 messages), no network at all")
    line("| door | coverage | precision among answered | right of all | handed over |")
    line("|---|---:|---:|---:|---:|")

    // tier 1: cues alone — needs nothing at run time, not even the artifact
    val cuesOnly = measure("cues only (floor 0.4)",
      m => Patterns.classify(Models.cues, m, floor = 0.4))

    // tier 2: the shipped gram model alone, at several abstention floors
    for floor <- List(0.0, 0.1, 0.2, 0.3, 0.5) do
      measure(f"grams only, margin >= $floor%.1f",
        m => CharGrams.score(Models.meeting, m).filter(_.margin >= floor).map(_.best))

    // the shipped composite: cues first, grams behind them
    for floor <- List(0.0, 0.1, 0.2, 0.3, 0.5) do
      measure(f"cues, then grams margin >= $floor%.1f",
        m => Patterns.classify(Models.cues, m, floor = 0.4)
          .orElse(CharGrams.score(Models.meeting, m).filter(_.margin >= floor).map(_.best)))

    assert(cuesOnly._1 > 0.0 && cuesOnly._2 > 0.0)
  }

  test("what a caller can promise: the best coverage at each precision they might need") {
    val doors: List[(String, String => Option[String])] =
      ("cues only", (m: String) => Patterns.classify(Models.cues, m, floor = 0.4)) ::
        List(0.0, 0.1, 0.2, 0.3, 0.5).map { fl =>
          (f"cues+grams >= $fl%.1f",
            (m: String) => Patterns.classify(Models.cues, m, floor = 0.4)
              .orElse(CharGrams.score(Models.meeting, m).filter(_.margin >= fl).map(_.best)))
        }
    val measured = doors.map { (n, d) =>
      val answers = heldOut.map((m, gold) => (gold, d(m)))
      val answered = answers.count(_._2.isDefined)
      val right = answers.count((gold, got) => got.contains(gold))
      (n, 100.0 * answered / heldOut.size, if answered == 0 then 0.0 else 100.0 * right / answered)
    }
    line("\n### the promise table: at precision P, this much needs no network")
    line("| precision needed | best door | coverage | handed over |")
    line("|---|---|---:|---:|")
    for p <- List(95.0, 90.0, 85.0, 80.0) do
      measured.filter(_._3 >= p).maxByOption(_._2) match
        case Some((n, cov, prec)) =>
          line(f"| >= $p%.0f%% | $n (measured $prec%.1f%%) | $cov%.1f%% | ${100.0 - cov}%.1f%% |")
        case None =>
          line(f"| >= $p%.0f%% | none of the offline doors reaches it | 0.0%% | 100.0%% |")
    assert(measured.nonEmpty)
  }

  test("per class, because an aggregate hides the class a caller will meet") {
    val door = (m: String) => Patterns.classify(Models.cues, m, floor = 0.4)
      .orElse(CharGrams.score(Models.meeting, m).map(_.best))
    val c = heldOut.foldLeft(Eval.Confusion()) { case (acc, (m, gold)) =>
      door(m).fold(acc)(p => acc.observe(gold, p)) }
    val r = Eval.report(c)
    line("\n### the shipped offline door, per class")
    line("| class | precision | recall | F1 |")
    line("|---|---:|---:|---:|")
    for cls <- IntentFixture.classes do
      r.perClass.get(cls).foreach(s => line(f"| $cls | ${s.precision}%.2f | ${s.recall}%.2f | ${s.f1}%.2f |"))
    // the law the shipped model is held to, restated here so the
    // autonomy report fails if a refit ever breaks it
    val worst = IntentFixture.classes.flatMap(cls => r.perClass.get(cls).map(_.f1)).min
    assert(worst >= 0.50, f"a class fell below the shipped-model law: $worst%.2f")
  }

  override def afterAll(): Unit =
    println("\n--- copy into specs/intent-classify.md, 'The autonomy programme' ---")
    rows.foreach(println)
}
