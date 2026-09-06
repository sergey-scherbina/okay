package okay.intent

/**
 * The n-gram window against the per-class law, at each hash width
 * (specs/intent-classify.md, intent-window-by-dim).
 *
 * intent-typo-robustness found that a narrower window survives a typo
 * and, at the shipped width, takes `Other` under the 0.50 F1 floor
 * `TestModels` asserts while the total rises. The one question it
 * left: does the narrower window keep every class at the wider hash?
 * The per-class law runs on the shipped artifact only, so this suite
 * runs it for every (window, width) pair, on the same held-out half,
 * for the grams alone AND for the composite the law is stated on
 * (cues first, grams for the rest), clean and under the typo.
 *
 * Prints the table; asserts only what must hold structurally. No
 * default moves here: the number decides the next lane.
 */
class TestWindowByDim extends munit.FunSuite {

  private val (train, held) = IntentFixture.labelled.zipWithIndex
    .partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  /** one transposition in the longest word, deterministically (TestSecondAuthor's) */
  private def typo(s: String): String =
    val words = s.split(" ")
    val i = words.indices.maxBy(j => words(j).length)
    val w = words(i)
    if w.length < 4 then s
    else
      val k = w.length / 2
      words.updated(i, w.take(k - 1) + w(k) + w(k - 1) + w.drop(k + 1)).mkString(" ")

  private def confusion(rows: Seq[(String, String)], best: String => Option[String]): Eval.Confusion =
    rows.foldLeft(Eval.Confusion()) { case (m, (msg, gold)) => m.observe(gold, best(msg).getOrElse("-")) }

  private def line(name: String, m: Eval.Confusion): String =
    val r = Eval.report(m)
    val acc = m.classes.filter(_ != "-").map(c => m.count(c, c)).sum * 100.0 / m.total
    val worst = r.worst.map((c, s) => f"$c%-12s ${s.f1}%.2f").getOrElse("-")
    "  %-22s total %5.1f%%  Other F1 %.2f  worst %s  macro %.3f".format(name, acc, r.f1("Other"), worst, r.macroF1)

  test("per-class F1 by window and width, grams alone and behind the cues, clean and under a typo") {
    val typod = held.map((m, g) => (typo(m), g))
    println(f"\n[window-by-dim] ${train.length} train / ${held.length} held out; the law: every class F1 >= 0.50")
    for dim <- Vector(1024, 4096); (low, high) <- Vector((3, 5), (2, 4), (2, 3)) do
      val g = CharGrams.train(train, dim = dim, low = low, high = high)
      val alone: String => Option[String] = msg => CharGrams.score(g, msg).map(_.best)
      val composite: String => Option[String] = msg =>
        Patterns.classify(Models.cues, msg, floor = 0.4).orElse(alone(msg))
      println(s"($low,$high) @$dim")
      println(line("grams alone, clean", confusion(held, alone)))
      println(line("grams alone, typo", confusion(typod, alone)))
      println(line("composite, clean", confusion(held, composite)))
      println(line("composite, typo", confusion(typod, composite)))
    assert(held.nonEmpty)
  }
}
