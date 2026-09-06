package okay.intent

/**
 * What one typo costs each no-network tier, and whether a smaller
 * n-gram buys it back (specs/intent-classify.md,
 * intent-typo-robustness).
 *
 * The claim under test: character n-grams survive a typo because a
 * word's other windows still match. On 2026-09-04 one transposition
 * in the longest word took the hashed 3–5-gram model from 61.7% to
 * 55.0%, and the entry named two fixes to measure rather than assume
 * — a smaller n, more rows. This suite measures the first on today's
 * fixture (the rows already grew), with the word TF-IDF tier beside
 * it as the control that SHOULD collapse: a transposed word is a word
 * its vocabulary has never seen.
 *
 * Same split as `TestCharGrams` and `TestWordTfIdf` (odd rows train,
 * even rows test); the typo is `TestSecondAuthor`'s, deterministic.
 * Prints a table; asserts only what must hold structurally.
 */
class TestTypoRobustness extends munit.FunSuite {

  private val (train, test) = IntentFixture.labelled.zipWithIndex
    .partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  /** one transposition in the longest word, deterministically */
  private def typo(s: String): String =
    val words = s.split(" ")
    val i = words.indices.maxBy(j => words(j).length)
    val w = words(i)
    if w.length < 4 then s
    else
      val k = w.length / 2
      words.updated(i, w.take(k - 1) + w(k) + w(k - 1) + w.drop(k + 1)).mkString(" ")

  private def accuracy(rows: Seq[(String, String)], best: String => Option[String]): Double =
    rows.count((m, g) => best(m).contains(g)) * 100.0 / rows.length

  test("the typo changes every message with a word of four letters or more, and nothing else") {
    val changed = test.count((m, _) => typo(m) != m)
    assert(changed > test.length / 2, s"only $changed of ${test.length} messages changed")
    assertEquals(typo("send the agenda"), "send the agneda")
    assertEquals(typo("ok go"), "ok go")
  }

  test("a typo's cost per tier, and per n-gram width") {
    val typod = test.map((m, g) => (typo(m), g))
    println(f"\n[typo] ${train.length} train / ${test.length} test; one transposition in the longest word")
    println("  %-26s %8s %8s %6s".format("tier", "clean", "typo", "drop"))
    val grams = Vector((3, 5), (2, 4), (2, 3), (3, 4), (4, 6))
    // at both widths, because they interact: CharGrams' own default
    // is 4096 and `Fit`'s (the shipped model's) is 1024
    for dim <- Vector(4096, 1024); (low, high) <- grams do
      val m = CharGrams.train(train, dim = dim, low = low, high = high)
      val clean = accuracy(test, t => CharGrams.score(m, t).map(_.best))
      val hit = accuracy(typod, t => CharGrams.score(m, t).map(_.best))
      println("  %-26s %7.1f%% %7.1f%% %+5.1f".format(s"chargrams ($low,$high) @$dim", clean, hit, hit - clean))
    val w = WordTfIdf.train(train)
    val wClean = accuracy(test, t => WordTfIdf.score(w, t).map(_.best))
    val wHit = accuracy(typod, t => WordTfIdf.score(w, t).map(_.best))
    println("  %-26s %7.1f%% %7.1f%% %+5.1f".format("word tf-idf (control)", wClean, wHit, wHit - wClean))
    assert(test.nonEmpty)
  }
}
