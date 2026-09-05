package okay.intent

/**
 * What a parallel fixture cannot tell you about itself
 * (specs/intent-classify.md).
 *
 * A native reader found two defects by reading sixty rows, and the
 * FIRST is about the fixture's shape rather than any row: their
 * earlier corrections went into the Russian arm and the same calque
 * stayed in Ukrainian and Polish, because every arm is a translation
 * of one English sentence. A bad row is eight bad rows, fixing one
 * fixes an eighth, and NOTHING SAYS SO. Their rule — review a row,
 * not a language — is the habit; this suite is the part a habit
 * cannot do.
 */
class TestFixtureHygiene extends munit.FunSuite {

  /** character trigrams, the same view the shipped tier has */
  private def grams(s: String): Set[String] =
    val t = s.toLowerCase.replaceAll("[\\p{Punct}]", " ").replaceAll("\\s+", " ").trim
    if t.length < 3 then Set(t) else (0 to t.length - 3).map(i => t.substring(i, i + 3)).toSet

  private def similarity(a: String, b: String): Double =
    val (x, y) = (grams(a), grams(b))
    if x.isEmpty && y.isEmpty then 1.0 else (x intersect y).size.toDouble / (x union y).size

  /** how many single-character edits turn one word into the other */
  private def edits(a: String, b: String): Int =
    val prev = Array.tabulate(b.length + 1)(identity)
    val cur = new Array[Int](b.length + 1)
    for i <- 1 to a.length do
      cur(0) = i
      for j <- 1 to b.length do
        cur(j) = math.min(math.min(cur(j - 1) + 1, prev(j) + 1),
          prev(j - 1) + (if a(i - 1) == b(j - 1) then 0 else 1))
      Array.copy(cur, 0, prev, 0, b.length + 1)
    prev(b.length)

  private def opening(s: String): String =
    s.toLowerCase.replaceAll("[\\p{Punct}]", " ").trim.split("\\s+").headOption.getOrElse("")

  test("two classes must not open on the same word with a letter changed") {
    // THE READER'S SECOND FINDING, as a rule. uk "Можемо зустрітися"
    // (Proposal) against "Можете перевірити" (Request) differ by two
    // letters in the morpheme that carries the class; pl "możemy"
    // against "możesz" the same. A hashed 3-5-gram cannot see a
    // contrast its own features collide on, so such a pair is not a
    // hard example, it is an unlearnable one.
    //
    // Whole-sentence similarity does NOT find this — measured: those
    // pairs score 0.047 and 0.121 by trigram Jaccard over the whole
    // message, far below any threshold worth setting. The signal is
    // in the opening word alone, which is why this rule looks at that
    // and nothing else.
    val hits = for
      lang <- IntentFixture.languages
      rows = IntentFixture.inLanguage(lang)
      (a, i) <- rows.zipWithIndex
      (b, j) <- rows.zipWithIndex
      wa = opening(a._1); wb = opening(b._1)
      if i < j && a._2 != b._2 && wa.length >= 4 && wa != wb && edits(wa, wb) <= 2
    yield f"$lang%2s  [${a._2}] $wa%s  vs  [${b._2}] $wb%s  (${edits(wa, wb)}%d edits)"
    hits.distinct.foreach(h => println(s"[collide] $h"))
    // PRINTED, NOT ASSERTED, and the reason is worth stating: "would"
    // against "could" is how ENGLISH marks the distinction, and
    // "können" against "könnten" is how German does. A fixture that
    // avoided them would be less like the language, not more. So this
    // is a diagnostic of where the TIER is structurally blind rather
    // than a rule the corpus must obey — the reader's finding
    // generalises past the two languages they were reading.
    assert(hits.nonEmpty, "if this ever empties, the corpus has stopped resembling the languages")
  }

  test("the English corpus every number is measured on, checked the same way") {
    // `labelled` is what the shipped model is fitted on and what
    // 76.7% is measured against, and the twin rule had never been
    // pointed at it. Reported rather than fixed here: changing this
    // corpus moves every published number, which is its own lane with
    // its own re-publish.
    val rows = IntentFixture.labelled
    val twins = for
      (a, i) <- rows.zipWithIndex
      (b, j) <- rows.zipWithIndex
      if i < j && a._2 == b._2 && similarity(a._1, b._1) >= 0.30
    yield f"${similarity(a._1, b._1)}%.2f  [${a._2}] ${a._1}  ||  ${b._1}"
    twins.foreach(t => println(s"[en-twin] $t"))
    println(s"[en-twin] ${twins.size} near-twin pairs in the English corpus")
  }

  test("no two rows of the same class in one language are built from one template") {
    // the template duplication the reader caught by eye. The
    // threshold is set from the pairs they named, which scored
    // 0.32-0.34 before the fix and 0.06-0.14 after.
    val twins = for
      lang <- IntentFixture.languages
      rows = IntentFixture.inLanguage(lang)
      (a, i) <- rows.zipWithIndex
      (b, j) <- rows.zipWithIndex
      if i < j && a._2 == b._2 && similarity(a._1, b._1) >= 0.30
    yield f"$lang%2s ${similarity(a._1, b._1)}%.2f  ${a._1}  ||  ${b._1}"
    twins.foreach(t => println(s"[twin] $t"))
    assert(twins.isEmpty, s"${twins.size} near-twin pairs within one class and language")
  }

  test("every arm of a meaning is a different sentence, not a copy") {
    // a translation that leaves the source in place is the other way
    // a parallel fixture goes stale unnoticed
    val copies = IntentFixture.parallel.flatMap { p =>
      val en = p.byLang("en")
      p.byLang.toVector.filter((l, m) => l != "en" && m == en).map((l, _) => s"${p.id}/$l")
    }
    assertEquals(copies, Nil)
  }

  test("what the collision costs the tier, in English, where every number is measured") {
    // "would" (Proposal) against "could" (Request) is ONE edit, and
    // it is not a fixture defect — it is how English marks the
    // distinction. Which makes it a fact about the TIER: a hashed
    // 3-5-gram sees "oul", "uld" in both and has to find the class
    // somewhere else in the sentence.
    val (train, test) = IntentFixture.labelled.zipWithIndex
      .partition(_._2 % 2 == 1) match
        case (a, b) => (a.map(_._1), b.map(_._1))
    val t = CharGrams.train(train, dim = 1024)
    def openingOf(m: String) = opening(m)
    val modal = Set("would", "could", "can", "shall", "will")
    val (onModal, rest) = test.partition((m, _) => modal.contains(openingOf(m)))
    def acc(rows: List[(String, String)]) =
      if rows.isEmpty then Double.NaN
      else 100.0 * rows.count((m, g) => CharGrams.score(t, m).exists(_.best == g)) / rows.size
    println(f"[modal] messages opening on a modal: ${onModal.size}%2d, accuracy ${acc(onModal)}%5.1f%%")
    println(f"[modal] everything else:             ${rest.size}%2d, accuracy ${acc(rest)}%5.1f%%")
    onModal.foreach { (m, gold) =>
      val got = CharGrams.score(t, m).map(_.best).getOrElse("-")
      if got != gold then println(s"[modal]   [$gold -> $got] $m")
    }
  }
}

