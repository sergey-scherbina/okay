package okay.intent

/**
 * Ukrainian and Polish, and what the shipped tiers do with them
 * (specs/intent-classify.md).
 *
 * Owed to a consumer who runs in both and asked twice. The point is
 * not to claim the tiers work there — one of them is fitted on 60
 * English messages — it is to give a native-language caller NUMBERS
 * and ROWS they can correct, which is the only useful thing an
 * author-written fixture can be for a language its author does not
 * speak natively.
 *
 * `CharGrams` is the tier this actually tests: it claims to be
 * language-agnostic BY CONSTRUCTION, and until now it had seen one
 * Cyrillic language and no Latin-script Slavic one at all.
 */
class TestSlavicRows extends munit.FunSuite {

  private def split(rows: List[(String, String)]) =
    rows.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  test("every meaning is present in all eight languages") {
    val holes = IntentFixture.parallel.flatMap(p =>
      IntentFixture.languages.filterNot(p.byLang.contains).map(l => s"${p.id}/$l"))
    assertEquals(holes, Nil)
    assertEquals(IntentFixture.inLanguage("uk").size, 30)
    assertEquals(IntentFixture.inLanguage("pl").size, 30)
  }

  test("the shipped model on eight languages, one row each") {
    // it is fitted on English alone, so most of this is a floor
    // rather than a result — 25% is what guessing one class gets
    IntentFixture.languages.foreach { lang =>
      val rows = IntentFixture.inLanguage(lang)
      val hit = rows.count((m, g) => CharGrams.score(Models.meeting, m).exists(_.best == g))
      val cued = rows.count((m, _) => Patterns.classify(Models.cues, m, 0.4).isDefined)
      println(f"[shipped] $lang%2s  model ${100.0 * hit / rows.size}%5.1f%%  cues fired on $cued%2d of ${rows.size}%2d")
    }
  }

  test("fitted on all eight, held out per language — what the tier can do when it has seen the language") {
    val perLang = IntentFixture.languages.map(l => l -> split(IntentFixture.inLanguage(l))).toMap
    val train = split(IntentFixture.labelled)._1 ++
      IntentFixture.languages.flatMap(l => perLang(l)._1)
    val t = CharGrams.train(train, dim = 4096)
    IntentFixture.languages.foreach { lang =>
      val te = perLang(lang)._2
      val hit = te.count((m, g) => CharGrams.score(t, m).exists(_.best == g))
      val m = te.foldLeft(Eval.Confusion()) { case (acc, (msg, gold)) =>
        CharGrams.score(t, msg).fold(acc)(v => acc.observe(gold, v.best))
      }
      val worst = Eval.report(m).worst
      println(f"[fitted] $lang%2s  ${100.0 * hit / te.size}%5.1f%% of ${te.size}%2d held out" +
        f"  worst class ${worst.map(_._1).getOrElse("-")}%-12s F1 ${worst.map(_._2.f1).getOrElse(0.0)}%.2f")
    }
    // fifteen rows a language cannot carry a claim; this prints so a
    // native speaker can see WHICH rows are being missed
    val te = perLang("uk")._2 ++ perLang("pl")._2
    val wrong = te.filterNot((m, g) => CharGrams.score(t, m).exists(_.best == g))
    println(s"[review] Slavic rows the tier gets wrong, for a native speaker to check first:")
    wrong.foreach((m, g) => println(s"[review]   [$g] $m"))
  }
}
