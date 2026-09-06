package okay.intent

/**
 * Cues induced from the corpus, measured where the hand-written ones
 * are (specs/intent-classify.md, intent-rule-induction): the
 * `TestCharGrams` split, coverage and precision where fired on the
 * held-out half, beside `Patterns.meeting` on the same rows.
 */
class TestRuleInduction extends munit.FunSuite {

  private val (train, held) = IntentFixture.labelled.zipWithIndex
    .partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  private lazy val model = Induced.train(train)

  private def report(name: String, best: String => Option[String]): (Double, Double, Map[String, (Int, Int)]) =
    val answered = held.flatMap((m, g) => best(m).map(b => (g, b)))
    val coverage = answered.size * 100.0 / held.size
    val precision = if answered.isEmpty then 0.0 else answered.count((g, b) => g == b) * 100.0 / answered.size
    val perClass = answered.groupBy(_._2).map((c, xs) => c -> (xs.count((g, b) => g == b), xs.size))
    println(f"[$name%-14s] fired on ${coverage}%5.1f%%, right where fired ${precision}%5.1f%%  " +
      perClass.toVector.sorted.map((c, pn) => s"$c ${pn._1}/${pn._2}").mkString("  "))
    (coverage, precision, perClass)

  test("the rules read as cues a person could have written, and each stands on two training rows") {
    println(s"\n[induced] ${model.rules.size} rules from ${train.size} rows:")
    model.rules.foreach(r => println("  " + r.show))
    assert(model.rules.nonEmpty)
    model.rules.foreach { r =>
      val support = train.count((m, c) => c == r.cls && r.fires(Induced.features(m)))
      assert(support >= 2, s"'${r.show}' stands on $support training rows")
    }
  }

  test("training is deterministic") {
    assertEquals(Induced.train(train).rules, Induced.train(train).rules)
  }

  test("coverage and precision where fired, beside the hand-written cues, on the same held-out rows") {
    val (cov, prec, per) = report("induced", m => Induced.classify(model, m, floor = 0.4))
    val (hcov, hprec, _) = report("hand-written", m => Patterns.classify(Models.cues, m, floor = 0.4))
    println(f"[induced] hand-written: fired ${hcov}%5.1f%% at ${hprec}%5.1f%%; induced: fired ${cov}%5.1f%% at ${prec}%5.1f%%")
    // the property the trusted tier holds is precision where it fires;
    // an induced set that fires more but is wrong more is not a cue
    // set — at the default floor (0.9) the grid below reads 85.7% on
    // 11.7% of the messages, and that is the number this asserts
    assert(prec >= 80.0, f"induced cues are right only ${prec}%.1f%% of the time where they fire")
    assert(cov > 0.0)
    per.foreach((c, pn) => assert(pn._2 > 0, c))
  }

  test("the trade-off, by support and precision floor, on the held-out rows") {
    println("\n[grid] support x precision floor -> rules, fired%, right-where-fired% (held-out)")
    for support <- Vector(2, 3, 4); floor <- Vector(0.8, 0.9, 1.0) do
      val m = Induced.train(train, minSupport = support, minPrecision = floor)
      val answered = held.flatMap((msg, g) => Induced.classify(m, msg, floor = 0.4).map(b => (g, b)))
      val cov = answered.size * 100.0 / held.size
      val prec = if answered.isEmpty then 0.0 else answered.count((g, b) => g == b) * 100.0 / answered.size
      println(f"  support $support  floor $floor%.1f  ->  ${m.rules.size}%2d rules, fired ${cov}%5.1f%%, right ${prec}%5.1f%%")
    assert(held.nonEmpty)
  }

  test("silence is visible: a class no rule reaches is named, against a declared taxonomy") {
    val without = Induced.against(Patterns.canonical, train.filter(_._2 != "Other")).toOption.get
    assert(without.silent.contains("Other"), s"silent: ${without.silent}")
    assert(!without.rules.exists(_.cls == "Other"))
    // and a label outside the taxonomy is refused, as every tier refuses it
    assert(Induced.against(Patterns.canonical, Seq("shall we meet" -> "Stray")).isLeft)
  }
}
