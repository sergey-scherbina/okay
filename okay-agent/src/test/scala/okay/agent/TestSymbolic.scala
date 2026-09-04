package okay.agent

/**
 * The symbolic tier, measured on a SPLIT (specs/intent-classify.md).
 *
 * An index scored against its own training examples measures nothing —
 * BM25 will find the identical document and report a perfect margin.
 * So the fixture is halved deterministically: odd positions train,
 * even positions are scored, and no message appears in both.
 *
 * The numbers this suite prints are the ones that decide whether the
 * tier earns a place: COVERAGE at a margin, AGREEMENT with gold on
 * what it answered, and the cost of the pass. It runs in the default
 * gate, with no model, which is the tier's whole argument.
 */
class TestSymbolic extends munit.FunSuite {

  private val all = IntentFixture.labelled
  private val (train, test) = all.zipWithIndex.partition(_._2 % 2 == 1) match
    case (a, b) => (a.map(_._1), b.map(_._1))

  private val trained = Symbolic.train(train)

  test("the split is a split: nothing scored was trained on") {
    val overlap = test.map(_._1).toSet intersect train.map(_._1).toSet
    assertEquals(overlap, Set.empty[String])
    assert(train.length >= 55 && test.length >= 55, s"${train.length}/${test.length}")
  }

  test("a trained class is found by its own words, and the margin is relative") {
    // "please send the agenda" is a Request in the fixture's own terms
    val v = Symbolic.score(trained, "Could you send me the agenda please?")
    assert(v.isDefined, "nothing scored at all")
    assert(v.get.margin >= 0.0 && v.get.margin <= 1.0, s"margin out of range: ${v.get.margin}")
  }

  test("a message with no shared vocabulary is declined, not guessed") {
    // the tier's safety property: silence is an answer it is allowed
    // to give, and the whole reason it can sit in front of a model
    assertEquals(Symbolic.classify(trained, "zzzz qqqq xxxx"), None)
  }

  test("coverage and agreement at several margins — the numbers that decide it") {
    val t0 = System.nanoTime()
    val scored = test.map((m, gold) => (gold, Symbolic.score(trained, m)))
    val elapsedMicros = (System.nanoTime() - t0) / 1000 / math.max(test.length, 1)

    println(f"\n[symbolic] ${test.length} messages, ${elapsedMicros}us per message")
    for floor <- Seq(0.0, 0.1, 0.2, 0.3, 0.5) do
      val answered = scored.collect { case (g, Some(v)) if v.margin >= floor => (g, v.best) }
      val right = answered.count((g, b) => g == b)
      val cover = answered.length.toDouble / test.length
      val acc = if answered.isEmpty then 0.0 else right.toDouble / answered.length
      println(f"  margin >= $floor%.1f   coverage ${cover * 100}%5.1f%%   agreement ${acc * 100}%5.1f%%")

    // ours to assert: the pass runs and stays inside its contract
    assert(scored.forall { case (_, v) => v.forall(x => x.margin >= 0.0 && x.margin <= 1.0) })
    assert(elapsedMicros < 50000, s"a symbolic pass taking ${elapsedMicros}us is not a fast tier")
  }
}
