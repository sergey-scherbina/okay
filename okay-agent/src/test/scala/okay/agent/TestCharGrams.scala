package okay.agent

/**
 * Character n-grams, in the default gate: no model, no network, no
 * embedding (specs/intent-classify.md).
 *
 * The tier that could make the zero-infrastructure path viable, and
 * the one that should be language-agnostic by construction — so it is
 * measured twice, on the English fixture and on the parallel set,
 * where every other tier's Russian arm falls away.
 */
class TestCharGrams extends munit.FunSuite {

  private val (train, test) = IntentFixture.labelled.zipWithIndex
    .partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  private lazy val model = CharGrams.train(train)

  test("the features are language-agnostic by construction") {
    // a 4-gram window does not know what alphabet it is in: the same
    // text in two scripts produces vectors of the same shape, and a
    // message shares nothing with an unrelated one
    val ru = CharGrams.features("Можем встретиться во вторник?", 256, 3, 5)
    val en = CharGrams.features("Can we meet on Tuesday?", 256, 3, 5)
    assertEquals(ru.length, en.length)
    assert(ru.exists(_ > 0.0) && en.exists(_ > 0.0))
    assertEqualsDouble(math.sqrt(ru.map(x => x * x).sum), 1.0, 1e-9)
  }

  test("training is deterministic: the same data gives the same model") {
    val a = CharGrams.train(train.take(20), dim = 512, epochs = 20)
    val b = CharGrams.train(train.take(20), dim = 512, epochs = 20)
    assertEquals(a.classes, b.classes)
    assert(a.w(0).zip(b.w(0)).forall((x, y) => math.abs(x - y) < 1e-12),
      "gradient descent from zero weights over ordered data must be reproducible")
  }

  test("accuracy on the English fixture, at full coverage and at a margin") {
    // force the fit BEFORE the timer: `model` is lazy, and the first
    // version of this timed the training inside the scoring loop and
    // reported it as a per-message cost
    val fitStart = System.nanoTime()
    val ready = model
    val fitMs = (System.nanoTime() - fitStart) / 1000000
    assert(ready.classes.nonEmpty)
    val t0 = System.nanoTime()
    val scored = test.map((m, gold) => (gold, CharGrams.score(model, m)))
    val micros = (System.nanoTime() - t0) / 1000 / math.max(test.length, 1)
    val right = scored.count { case (g, v) => v.exists(_.best == g) }
    println(f"\n[chargrams] ${micros}us per message (fit took ${fitMs}ms), no network")
    println(f"  accuracy over ALL messages: ${right * 100.0 / test.length}%5.1f%%")
    for floor <- Seq(0.0, 0.3, 0.6) do
      val answered = scored.collect { case (g, Some(v)) if v.margin >= floor => (g, v.best) }
      val acc = if answered.isEmpty then 0.0 else answered.count((g, b) => g == b) * 100.0 / answered.length
      println(f"  margin >= $floor%.1f   coverage ${answered.length * 100.0 / test.length}%5.1f%%   agreement $acc%5.1f%%")
    assert(micros < 20000, s"${micros}us is not a fast tier")
  }

  test("and the same, per language, where the embedding tiers lose Russian") {
    // trained on ALL languages at once: one model, not six
    val rows = IntentFixture.languages.flatMap(l => IntentFixture.inLanguage(l))
    val (tr, te) = rows.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))
    val m = CharGrams.train(tr)
    println(f"\n[chargrams, multilingual] trained on ${tr.length}, scored on ${te.length}")
    for lang <- IntentFixture.languages do
      val rowsFor = IntentFixture.inLanguage(lang).filter(r => te.contains(r))
      if rowsFor.nonEmpty then
        val right = rowsFor.count((msg, gold) => CharGrams.score(m, msg).exists(_.best == gold))
        println(f"  $lang%-3s ${right * 100.0 / rowsFor.length}%5.1f%% over ${rowsFor.length} messages")
    assert(te.nonEmpty)
  }
}
