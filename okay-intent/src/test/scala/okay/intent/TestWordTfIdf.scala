package okay.intent

/**
 * Word TF-IDF with a linear head, in the default gate: no model, no
 * network (specs/intent-classify.md, intent-tfidf-word-linear).
 *
 * Measured on the same split and the same per-language set as
 * `TestCharGrams`, so its number goes into the same table beside the
 * n-gram tier's — the question being whether that tier's accuracy is
 * about characters or about a linear model.
 */
class TestWordTfIdf extends munit.FunSuite {

  private val (train, test) = IntentFixture.labelled.zipWithIndex
    .partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  private lazy val model = WordTfIdf.train(train)

  test("tokens are runs of letters or digits in any script, lowercased") {
    assertEquals(WordTfIdf.tokens("Can we meet on Tuesday at 10?"),
      Vector("can", "we", "meet", "on", "tuesday", "at", "10"))
    assertEquals(WordTfIdf.tokens("Можем встретиться во вторник?"),
      Vector("можем", "встретиться", "во", "вторник"))
  }

  test("the vocabulary and IDF come from the training half only; an unseen word weighs nothing") {
    val vocab = WordTfIdf.fit(Seq("meet on tuesday", "meet on friday"))
    assertEquals(vocab.size, 4)
    // "meet" and "on" are in every document: the smallest IDF; the
    // days are in one each: larger
    val i = vocab.index
    assert(vocab.idf(i("meet")) < vocab.idf(i("tuesday")))
    val v = WordTfIdf.features(vocab, "meet on saturday")
    assert(v.exists(_ > 0f))
    assertEqualsDouble(math.sqrt(v.map(x => x.toDouble * x).sum), 1.0, 1e-6)
  }

  test("training is deterministic: the same data gives the same model") {
    val a = WordTfIdf.train(train.take(20), epochs = 20)
    val b = WordTfIdf.train(train.take(20), epochs = 20)
    assertEquals(a.classes, b.classes)
    assert(a.probe.w(0).zip(b.probe.w(0)).forall((x, y) => math.abs(x - y) < 1e-12))
  }

  test("accuracy on the English fixture, at full coverage and at a margin") {
    val fitStart = System.nanoTime()
    val ready = model
    val fitMs = (System.nanoTime() - fitStart) / 1000000
    assert(ready.classes.nonEmpty)
    val t0 = System.nanoTime()
    val scored = test.map((m, gold) => (gold, WordTfIdf.score(model, m)))
    val micros = (System.nanoTime() - t0) / 1000 / math.max(test.length, 1)
    val right = scored.count { case (g, v) => v.exists(_.best == g) }
    println(f"\n[word tf-idf] ${micros}us per message (fit took ${fitMs}ms, vocabulary ${model.vocab.size}), no network")
    println(f"  accuracy over ALL messages: ${right * 100.0 / test.length}%5.1f%%")
    for floor <- Seq(0.0, 0.3, 0.6) do
      val answered = scored.collect { case (g, Some(v)) if v.margin >= floor => (g, v.best) }
      val acc = if answered.isEmpty then 0.0 else answered.count((g, b) => g == b) * 100.0 / answered.length
      println(f"  margin >= $floor%.1f   coverage ${answered.length * 100.0 / test.length}%5.1f%%   agreement $acc%5.1f%%")
    assert(micros < 20000, s"${micros}us is not a fast tier")
  }

  test("and the same, per language, on one model trained on all of them") {
    val rows = IntentFixture.languages.flatMap(l => IntentFixture.inLanguage(l))
    val (tr, te) = rows.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))
    val m = WordTfIdf.train(tr)
    println(f"\n[word tf-idf, multilingual] trained on ${tr.length}, scored on ${te.length}, vocabulary ${m.vocab.size}")
    for lang <- IntentFixture.languages do
      val rowsFor = IntentFixture.inLanguage(lang).filter(r => te.contains(r))
      if rowsFor.nonEmpty then
        val right = rowsFor.count((msg, gold) => WordTfIdf.score(m, msg).exists(_.best == gold))
        println(f"  $lang%-3s ${right * 100.0 / rowsFor.length}%5.1f%% over ${rowsFor.length} messages")
    assert(te.nonEmpty)
  }
}
