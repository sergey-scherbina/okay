package okay.agent

import okay.rag.Embedding

/**
 * Distil a static table from the teacher, then classify without it
 * (specs/intent-classify.md).
 *
 * The distillation is the only step that touches the gateway, and it
 * happens once. Everything measured after it runs on a table.
 *
 * The vocabulary comes from the TRAINING half alone, so out-of-
 * vocabulary words in the test half are a measured cost rather than a
 * leak — which is the whole question for a static table: how much of
 * an unseen message can it see at all.
 */
class TestStatic extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(900, "s")

  private val embedUrl = sys.env.getOrElse("OKAY_EMBED_URL",
    "http://127.0.0.1:8089/v1/embeddings")

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(embedUrl.replace("/embeddings", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def embed(texts: Seq[String]): Vector[Embedding] =
    if texts.isEmpty then Vector.empty
    else
      val body = okay.codec.Json.JObj(Vector(
        "input" -> okay.codec.Json.JArr(texts.map(okay.codec.Json.JStr(_)).toVector)))
      val conn = java.net.URI.create(embedUrl).toURL.openConnection()
        .asInstanceOf[java.net.HttpURLConnection]
      conn.setRequestMethod("POST")
      conn.setRequestProperty("Content-Type", "application/json")
      conn.setDoOutput(true)
      conn.setConnectTimeout(10000)
      conn.setReadTimeout(300000)
      val out = conn.getOutputStream
      out.write(okay.codec.Json.print(body).getBytes("UTF-8"))
      out.close()
      val text = scala.io.Source.fromInputStream(conn.getInputStream, "UTF-8").mkString
      okay.codec.Json.parseValue(text) match
        case okay.codec.Json.JObj(fields) =>
          fields.collectFirst { case ("data", okay.codec.Json.JArr(rows)) => rows }
            .getOrElse(Vector.empty)
            .flatMap {
              case okay.codec.Json.JObj(row) =>
                row.collectFirst { case ("embedding", okay.codec.Json.JArr(xs)) =>
                  okay.rag.embedding(xs.collect { case okay.codec.Json.JNum(d) => d.toFloat }.toArray)
                }
              case _ => None
            }
        case _ => Vector.empty

  test("live: distil a table, then classify with the gateway switched off") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val (train, test) = rows.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    // ---- the one and only step that uses the teacher -------------
    val vocab = train.flatMap((t, _) => Static.tokens(t)).distinct.sorted
    val vecs = vocab.grouped(64).flatMap(g => embed(g)).toVector
    assertEquals(vecs.length, vocab.length, "the teacher returned a different number of vectors")
    val tbl = Static.table(vocab.zip(vecs).toMap, train.map(_._1))
    println(f"\n[static] table of ${tbl.size} tokens x ${tbl.dim} dims " +
            f"(${tbl.size * tbl.dim * 4 / 1024}KB as float32)")

    // ---- everything below runs on the table ----------------------
    val cov = test.map((t, _) => Static.coverage(tbl, t))
    println(f"  vocabulary sees ${cov.sum / cov.length * 100}%.1f%% of an unseen message's words; " +
            f"${cov.count(_ == 0.0)} of ${test.length} messages entirely unseen")

    val t0 = System.nanoTime()
    val trainStatic = train.flatMap((t, g) => Static.encode(tbl, t).map(v => (v, g)))
    val testStatic = test.map((t, g) => (Static.encode(tbl, t), g))
    val micros = (System.nanoTime() - t0) / 1000 / math.max(rows.length, 1)

    val probe = Probe.train(trainStatic)
    val cen = Centroid.train(trainStatic)
    val answered = testStatic.count(_._1.isDefined)
    val pRight = testStatic.count((v, g) => v.flatMap(Probe.score(probe, _)).exists(_.best == g))
    val cRight = testStatic.count((v, g) => v.flatMap(Centroid.score(cen, _)).exists(_.best == g))

    println(f"  ${micros}us per message, NO network")
    println(f"  probe over static vectors:    ${pRight * 100.0 / test.length}%5.1f%% " +
            f"(teacher's live vectors: 86.7%%)")
    println(f"  centroid over static vectors: ${cRight * 100.0 / test.length}%5.1f%% " +
            f"(teacher's live vectors: 80.0%%)")
    println(f"  chargrams, for the other no-network option:   60.0%%")
    println(f"  encoded $answered of ${test.length}; the rest had no known token")

    assert(tbl.size > 100, s"a table of ${tbl.size} tokens is too small to mean anything")
    assert(micros < 50000)
  }

  /**
   * The same distillation with a vocabulary that is not starved.
   *
   * The first run's table held 301 tokens and saw 66% of an unseen
   * message's words, so 43.3% could mean "the method is weak" or "the
   * table is small" and the two need separating. A table's VOCABULARY
   * is not labels — a production one is built from a dictionary, not
   * from the training set — so distilling over every word in the
   * fixture bounds the method from above without leaking a single
   * label.
   */
  test("live: is it the method or the vocabulary") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val (train, test) = rows.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    // every word anyone might write, from the whole fixture and the
    // multilingual set: still no labels, only a dictionary
    val vocab = (rows.map(_._1) ++ IntentFixture.languages.flatMap(l =>
      IntentFixture.inLanguage(l).map(_._1))).flatMap(Static.tokens).distinct.sorted
    val vecs = vocab.grouped(64).flatMap(g => embed(g)).toVector
    assertEquals(vecs.length, vocab.length)
    val tbl = Static.table(vocab.zip(vecs).toMap, rows.map(_._1))
    val cov = test.map((t, _) => Static.coverage(tbl, t))
    println(f"\n[static, full dictionary] ${tbl.size} tokens, " +
            f"sees ${cov.sum / cov.length * 100}%.1f%% of an unseen message")

    val trainStatic = train.flatMap((t, g) => Static.encode(tbl, t).map(v => (v, g)))
    val probe = Probe.train(trainStatic)
    val cen = Centroid.train(trainStatic)
    val pRight = test.count((t, g) => Static.encode(tbl, t).flatMap(Probe.score(probe, _)).exists(_.best == g))
    val cRight = test.count((t, g) => Static.encode(tbl, t).flatMap(Centroid.score(cen, _)).exists(_.best == g))
    println(f"  probe    ${pRight * 100.0 / test.length}%5.1f%%   (starved table: 43.3%%, teacher: 86.7%%)")
    println(f"  centroid ${cRight * 100.0 / test.length}%5.1f%%   (starved table: 41.7%%, teacher: 80.0%%)")
    assert(tbl.size > 300)
  }

  /**
   * Words plus adjacent PAIRS, which is the fix the diagnosis implies.
   *
   * A word-only table is a bag of words, and this task's signal is
   * order — "could you" requests where "we could" proposes. The pair
   * is a unit the teacher embeds exactly like a word, so the whole
   * cost is a longer distillation and nothing at request time.
   */
  test("live: words and pairs, since order is where the signal is") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val (train, test) = rows.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    val vocab = rows.map(_._1).flatMap(Static.units).distinct.sorted
    val vecs = vocab.grouped(64).flatMap(g => embed(g)).toVector
    assertEquals(vecs.length, vocab.length)
    val tbl = Static.table(vocab.zip(vecs).toMap, rows.map(_._1), split = Static.units)
    println(f"\n[static, words+pairs] ${tbl.size} units x ${tbl.dim} dims " +
            f"(${tbl.size * tbl.dim * 4 / 1024}KB as float32)")

    val trainStatic = train.flatMap((t, g) => Static.encode(tbl, t).map(v => (v, g)))
    val probe = Probe.train(trainStatic)
    val cen = Centroid.train(trainStatic)
    val pRight = test.count((t, g) => Static.encode(tbl, t).flatMap(Probe.score(probe, _)).exists(_.best == g))
    val cRight = test.count((t, g) => Static.encode(tbl, t).flatMap(Centroid.score(cen, _)).exists(_.best == g))
    println(f"  probe    ${pRight * 100.0 / test.length}%5.1f%%   (words only: 51.7%%, chargrams: 60.0%%, teacher: 86.7%%)")
    println(f"  centroid ${cRight * 100.0 / test.length}%5.1f%%   (words only: 43.3%%, teacher: 80.0%%)")
    assert(tbl.size > 500)
  }
}
