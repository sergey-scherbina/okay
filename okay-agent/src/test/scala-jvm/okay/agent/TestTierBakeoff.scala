package okay.agent

import okay.rag.Embedding

/**
 * Every tier, one split, one table (specs/intent-classify.md).
 *
 * The goal these are being measured against changed mid-programme and
 * the reporting changed with it: the target is a classifier that needs
 * NO GENERATION on the request path, so the number that decides a tier
 * is its accuracy at FULL coverage, with a margin table beside it for
 * whoever wants to hand the uncertain tail to a person instead.
 *
 * Embeddings are not generation — a vectoriser, 12ms, no tokens — so
 * they stay inside the budget. Labels may come from a model once,
 * offline; the ban is on a model being present when a message arrives.
 */
class TestTierBakeoff extends munit.FunSuite {

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
    val body = okay.codec.Json.JObj(Vector(
      "input" -> okay.codec.Json.JArr(texts.map(okay.codec.Json.JStr(_)).toVector)))
    val conn = java.net.URI.create(embedUrl).toURL.openConnection()
      .asInstanceOf[java.net.HttpURLConnection]
    conn.setRequestMethod("POST")
    conn.setRequestProperty("Content-Type", "application/json")
    conn.setDoOutput(true)
    conn.setConnectTimeout(10000)
    conn.setReadTimeout(180000)
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

  /** one tier's row: what it answers, how often it is right when it
   * answers, and how often it is right over EVERYTHING — the last is
   * the one that matters for replacing a model rather than filtering
   * for one */
  private def report(name: String, n: Int, micros: Long,
                     scored: Seq[(String, Option[(String, Double)])],
                     floors: Seq[Double]): Unit =
    val overall = scored.count { case (g, v) => v.exists(_._1 == g) }
    println(f"\n[$name] ${micros}us per message")
    println(f"  accuracy over ALL messages: ${overall * 100.0 / n}%5.1f%%")
    for floor <- floors do
      val answered = scored.collect { case (g, Some((b, m))) if m >= floor => (g, b) }
      val right = answered.count((g, b) => g == b)
      val acc = if answered.isEmpty then 0.0 else right * 100.0 / answered.length
      println(f"  margin >= $floor%.2f   coverage ${answered.length * 100.0 / n}%5.1f%%   agreement $acc%5.1f%%")

  test("live: every tier on one split") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val (train, test) = IntentFixture.labelled.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    val trainVecs = embed(train.map(_._1))
    val testVecs = embed(test.map(_._1))
    assertEquals(trainVecs.length, train.length)
    val labelled = trainVecs.zip(train.map(_._2))
    val gold = test.map(_._2)
    val n = test.length
    println(s"\n=== ${train.length} train / $n test, dim ${trainVecs.head.length} ===")

    def timed[A](f: => A): (A, Long) =
      val t0 = System.nanoTime(); val a = f; (a, (System.nanoTime() - t0) / 1000 / math.max(n, 1))

    // patterns: no embedding at all
    val (pat, patUs) = timed(test.map((m, _) =>
      Patterns.score(Patterns.meeting, m).map(v => (v.best, v.margin))))
    report("patterns", n, patUs, gold.zip(pat), Seq(0.0, 0.2, 0.4))

    // symbolic: BM25, no embedding
    val sym = Symbolic.train(train)
    val (symOut, symUs) = timed(test.map((m, _) =>
      Symbolic.score(sym, m).map(v => (v.best, v.margin))))
    report("symbolic (bm25)", n, symUs, gold.zip(symOut), Seq(0.0, 0.2, 0.3))

    val cen = Centroid.train(labelled)
    val (cenOut, cenUs) = timed(testVecs.map(v =>
      Centroid.score(cen, v).map(x => (x.best, x.margin))))
    report("centroid", n, cenUs, gold.zip(cenOut), Seq(0.0, 0.02, 0.05))

    val knn = Nearest.train(labelled)
    val (knnOut, knnUs) = timed(testVecs.map(v =>
      Nearest.score(knn, v).map(x => (x.best, x.margin))))
    report("knn (k=5)", n, knnUs, gold.zip(knnOut), Seq(0.0, 0.1, 0.2))

    val t0 = System.nanoTime()
    val probe = Probe.train(labelled)
    val fitMs = (System.nanoTime() - t0) / 1000000
    val (probeOut, probeUs) = timed(testVecs.map(v =>
      Probe.score(probe, v).map(x => (x.best, x.margin))))
    println(f"\n(the probe fit in ${fitMs}ms)")
    report("linear probe", n, probeUs, gold.zip(probeOut), Seq(0.0, 0.3, 0.6))

    assert(pat.length == n && cenOut.length == n && probeOut.length == n)
  }
}
