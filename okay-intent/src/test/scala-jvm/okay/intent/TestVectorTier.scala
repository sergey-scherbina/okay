package okay.intent

import okay.given
import okay.rag.Embedding

/**
 * The vector tier on the same split as the symbolic one
 * (specs/intent-classify.md).
 *
 * Live-tagged because embedding needs the gateway, but the QUESTION is
 * not about the model's judgement: it is whether class centroids give
 * a margin that behaves like a confidence, where BM25's did not. So
 * the arms print the same three numbers the symbolic tier printed —
 * coverage, agreement, cost — and on the same odd/even split, so the
 * two tables are comparable line for line.
 */
class TestVectorTier extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(900, "s")

  private val embedUrl = sys.env.getOrElse("OKAY_EMBED_URL",
    "http://127.0.0.1:8089/v1/embeddings")

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(embedUrl.replace("/embeddings", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  /** one batch, straight at the gateway — the tier itself never calls
   * out, so this plumbing lives in the test where it belongs */
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

  test("live: does a centroid margin behave like a confidence") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val all = IntentFixture.labelled
    val (train, test) = all.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    // batched: one call per half, not one per message
    val trainVecs = embed(train.map(_._1))
    val testVecs = embed(test.map(_._1))
    assertEquals(trainVecs.length, train.length, "the gateway returned a different number of vectors")
    assertEquals(testVecs.length, test.length)
    println(s"\n[vector] dim ${trainVecs.head.length}, ${train.length} train / ${test.length} test")

    val centroids = Centroid.train(trainVecs.zip(train.map(_._2)))

    val t0 = System.nanoTime()
    val scored = testVecs.zip(test.map(_._2)).map((v, gold) => (gold, Centroid.score(centroids, v)))
    val micros = (System.nanoTime() - t0) / 1000 / math.max(test.length, 1)

    println(f"[vector] ${micros}us per message once embedded")
    for floor <- Seq(0.0, 0.02, 0.05, 0.10, 0.15) do
      val answered = scored.collect { case (g, Some(v)) if v.margin >= floor => (g, v.best) }
      val right = answered.count((g, b) => g == b)
      val cover = answered.length.toDouble / test.length
      val acc = if answered.isEmpty then 0.0 else right.toDouble / answered.length
      println(f"  margin >= $floor%.2f   coverage ${cover * 100}%5.1f%%   agreement ${acc * 100}%5.1f%%")

    assert(scored.forall((_, v) => v.forall(x => x.similarity >= -1.01 && x.similarity <= 1.01)),
      "a cosine outside [-1, 1] means the vectors are not what we think they are")

    // the cost argument needs the number the batch hides: what ONE
    // message costs to embed, since production embeds one at a time
    val single = test.head._1
    val warm = embed(Seq(single))
    assertEquals(warm.length, 1)
    val t1 = System.nanoTime()
    for _ <- 1 to 5 do embed(Seq(single)): Unit
    val embedMs = (System.nanoTime() - t1) / 1000000 / 5
    println(f"[vector] one message embedded: ${embedMs}ms round trip, then ${micros}us to classify")
  }
}
