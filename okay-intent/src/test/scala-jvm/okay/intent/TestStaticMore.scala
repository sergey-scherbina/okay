package okay.intent

import okay.rag.{Embedding, embedding}

/**
 * The two extensions of the static table filed by
 * intent-static-embeddings rather than guessed
 * (specs/intent-classify.md, intent-static-trigrams-and-pca):
 *
 *  - adjacent TRIPLES beside words and pairs — pairs were worth 11.6
 *    points over words, and the same argument applies once more with
 *    diminishing returns and a bigger table;
 *  - model2vec's PCA step — 1024 dimensions cut to 256 (and 128), which
 *    decides whether a production vocabulary ships: 30k units at 1024
 *    float32 is 120 MB.
 *
 * One distillation against the teacher for all of it; the baseline
 * (words + pairs at 1024) is re-measured in the SAME run and split, so
 * every comparison is like for like. The PCA is fitted on the table's
 * own unit vectors (centred, subspace iteration), and a message is
 * encoded by pooling the PROJECTED unit vectors — the same code path
 * as before, on a narrower table. Measured 2026-09-07, same split:
 * words+pairs 61.7/53.3 (5.3 MB); +triples 66.7/65.0 (8.7 MB); pairs
 * at PCA 256 66.7/58.3 (1.3 MB, 91.5% of the variance); triples at
 * PCA 256 68.3/66.7 (2.1 MB) — both landed in `Static`.
 */
class TestStaticMore extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1200, "s")

  private val embedUrl = sys.env.getOrElse("OKAY_EMBED_URL", "http://127.0.0.1:8089/v1/embeddings")

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
      val conn = java.net.URI.create(embedUrl).toURL.openConnection().asInstanceOf[java.net.HttpURLConnection]
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
          fields.collectFirst { case ("data", okay.codec.Json.JArr(rows)) => rows }.getOrElse(Vector.empty)
            .flatMap {
              case okay.codec.Json.JObj(row) =>
                row.collectFirst { case ("embedding", okay.codec.Json.JArr(xs)) =>
                  embedding(xs.collect { case okay.codec.Json.JNum(d) => d.toFloat }.toArray) }
              case _ => None
            }
        case _ => Vector.empty

  private def units3(text: String): Vector[String] = Static.units3(text)
  private type Pca = Static.Pca
  private def fitPca(vectors: Iterable[Embedding], k: Int): Pca = Static.fitPca(vectors, k)
  private def projected(t: Static.Table, p: Pca): Static.Table = Static.projected(t, p)
  private def kept(p: Pca, vectors: Iterable[Embedding]): Double = Static.variance(p, vectors)

  // ---- the measurement -----------------------------------------

  private final case class Score(probe: Double, centroid: Double, answered: Int)

  private def score(tbl: Static.Table, train: Seq[(String, String)], test: Seq[(String, String)]): Score =
    val trainStatic = train.flatMap((t, g) => Static.encode(tbl, t).map(v => (v, g)))
    val probe = Probe.train(trainStatic)
    val cen = Centroid.train(trainStatic)
    val enc = test.map((t, g) => (Static.encode(tbl, t), g))
    Score(
      enc.count((v, g) => v.flatMap(Probe.score(probe, _)).exists(_.best == g)) * 100.0 / test.length,
      enc.count((v, g) => v.flatMap(Centroid.score(cen, _)).exists(_.best == g)) * 100.0 / test.length,
      enc.count(_._1.isDefined))

  private def kb(t: Static.Table): Long = t.size.toLong * t.dim * 4 / 1024

  test("live: triples beside pairs, and PCA to 256 and 128, against words+pairs in the same run") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val (train, test) = rows.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    // one distillation: every word, pair and triple anyone wrote
    val vocab = rows.map(_._1).flatMap(units3).distinct.sorted
    val t0 = System.nanoTime()
    val vecs = vocab.grouped(64).flatMap(g => embed(g)).toVector
    assertEquals(vecs.length, vocab.length, "the teacher returned a different number of vectors")
    val all = vocab.zip(vecs).toMap
    println(f"\n[static-more] ${vocab.length} units distilled in ${(System.nanoTime() - t0) / 1e9}%.1f s")

    val pairsVocab = rows.map(_._1).flatMap(Static.units).distinct.toSet
    val pairs = Static.table(all.filter((k, _) => pairsVocab(k)), rows.map(_._1), split = Static.units)
    val triples = Static.table(all, rows.map(_._1), split = units3)

    val base = score(pairs, train, test)
    val tri = score(triples, train, test)
    println(f"  words+pairs        ${pairs.size}%5d units x ${pairs.dim}%4d  ${kb(pairs)}%6d KB  probe ${base.probe}%5.1f%%  centroid ${base.centroid}%5.1f%%  (answered ${base.answered}/${test.length})")
    println(f"  words+pairs+triples ${triples.size}%5d units x ${triples.dim}%4d  ${kb(triples)}%6d KB  probe ${tri.probe}%5.1f%%  centroid ${tri.centroid}%5.1f%%  (answered ${tri.answered}/${test.length})")

    val t1 = System.nanoTime()
    val p256 = fitPca(pairs.vectors.values, 256)
    val p128 = p256.take(128)
    println(f"  pca fitted on the words+pairs table in ${(System.nanoTime() - t1) / 1e9}%.1f s; variance kept: 256 -> ${kept(p256, pairs.vectors.values) * 100}%.1f%%, 128 -> ${kept(p128, pairs.vectors.values) * 100}%.1f%%")
    for (k, p) <- Seq(256 -> p256, 128 -> p128) do
      val tbl = projected(pairs, p)
      val s = score(tbl, train, test)
      println(f"  words+pairs @ pca $k%4d ${tbl.size}%5d units x ${tbl.dim}%4d  ${kb(tbl)}%6d KB  probe ${s.probe}%5.1f%%  centroid ${s.centroid}%5.1f%%")
    val tri256 = projected(triples, fitPca(triples.vectors.values, 256))
    val s3 = score(tri256, train, test)
    println(f"  +triples @ pca  256 ${tri256.size}%5d units x ${tri256.dim}%4d  ${kb(tri256)}%6d KB  probe ${s3.probe}%5.1f%%  centroid ${s3.centroid}%5.1f%%")
    println(f"  (teacher, live vectors: probe 86.7%%, centroid 80.0%%; chargrams 60.0%%)")

    assert(pairs.size > 500 && triples.size > pairs.size)
  }
}
