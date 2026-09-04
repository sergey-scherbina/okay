package okay.agent

import okay.rag.Embedding

/**
 * How much is more data worth? (specs/intent-classify.md)
 *
 * The probe sits at 86.7% against the model tier's ~90%, and that gap
 * is either a DATA limit or a REPRESENTATION limit — 4096 weights on
 * 60 rows could be either, and the two call for opposite lanes. A
 * curve answers it: still climbing at the right-hand end means more
 * labels pay, flat means the embedding is the ceiling.
 *
 * The centroid is measured beside it as a control. It fits four
 * vectors rather than 4096 weights, so if the story is PARAMETERS it
 * should flatten much earlier; if both flatten together the limit is
 * the signal they share.
 */
class TestLearningCurve extends munit.FunSuite {

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

  test("live: what another label is worth") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val vecs = embed(rows.map(_._1))
    val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
    val (trainAll, test) = all.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    // grow the training set CLASS-BALANCED, so a small n is not
    // accidentally a different problem from a large one
    def firstN(n: Int): Seq[(String, Embedding, String)] =
      trainAll.groupBy(_._3).toSeq.sortBy(_._1)
        .flatMap((_, rs) => rs.take(math.max(1, n / 4)))

    println(f"\n=== learning curve, ${test.length} held out ===")
    println("  n    probe   centroid   chargrams")
    for n <- Seq(8, 16, 24, 32, 40, 48, 56, trainAll.length) do
      val tr = firstN(n)
      val probe = Probe.train(tr.map((_, v, c) => (v, c)))
      val cen = Centroid.train(tr.map((_, v, c) => (v, c)))
      val cg = CharGrams.train(tr.map((t, _, c) => (t, c)))
      val p = test.count((_, v, g) => Probe.score(probe, v).exists(_.best == g)) * 100.0 / test.length
      val c = test.count((_, v, g) => Centroid.score(cen, v).exists(_.best == g)) * 100.0 / test.length
      val h = test.count((t, _, g) => CharGrams.score(cg, t).exists(_.best == g)) * 100.0 / test.length
      println(f"  ${tr.length}%3d  $p%5.1f%%   $c%5.1f%%     $h%5.1f%%")

    assert(test.nonEmpty)
  }
}
