package okay.agent

import okay.rag.Embedding

/**
 * Two embedders, one harness (specs/intent-classify.md).
 *
 * The experiment `intent-embedding-choice` was blocked on: a second
 * embedding model now exists on this machine, so the vectoriser can be
 * the only thing that changes. It matters more than another classifier
 * would, because the journal already showed the probe and the model
 * tier sharing ZERO errors — the signal is in the text and the vector
 * is losing it, which makes this the ceiling rather than the method.
 */
class TestSecondEmbedder extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1800, "s")

  private val url = sys.env.getOrElse("OKAY_EMBED_URL",
    "http://127.0.0.1:8089/v1/embeddings")

  private val small = "mlx-community:Qwen3-Embedding-0.6B-4bit-DWQ"
  private val large = "mlx-community:Qwen3-Embedding-4B-4bit-DWQ"

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(url.replace("/embeddings", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def embed(model: String, texts: Seq[String]): Vector[Embedding] =
    if texts.isEmpty then Vector.empty
    else
      val body = okay.codec.Json.JObj(Vector(
        "model" -> okay.codec.Json.JStr(model),
        "input" -> okay.codec.Json.JArr(texts.map(okay.codec.Json.JStr(_)).toVector)))
      val conn = java.net.URI.create(url).toURL.openConnection()
        .asInstanceOf[java.net.HttpURLConnection]
      conn.setRequestMethod("POST")
      conn.setRequestProperty("Content-Type", "application/json")
      conn.setDoOutput(true)
      conn.setConnectTimeout(10000)
      conn.setReadTimeout(600000)
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

  private def split[A](xs: Seq[A]): (Seq[A], Seq[A]) =
    xs.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  private def score(rows: Seq[(String, String)], vecs: Vector[Embedding]): (Double, Double) =
    val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
    val (train, test) = split(all)
    val probe = Probe.train(train.map((_, v, c) => (v, c)))
    val cen = Centroid.train(train.map((_, v, c) => (v, c)))
    val p = test.count((_, v, g) => Probe.score(probe, v).exists(_.best == g)) * 100.0 / test.length
    val c = test.count((_, v, g) => Centroid.score(cen, v).exists(_.best == g)) * 100.0 / test.length
    (p, c)

  test("live: 0.6B against 4B, on the English fixture") {
    assume(reachable, s"no embeddings endpoint at $url")
    val rows = IntentFixture.labelled
    println("\n=== the same classifier over two vectorisers ===")
    for (model, name) <- Seq(small -> "0.6B (1024 dims)", large -> "4B (2560 dims)") do
      val t0 = System.nanoTime()
      val vecs = rows.grouped(32).flatMap(g => embed(model, g.map(_._1))).toVector
      val ms = (System.nanoTime() - t0) / 1000000
      assertEquals(vecs.length, rows.length, s"$name returned the wrong count")
      val (p, c) = score(rows, vecs)
      println(f"  $name%-18s probe $p%5.1f%%   centroid $c%5.1f%%   (${ms}ms for ${rows.length} messages)")
  }

  test("live: 0.6B against 4B, per language") {
    assume(reachable, s"no embeddings endpoint at $url")
    println("\n=== per language ===")
    println("  lang   0.6B probe   4B probe")
    for lang <- IntentFixture.languages do
      val rows = IntentFixture.inLanguage(lang)
      val a = score(rows, rows.grouped(32).flatMap(g => embed(small, g.map(_._1))).toVector)._1
      val b = score(rows, rows.grouped(32).flatMap(g => embed(large, g.map(_._1))).toVector)._1
      println(f"  $lang%-5s  $a%9.1f%%  $b%9.1f%%")
  }

  /**
   * The 4B model, asked properly.
   *
   * "Bigger is worse" is a claim about a model, and before making it
   * the model deserves the framing it was built for: Qwen3-Embedding
   * is instruction-tuned, a short classify instruction was already
   * worth +1.6/+3.3 to the 0.6B, and a larger instruction-tuned model
   * is if anything MORE sensitive to being told what the vector is
   * for. If the gap closes here, the finding is about framing; if it
   * does not, it is about the model.
   */
  test("live: does the 4B need the instruction the 0.6B merely liked") {
    assume(reachable, s"no embeddings endpoint at $url")
    val rows = IntentFixture.labelled
    val prefix = "Classify the intent of this message: "
    println("\n=== with and without a classify instruction ===")
    for (model, name) <- Seq(small -> "0.6B", large -> "4B  ") do
      for (pre, how) <- Seq("" -> "bare  ", prefix -> "framed") do
        val vecs = rows.grouped(32).flatMap(g => embed(model, g.map((t, _) => pre + t))).toVector
        assertEquals(vecs.length, rows.length)
        val (p, c) = score(rows, vecs)
        println(f"  $name $how   probe $p%5.1f%%   centroid $c%5.1f%%")
  }
}
