package okay.agent

import okay.given
import okay.codec.{Json, Schema}
import okay.rag.Embedding
import java.nio.file.{Files, Paths}

/**
 * The programme, re-read with the centroid as the subject
 * (specs/intent-classify.md).
 *
 * Forty distilled rows took the centroid to 90.0%, matching the model
 * tier from four vectors — and every table before that was read with
 * the PROBE as the subject. Three conclusions turned on the probe's
 * number specifically, and each is re-run here:
 *
 *  - the 4B embedder was judged worse because 2560 dimensions cost the
 *    probe weights it could not fill on sixty examples; a centroid
 *    fits four vectors either way and may not care.
 *  - the classify instruction was worth +1.6 to the probe and +3.3 to
 *    the centroid, and the default was chosen on the smaller of the two.
 *  - the learning curve was read as a SIGNAL ceiling because both
 *    flattened together — measured before distilled data existed, and
 *    the centroid has since moved ten points.
 */
class TestCentroidFirst extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1800, "s")

  private val url = sys.env.getOrElse("OKAY_EMBED_URL",
    "http://127.0.0.1:8089/v1/embeddings")
  private val small = "mlx-community:Qwen3-Embedding-0.6B-4bit-DWQ"
  private val large = "mlx-community:Qwen3-Embedding-4B-4bit-DWQ"
  private val store = Paths.get("okay-agent/src/test/resources/intent-distilled.json")

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

  final case class Corpus(rows: Vector[Phrasing]) derives Schema

  private val human = IntentFixture.labelled
  private val (trainIdx, testIdx) = human.indices.partition(_ % 2 == 1)

  private def score(vecs: Vector[Embedding], extra: Seq[(Embedding, String)] = Nil): (Double, Double) =
    val train = trainIdx.map(i => (vecs(i), human(i)._2)) ++ extra
    val test = testIdx.map(i => (vecs(i), human(i)._2))
    val p = Probe.train(train)
    val c = Centroid.train(train)
    (test.count((v, g) => Probe.score(p, v).exists(_.best == g)) * 100.0 / test.length,
     test.count((v, g) => Centroid.score(c, v).exists(_.best == g)) * 100.0 / test.length)

  test("live: the 4B embedder, judged by the centroid rather than the probe") {
    assume(reachable, s"no endpoint at $url")
    val prefix = "Classify the intent of this message: "
    println("\n=== which model, and who is asking ===")
    for (m, name) <- Seq(small -> "0.6B", large -> "4B  ") do
      for (pre, how) <- Seq("" -> "bare  ", prefix -> "framed") do
        val vecs = human.grouped(32).flatMap(g => embed(m, g.map((t, _) => pre + t))).toVector
        val (p, c) = score(vecs)
        println(f"  $name $how   probe $p%5.1f%%   centroid $c%5.1f%%")
  }

  test("live: the learning curve, with distilled data available") {
    assume(reachable, s"no endpoint at $url")
    assume(Files.exists(store), "no distilled corpus")
    val distilled = Json.decode(summon[Schema[Corpus]])(Json.parseValue(Files.readString(store)))
      .map(_.rows).getOrElse(Vector.empty)
    val prefix = "Classify the intent of this message: "
    val vecs = human.grouped(32).flatMap(g => embed(small, g.map((t, _) => prefix + t))).toVector
    val dVecs = distilled.grouped(64).flatMap(g => embed(small, g.map(prefix + _.text))).toVector
    val dRows = dVecs.zip(distilled.map(_.cls))

    println("\n=== the curve the learning-curve lane could not draw ===")
    println("  distilled added   probe   centroid")
    for n <- Seq(0, 10, 20, 40, 60, 80, 120) do
      val (p, c) = score(vecs, dRows.take(n))
      println(f"  ${n}%15d   $p%5.1f%%   $c%5.1f%%")
    assert(dRows.nonEmpty)
  }
}
