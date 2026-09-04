package okay.intent

import okay.given
import okay.codec.{Json, Schema}
import okay.rag.Embedding
import java.nio.file.{Files, Paths}

/**
 * Does the probe's flat curve survive DIFFERENT data?
 * (specs/intent-classify.md)
 *
 * The learning curve found the probe flat past 32 examples and this
 * spec concluded that labels are not its constraint. That conclusion
 * was drawn on one author's sentences in one register — and the
 * distillation lane then showed the generated corpus has a measurably
 * different distribution, since training on it alone scores ten points
 * BELOW a human fixture a third the size.
 *
 * So the flatness may be about HOMOGENEITY rather than quantity, and
 * the test is cheap: the corpus is already generated, and it only has
 * to be embedded.
 *
 * Evaluation stays on held-out HUMAN data, so a corpus cannot score
 * itself.
 */
class TestDistilForProbe extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1800, "s")

  private val embedUrl = sys.env.getOrElse("OKAY_EMBED_URL",
    "http://127.0.0.1:8089/v1/embeddings")
  private val store = Paths.get("okay-agent/src/test/resources/intent-distilled.json")

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

  test("live: is the probe's ceiling quantity, or one author's register") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    assume(Files.exists(store), "no distilled corpus — run TestDistil's generator")
    val distilled = Json.decode(summon[Schema[Corpus]])(Json.parseValue(Files.readString(store)))
      .map(_.rows).getOrElse(Vector.empty)
    assume(distilled.length >= 100, s"only ${distilled.length} distilled rows")

    val human = IntentFixture.labelled
    val (humanTrain, humanTest) = human.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    val humanVecs = embed(human.map(_._1))
    val distVecs = distilled.grouped(64).flatMap(g => embed(g.map(_.text))).toVector
    assertEquals(humanVecs.length, human.length)
    assertEquals(distVecs.length, distilled.length)

    val byText = human.map(_._1).zip(humanVecs).toMap
    val trainH = humanTrain.map((t, c) => (byText(t), c))
    val testH = humanTest.map((t, c) => (byText(t), c))
    val trainD = distVecs.zip(distilled.map(_.cls))

    // every row carries the terms it was taken under: this lane's
    // predecessor published a number that a later run could not
    // reproduce, because the two framings were never printed
    // the distilled count is PASSED, not derived: deriving it as
    // rows.length - trainH.length said "distilled=260" for the arm
    // that has no human rows at all, and a condition that lies is
    // worse than one that is missing
    def run(name: String, rows: Seq[(Embedding, String)], distilled: Int, corpus: String): Unit =
      val p = Probe.train(rows)
      val c = Centroid.train(rows)
      val pr = testH.count((v, g) => Probe.score(p, v).exists(_.best == g)) * 100.0 / testH.length
      val cr = testH.count((v, g) => Centroid.score(c, v).exists(_.best == g)) * 100.0 / testH.length
      println(Conditions.line(
        Conditions(Conditions.SmallEmbedder, Conditions.Bare, rows.length, testH.length,
          corpus = corpus, extra = s"distilled=$distilled"),
        name, f"probe $pr%5.1f%%  centroid $cr%5.1f%%"))

    println(f"\n=== held out: ${testH.length} human messages, never trained on ===")
    run("human fixture alone", trainH, 0, "IntentFixture.labelled")
    run("distilled alone", trainD, trainD.length, "intent-distilled.json")
    run("human + distilled", trainH ++ trainD, trainD.length, "both")
    // and the question the learning curve could not ask: does MORE of
    // the different data keep helping, where more of the same did not
    for take <- Seq(40, 80, 120, trainD.length) do
      run(s"human + $take distilled", trainH ++ trainD.take(take), take, "both")

    assert(testH.nonEmpty)
  }
}
