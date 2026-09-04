package okay.intent

import okay.agent.FileVersions

import okay.rag.{Embedding, embedding}
import java.nio.file.{Files, Paths}

/**
 * Is the ceiling the representation, or the task? (specs/intent-classify.md)
 *
 * The learning curve showed the probe and the centroid flattening
 * together at 32 examples, which rules out capacity and leaves two
 * candidates: the embedding carries no more signal, or the MESSAGES do
 * not — four classes that overlap, labelled by one person.
 *
 * No second embedding model is available on this machine (the gateway
 * answers 1024 dimensions whichever model is named), so the question
 * is asked two other ways.
 *
 * ONE, free: the recorded journal already holds the model tier's
 * prediction for every fixture message. If the probe and the model
 * fail on the SAME messages, both are hitting an ambiguity in the data
 * and no representation fixes it. If they fail on different ones,
 * there is signal one of them is missing.
 *
 * TWO: concatenate the embedding with chargram features — an
 * independent representation of the same message. If the ceiling is
 * the embedding's information content, an orthogonal signal should
 * lift it.
 */
class TestCeiling extends munit.FunSuite {

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

  /** the model tier's own answers, from the recording — no calls */
  private def modelSaid: Map[String, String] =
    val dir = Paths.get("okay-agent/src/test/resources/intent-journal")
    if !Files.isDirectory(dir) then Map.empty
    else
      import IntentFixture.Meeting
      given sM: okay.codec.Schema[Meeting] = summon[okay.codec.Schema[Meeting]]
      val mReading = Classify.reading[Meeting]
      new FileVersions(dir).all.headOption.map(_.entries).getOrElse(Vector.empty)
        .flatMap { e =>
          e.answer.flatMap { reply =>
            Classify.read[Meeting](reply)(using mReading).toOption
              .flatMap(_.spans.headOption)
              .flatMap(_.alts.headOption)
              .map(a => e.key -> IntentFixture.canonical.getOrElse(
                Classify.label(a.intent), Classify.label(a.intent)))
          }
        }.toMap

  test("live: do the probe and the model fail on the same messages") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val said = modelSaid
    assume(said.size >= 100, s"no usable recording (${said.size} entries)")

    val rows = IntentFixture.labelled
    val vecs = embed(rows.map(_._1))
    val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
    val (train, test) = all.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))
    val probe = Probe.train(train.map((_, v, c) => (v, c)))

    val judged = test.flatMap { (t, v, gold) =>
      for
        m <- said.get(t)
        p <- Probe.score(probe, v).map(_.best)
      yield (t, gold, m, p)
    }
    val modelWrong = judged.filter((_, g, m, _) => m != g).map(_._1).toSet
    val probeWrong = judged.filter((_, g, _, p) => p != g).map(_._1).toSet
    val both = modelWrong intersect probeWrong

    println(f"\n=== ${judged.length} messages judged by both ===")
    println(f"  model wrong on ${modelWrong.size}, probe wrong on ${probeWrong.size}, BOTH on ${both.size}")
    val overlap = if probeWrong.isEmpty then 0.0 else both.size * 100.0 / probeWrong.size
    println(f"  ${overlap}%.0f%% of the probe's errors are also the model's")
    println("  the messages both got wrong:")
    for t <- both.take(8) do println(s"    ${t.take(72)}")

    assert(judged.nonEmpty)
  }

  test("live: does an orthogonal representation lift the ceiling") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val vecs = embed(rows.map(_._1))
    val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
    val (train, test) = all.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    // the embedding, the chargrams, and both concatenated — one probe
    // over each, so only the representation changes
    val cgDim = 512
    def grams(t: String): Embedding =
      embedding(CharGrams.features(t, cgDim, 3, 5).map(_.toFloat))
    def both(t: String, v: Embedding): Embedding =
      embedding(Centroid.normalise(v).toArray ++ grams(t).toArray)

    def run(name: String, rep: (String, Embedding) => Embedding): Unit =
      val p = Probe.train(train.map((t, v, c) => (rep(t, v), c)))
      val right = test.count((t, v, g) => Probe.score(p, rep(t, v)).exists(_.best == g))
      println(f"  $name%-22s ${right * 100.0 / test.length}%5.1f%%")

    println("\n=== one probe, three representations ===")
    run("embedding only", (_, v) => v)
    run("chargrams only", (t, _) => grams(t))
    run("both concatenated", both)
    assert(test.nonEmpty)
  }

  /**
   * The free experiment: an INSTRUCTION prefix.
   *
   * Qwen3-Embedding is instruction-tuned — what it encodes depends on
   * what it is told the vector is for — and every measurement in this
   * programme embedded the bare message. If the ceiling is the
   * representation, then asking the same model for a
   * classification-shaped vector is the cheapest thing that could move
   * it, and it needs no second model installed.
   */
  test("live: does an instruction prefix change what the embedding is worth") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val prefixes = Seq(
      "" -> "bare message",
      "Classify the intent of this message: " -> "classify instruction",
      "Instruct: Given an email, identify whether it proposes a meeting, requests an action, notifies, or is unrelated.\nQuery: " -> "task instruction (e5 style)",
      "Represent this message for intent classification: " -> "represent-for instruction")

    println("\n=== one model, four framings ===")
    for (prefix, name) <- prefixes do
      val vecs = embed(rows.map((t, _) => prefix + t))
      val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
      val (train, test) = all.zipWithIndex.partition(_._2 % 2 == 1) match
        case (a, b) => (a.map(_._1), b.map(_._1))
      val probe = Probe.train(train.map((_, v, c) => (v, c)))
      val cen = Centroid.train(train.map((_, v, c) => (v, c)))
      val p = test.count((_, v, g) => Probe.score(probe, v).exists(_.best == g)) * 100.0 / test.length
      val c = test.count((_, v, g) => Centroid.score(cen, v).exists(_.best == g)) * 100.0 / test.length
      println(f"  $name%-28s probe $p%5.1f%%   centroid $c%5.1f%%")
    assert(rows.nonEmpty)
  }
}
