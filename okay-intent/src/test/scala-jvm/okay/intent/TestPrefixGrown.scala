package okay.intent

import okay.rag.{Embedding, embedding}

/**
 * The instruction prefix, re-measured on the grown fixture
 * (specs/intent-classify.md, intent-instruction-prefix).
 *
 * The first measurement (TestCeiling, 60 messages) found +1.6 (probe)
 * and +3.3 (centroid) for "Classify the intent of this message: ", a
 * 6.6-point spread across four framings, and a cost for the LONG
 * e5-style instruction — all at or near the noise floor. The entry's
 * own ask: re-measure on the grown fixture before any default moves.
 *
 * Same four framings, same probe/centroid, the same model — and BOTH
 * mirror splits (train odd / test even, then the reverse), so the
 * split-to-split spread of the bare framing is measured in the same
 * run and is the bar a framing's gain has to clear on both splits.
 */
class TestPrefixGrown extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(900, "s")

  private val embedUrl = sys.env.getOrElse("OKAY_EMBED_URL", "http://127.0.0.1:8089/v1/embeddings")

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(embedUrl.replace("/embeddings", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def embed(texts: Seq[String]): Vector[Embedding] =
    texts.grouped(64).flatMap { batch =>
      val body = okay.codec.Json.JObj(Vector(
        "input" -> okay.codec.Json.JArr(batch.map(okay.codec.Json.JStr(_)).toVector)))
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
    }.toVector

  private val framings = Seq(
    "" -> "bare",
    "Classify the intent of this message: " -> "classify-instruction",
    "Instruct: Given an email, identify whether it proposes a meeting, requests an action, notifies, or is unrelated.\nQuery: " -> "task-instruction-e5",
    "Represent this message for intent classification: " -> "represent-for")

  private final case class Cell(probe: Double, centroid: Double)

  test("live: the four framings on the grown fixture, both mirror splits, the bare spread as the bar") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    println(s"\n=== instruction prefix, grown fixture: ${rows.length} messages, four framings, two mirror splits ===")

    val results: Seq[(String, Seq[Cell])] = framings.map { (prefix, name) =>
      val vecs = embed(rows.map((t, _) => prefix + t))
      assertEquals(vecs.length, rows.length, s"$name: the teacher returned a different number of vectors")
      val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
      val cells = Seq(1, 0).map { testParity =>
        val (test, train) = all.zipWithIndex.partition(_._2 % 2 == testParity) match
          case (a, b) => (a.map(_._1), b.map(_._1))
        val probe = Probe.train(train.map((_, v, c) => (v, c)))
        val cen = Centroid.train(train.map((_, v, c) => (v, c)))
        val cell = Cell(
          test.count((_, v, g) => Probe.score(probe, v).exists(_.best == g)) * 100.0 / test.length,
          test.count((_, v, g) => Centroid.score(cen, v).exists(_.best == g)) * 100.0 / test.length)
        val c = Conditions(Conditions.SmallEmbedder, name, train.length, test.length,
          extra = s"split=test-${if testParity == 1 then "odd" else "even"}")
        println(Conditions.line(c, name, f"probe ${cell.probe}%5.1f%%  centroid ${cell.centroid}%5.1f%%"))
        cell
      }
      name -> cells
    }

    val bare = results.head._2
    val bareSpreadP = math.abs(bare(0).probe - bare(1).probe)
    val bareSpreadC = math.abs(bare(0).centroid - bare(1).centroid)
    println(f"  bare, split to split: probe ${bareSpreadP}%.1f points apart, centroid ${bareSpreadC}%.1f — the bar")
    for (name, cells) <- results.tail do
      val dp = cells.zip(bare).map((c, b) => c.probe - b.probe)
      val dc = cells.zip(bare).map((c, b) => c.centroid - b.centroid)
      val clearsP = dp.forall(_ > bareSpreadP) && dp.forall(_ > 0)
      val clearsC = dc.forall(_ > bareSpreadC) && dc.forall(_ > 0)
      println(f"  $name%-22s over bare: probe ${dp(0)}%+5.1f / ${dp(1)}%+5.1f  centroid ${dc(0)}%+5.1f / ${dc(1)}%+5.1f  " +
              f"-> ${if clearsP then "probe clears" else "probe does not clear"}, ${if clearsC then "centroid clears" else "centroid does not clear"}")
    assert(rows.length >= 100, s"the fixture has ${rows.length} rows — not grown")
  }
}
