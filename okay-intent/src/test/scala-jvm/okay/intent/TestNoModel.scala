package okay.intent

import okay.rag.Embedding

/**
 * The assembled no-generation classifier (specs/intent-classify.md).
 *
 * Three splits, not two, and that is the point: the probe is FITTED on
 * one, the blend weight and the abstention threshold are CALIBRATED on
 * a second, and everything is reported on a third that neither has
 * seen. A threshold chosen on the data the model was fitted to
 * promises an accuracy nobody will observe.
 */
class TestNoModel extends munit.FunSuite {

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

  test("live: the assembled classifier, fitted, calibrated and reported on three disjoint splits") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val vecs = embed(rows.map(_._1))
    assertEquals(vecs.length, rows.length)
    val all = rows.zip(vecs).map { case ((text, gold), v) => (text, v, gold) }

    val fit = all.zipWithIndex.collect { case (r, i) if i % 3 == 0 => r }
    val cal = all.zipWithIndex.collect { case (r, i) if i % 3 == 1 => r }
    val eva = all.zipWithIndex.collect { case (r, i) if i % 3 == 2 => r }
    println(s"\n=== ${fit.length} fit / ${cal.length} calibrate / ${eva.length} report ===")
    assert((fit.map(_._1).toSet intersect eva.map(_._1).toSet).isEmpty)
    assert((cal.map(_._1).toSet intersect eva.map(_._1).toSet).isEmpty)

    val model = NoModel.fit(fit, cal)
    println(f"  pattern weight ${model.patternWeight}%.2f, threshold ${model.threshold}%.3f")
    model.promise match
      case Some(p) =>
        println(f"  PROMISE: at least ${p * 100}%.0f%% right, over " +
                f"${model.observedCoverage * 100}%.1f%% of calibration messages")
      case None =>
        println(f"  NO PROMISE: ${model.calibrationErrors} calibration errors, " +
                f"${model.errorsNeeded} needed for a bound at this error rate — " +
                f"the threshold still applies, the guarantee does not")

    // forced: what it says when it must say something
    val forced = eva.map((t, v, g) => (g, NoModel.force(model, t, v)))
    val forcedRight = forced.count((g, p) => p.contains(g))
    println(f"  DELIVERED at full coverage: ${forcedRight * 100.0 / eva.length}%.1f%%")

    // abstaining: the promise, kept or not
    val answered = eva.flatMap((t, v, g) => NoModel.classify(model, t, v).map(x => (g, x.best)))
    val acc = if answered.isEmpty then 0.0 else answered.count((g, b) => g == b) * 100.0 / answered.length
    println(f"  DELIVERED abstaining:      $acc%.1f%% right over " +
            f"${answered.length * 100.0 / eva.length}%.1f%% of messages")

    // the probe alone, for the comparison that decides whether the
    // assembly earned anything
    val probeOnly = Probe.train(fit.map((_, v, c) => (v, c)))
    val pRight = eva.count((_, v, g) => Probe.score(probeOnly, v).exists(_.best == g))
    println(f"  probe alone, same fit split: ${pRight * 100.0 / eva.length}%.1f%%")

    // the diagnosis behind the default: what the grid WOULD have
    // picked, and what each weight actually costs on held-out data
    println("  weight sweep on the report split (the grid never sees this):")
    for w <- Seq(0.0, 0.1, 0.3, 0.5, 0.8) do
      val m = NoModel.fit(fit, cal, weights = Seq(w))
      val right = eva.count((t, v, g) => NoModel.force(m, t, v).contains(g))
      println(f"    weight $w%.1f -> ${right * 100.0 / eva.length}%5.1f%% at full coverage")

    assert(model.threshold >= 0.0)
    assert(forced.length == eva.length)
  }
}
