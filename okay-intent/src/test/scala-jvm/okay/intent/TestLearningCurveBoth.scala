package okay.intent

import okay.rag.{Embedding, embedding}

/**
 * The learning curve on BOTH embedders (specs/intent-classify.md,
 * intent-4b-with-more-data).
 *
 * The entry's prediction: the 4B embedder is worse at 60 examples
 * because 2560 dimensions need more of them — a prediction rather
 * than a defeat, so re-run the curve on both and find where the
 * lines cross. intent-instruction-prefix measured the bar a single
 * split cannot see past (ten probe points between the two halves of
 * this fixture), so the curve runs on both mirror splits, and a
 * crossing counts only if it holds on both.
 */
class TestLearningCurveBoth extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1800, "s")

  private val url = sys.env.getOrElse("OKAY_EMBED_URL", "http://127.0.0.1:8089/v1/embeddings")
  private val small = "mlx-community:Qwen3-Embedding-0.6B-4bit-DWQ"
  private val large = "mlx-community:Qwen3-Embedding-4B-4bit-DWQ"

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(url.replace("/embeddings", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def embed(model: String, texts: Seq[String]): Vector[Embedding] =
    texts.grouped(32).flatMap { batch =>
      val body = okay.codec.Json.JObj(Vector(
        "model" -> okay.codec.Json.JStr(model),
        "input" -> okay.codec.Json.JArr(batch.map(okay.codec.Json.JStr(_)).toVector)))
      val conn = java.net.URI.create(url).toURL.openConnection().asInstanceOf[java.net.HttpURLConnection]
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
          fields.collectFirst { case ("data", okay.codec.Json.JArr(rows)) => rows }.getOrElse(Vector.empty)
            .flatMap {
              case okay.codec.Json.JObj(row) =>
                row.collectFirst { case ("embedding", okay.codec.Json.JArr(xs)) =>
                  embedding(xs.collect { case okay.codec.Json.JNum(d) => d.toFloat }.toArray) }
              case _ => None
            }
        case _ => Vector.empty
    }.toVector

  private val sizes = Seq(8, 16, 24, 32, 40, 48, 60)

  /** probe and centroid at each n, class-balanced, for one split */
  private def curve(all: Seq[(String, Embedding, String)], testParity: Int): Seq[(Int, Double, Double)] =
    val (test, trainAll) = all.zipWithIndex.partition(_._2 % 2 == testParity) match
      case (a, b) => (a.map(_._1), b.map(_._1))
    def firstN(n: Int) = trainAll.groupBy(_._3).toSeq.sortBy(_._1).flatMap((_, rs) => rs.take(math.max(1, n / 4)))
    sizes.map { n =>
      val tr = firstN(n)
      val probe = Probe.train(tr.map((_, v, c) => (v, c)))
      val cen = Centroid.train(tr.map((_, v, c) => (v, c)))
      (tr.length,
        test.count((_, v, g) => Probe.score(probe, v).exists(_.best == g)) * 100.0 / test.length,
        test.count((_, v, g) => Centroid.score(cen, v).exists(_.best == g)) * 100.0 / test.length)
    }

  test("live: the learning curve on 0.6B and 4B, both mirror splits — where, if anywhere, the 4B overtakes") {
    assume(reachable, s"no embeddings endpoint at $url")
    val rows = IntentFixture.labelled
    println(s"\n=== learning curve, two vectorisers, ${rows.length} messages, both mirror splits ===")
    val curves = Seq(small -> Conditions.SmallEmbedder, large -> Conditions.LargeEmbedder).map { (model, name) =>
      val t0 = System.nanoTime()
      val vecs = embed(model, rows.map(_._1))
      assertEquals(vecs.length, rows.length, s"$name: the teacher returned a different number of vectors")
      println(f"  $name: ${vecs.head.length} dims, embedded in ${(System.nanoTime() - t0) / 1e9}%.0f s")
      val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
      val bySplit = Seq(1, 0).map(p => p -> curve(all, p))
      for (p, c) <- bySplit; (n, pr, ce) <- c do
        println(Conditions.line(Conditions(name, Conditions.Bare, n, rows.length / 2,
          extra = s"split=test-${if p == 1 then "odd" else "even"}"), f"n=$n%2d", f"probe $pr%5.1f%%  centroid $ce%5.1f%%"))
      name -> bySplit
    }
    // the two splits' means, side by side, and where the large one is ahead on BOTH
    val (sName, sCurves) = curves(0); val (lName, lCurves) = curves(1)
    println(f"\n  n    $sName%-22s $lName%-22s   4B ahead on both splits?")
    println("       probe   centroid       probe   centroid")
    for i <- sizes.indices do
      val n = sCurves(0)._2(i)._1
      def mean(cs: Seq[(Int, Seq[(Int, Double, Double)])], f: ((Int, Double, Double)) => Double) = cs.map(c => f(c._2(i))).sum / cs.length
      val sp = mean(sCurves, _._2); val sc = mean(sCurves, _._3)
      val lp = mean(lCurves, _._2); val lc = mean(lCurves, _._3)
      val aheadP = sCurves.zip(lCurves).forall((s, l) => l._2(i)._2 > s._2(i)._2)
      val aheadC = sCurves.zip(lCurves).forall((s, l) => l._2(i)._3 > s._2(i)._3)
      println(f"  $n%2d   $sp%5.1f%%   $sc%5.1f%%        $lp%5.1f%%   $lc%5.1f%%       probe ${if aheadP then "yes" else "no"}, centroid ${if aheadC then "yes" else "no"}")
    assert(rows.length >= 100)
  }
}
