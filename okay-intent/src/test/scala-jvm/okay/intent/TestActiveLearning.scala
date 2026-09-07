package okay.intent

import okay.rag.{Embedding, embedding}

/**
 * Labels chosen by uncertainty rather than by order
 * (specs/intent-classify.md, intent-active-learning).
 *
 * "Labels are the bottleneck everywhere above, so choose the next ones
 * to label by uncertainty rather than by order." Simulated on the
 * fixture, whose labels are all known: the pool is the training half,
 * the seed is eight rows (two per class), and each round adds four
 * more from the pool — chosen by the probe's smallest MARGIN over the
 * unlabelled pool (the classical uncertainty sampling), or by ORDER
 * (the fixture's, what the learning curve did), or at RANDOM (five
 * seeds, the mean). The probe and the centroid are refitted each round
 * and scored on the held-out half; both mirror splits. The number is
 * the area between the curves: how many labels uncertainty saves for
 * the same accuracy, or does not. One embedding of the fixture; no
 * model in the loop after it.
 */
class TestActiveLearning extends munit.FunSuite {

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
      val body = okay.codec.Json.JObj(Vector("input" -> okay.codec.Json.JArr(batch.map(okay.codec.Json.JStr(_)).toVector)))
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
      okay.codec.Json.parse(text) match
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

  private type Row = (String, Embedding, String)
  private val step = 4
  private val seedPerClass = 2

  private def accuracy(train: Seq[Row], test: Seq[Row]): (Double, Double) =
    val probe = Probe.train(train.map((_, v, c) => (v, c)))
    val cen = Centroid.train(train.map((_, v, c) => (v, c)))
    (test.count((_, v, g) => Probe.score(probe, v).exists(_.best == g)) * 100.0 / test.length,
     test.count((_, v, g) => Centroid.score(cen, v).exists(_.best == g)) * 100.0 / test.length)

  /** one labelling run: the seed, then rounds of `step` from the pool by `pick` */
  private def run(pool: Seq[Row], test: Seq[Row], pick: (Probe.Trained, Seq[Row]) => Seq[Row]): Seq[(Int, Double, Double)] =
    val seed = pool.groupBy(_._3).toSeq.sortBy(_._1).flatMap(_._2.take(seedPerClass))
    var labelled = seed.toVector
    var rest = pool.filterNot(labelled.contains).toVector
    val out = Vector.newBuilder[(Int, Double, Double)]
    val (p0, c0) = accuracy(labelled, test); out += ((labelled.length, p0, c0))
    while rest.nonEmpty do
      val probe = Probe.train(labelled.map((_, v, c) => (v, c)))
      val chosen = pick(probe, rest).take(step)
      labelled = labelled ++ chosen
      rest = rest.filterNot(chosen.contains)
      val (p, c) = accuracy(labelled, test); out += ((labelled.length, p, c))
    out.result()

  private def byUncertainty(probe: Probe.Trained, rest: Seq[Row]): Seq[Row] =
    rest.sortBy((_, v, _) => Probe.score(probe, v).map(_.margin).getOrElse(0.0))
  private def byOrder(@annotation.unused probe: Probe.Trained, rest: Seq[Row]): Seq[Row] = rest
  private def byRandom(seed: Int)(@annotation.unused probe: Probe.Trained, rest: Seq[Row]): Seq[Row] = new scala.util.Random(seed).shuffle(rest)

  test("live: uncertainty sampling against order and random, both mirror splits — what a chosen label is worth") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val rows = IntentFixture.labelled
    val vecs = embed(rows.map(_._1))
    assertEquals(vecs.length, rows.length)
    val all: Seq[Row] = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
    println(s"\n=== active learning on ${rows.length} messages: seed ${seedPerClass * 4}, +$step a round, both mirror splits ===")

    val curves = Seq(1, 0).map { parity =>
      val (test, pool) = all.zipWithIndex.partition(_._2 % 2 == parity) match
        case (a, b) => (a.map(_._1), b.map(_._1))
      val unc = run(pool, test, byUncertainty)
      val ord = run(pool, test, byOrder)
      val rnd = (1 to 5).map(s => run(pool, test, byRandom(s)))
      val rndMean = unc.indices.map(i => (unc(i)._1, rnd.map(_(i)._2).sum / rnd.length, rnd.map(_(i)._3).sum / rnd.length))
      println(s"\n  -- split test-${if parity == 1 then "odd" else "even"} --")
      println("  n     uncertainty        order              random(5)          probe: unc-rnd")
      for i <- unc.indices do
        println(f"  ${unc(i)._1}%2d    ${unc(i)._2}%5.1f / ${unc(i)._3}%5.1f      ${ord(i)._2}%5.1f / ${ord(i)._3}%5.1f      ${rndMean(i)._2}%5.1f / ${rndMean(i)._3}%5.1f      ${unc(i)._2 - rndMean(i)._2}%+5.1f")
      (unc, ord, rndMean)
    }
    // the area between the probe curves (mean over rounds), both splits
    def area(a: Seq[(Int, Double, Double)], b: Seq[(Int, Double, Double)]) = a.zip(b).map((x, y) => x._2 - y._2).sum / a.length
    val vsRandom = curves.map((u, _, r) => area(u, r)); val vsOrder = curves.map((u, o, _) => area(u, o))
    println(f"\n  probe, mean over rounds: uncertainty - random ${vsRandom(0)}%+.1f / ${vsRandom(1)}%+.1f; uncertainty - order ${vsOrder(0)}%+.1f / ${vsOrder(1)}%+.1f")
    // labels needed to reach 80% probe accuracy, per strategy and split
    def reach(c: Seq[(Int, Double, Double)]) = c.find(_._2 >= 80.0).map(_._1.toString).getOrElse("never")
    println(s"  labels to reach 80% probe: uncertainty ${curves.map(c => reach(c._1)).mkString(" / ")}, order ${curves.map(c => reach(c._2)).mkString(" / ")}, random ${curves.map(c => reach(c._3)).mkString(" / ")}")
    println(Conditions.line(Conditions(Conditions.SmallEmbedder, Conditions.Bare, 60, 60, extra = s"seed=${seedPerClass * 4} step=$step random-seeds=5"), "conditions", ""))
    assert(curves.nonEmpty)
  }
}
