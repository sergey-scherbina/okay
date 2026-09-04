package okay.intent

import okay.rag.Embedding

/**
 * Is `Other` a class or a bin? (specs/intent-classify.md)
 *
 * A consumer's observation, and it reaches back through every
 * per-class number in this spec: `Other` holds social pleasantries
 * ("Happy birthday", "Thanks, that was helpful") and support
 * complaints ("charged twice", "the app crashes") and those share
 * nothing. A centroid over them is a point between two clouds and
 * belongs to neither; a probe's boundary for them is wherever the
 * mixture happened to fall.
 *
 * Three questions, in the order that makes each cheap.
 *
 * ONE: how much of the centroid's gap to the probe lives in `Other`
 * rows? If most of it does, the tier comparison this programme has
 * been running is largely a comparison of how two models cope with one
 * incoherent class.
 *
 * TWO: is `Other` actually two clusters? Measured, not asserted — the
 * mean distance within the pleasantries and within the complaints
 * against the distance between them.
 *
 * THREE: what happens if it is treated as a REJECTION rather than a
 * class — scored as abstention, the way the gate lane's in-domain
 * question already treats it?
 */
class TestOtherIsABin extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(900, "s")

  private val url = sys.env.getOrElse("OKAY_EMBED_URL",
    "http://127.0.0.1:8089/v1/embeddings")
  private val prefix = "Classify the intent of this message: "

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(url.replace("/embeddings", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def embed(texts: Seq[String]): Vector[Embedding] =
    if texts.isEmpty then Vector.empty
    else
      val body = okay.codec.Json.JObj(Vector(
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

  /** the split the consumer named: pleasantries against complaints */
  private val pleasantry = Set(
    "Happy birthday! Hope you have a great day.",
    "Thanks a lot, that was really helpful.",
    "Congratulations on the promotion, well deserved!",
    "Wishing you a restful holiday break.",
    "I loved the book you recommended.",
    "Great match yesterday, did you watch it?",
    "This newsletter is excellent, keep it up.",
    "Just finished the marathon, absolutely wrecked.",
    "Our cat had kittens over the weekend.",
    "Good morning! Coffee before we start?")

  test("live: how much of the centroid's gap lives in Other") {
    assume(reachable, s"no endpoint at $url")
    val rows = IntentFixture.labelled
    val vecs = rows.grouped(32).flatMap(g => embed(g.map((t, _) => prefix + t))).toVector
    val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
    val (train, test) = all.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    val probe = Probe.train(train.map((_, v, c) => (v, c)))
    val cen = Centroid.train(train.map((_, v, c) => (v, c)))

    def right(pred: Embedding => Option[String])(rowsIn: Seq[(String, Embedding, String)]) =
      rowsIn.count((_, v, g) => pred(v).contains(g))
    val pP = (v: Embedding) => Probe.score(probe, v).map(_.best)
    val pC = (v: Embedding) => Centroid.score(cen, v).map(_.best)

    val other = test.filter(_._3 == "Other")
    val rest = test.filterNot(_._3 == "Other")
    val gapAll = right(pP)(test) - right(pC)(test)
    val gapOther = right(pP)(other) - right(pC)(other)
    val gapRest = right(pP)(rest) - right(pC)(rest)

    println(f"\n=== the probe's lead over the centroid, by where it lives ===")
    println(f"  overall      probe ${right(pP)(test)}%2d / ${test.length}%2d   centroid ${right(pC)(test)}%2d   lead $gapAll%+d")
    println(f"  Other rows   probe ${right(pP)(other)}%2d / ${other.length}%2d   centroid ${right(pC)(other)}%2d   lead $gapOther%+d")
    println(f"  the rest     probe ${right(pP)(rest)}%2d / ${rest.length}%2d   centroid ${right(pC)(rest)}%2d   lead $gapRest%+d")
    assert(test.nonEmpty)
  }

  test("live: is Other two clusters or one") {
    assume(reachable, s"no endpoint at $url")
    val others = IntentFixture.labelled.filter(_._2 == "Other").map(_._1)
    val vecs = embed(others.map(prefix + _)).map(Centroid.normalise)
    val tagged = others.zip(vecs).map((t, v) => (pleasantry.contains(t), v))
    val a = tagged.filter(_._1).map(_._2)
    val b = tagged.filterNot(_._1).map(_._2)
    assume(a.length >= 3 && b.length >= 3, s"${a.length} pleasantries, ${b.length} complaints")

    def mean(xs: Seq[Embedding], ys: Seq[Embedding]) =
      val ps = for x <- xs; y <- ys if !(x eq y) yield Centroid.dot(x, y)
      if ps.isEmpty then 0.0 else ps.sum / ps.length

    println(f"\n=== Other, as two named halves (${a.length} pleasantries, ${b.length} complaints) ===")
    println(f"  within pleasantries  ${mean(a, a)}%.3f")
    println(f"  within complaints    ${mean(b, b)}%.3f")
    println(f"  across the two       ${mean(a, b)}%.3f")
    // for scale: how tight is a class nobody disputes?
    for cls <- Seq("Proposal", "Request", "Notification") do
      val ts = IntentFixture.labelled.filter(_._2 == cls).map(_._1)
      val vs = embed(ts.map(prefix + _)).map(Centroid.normalise)
      println(f"  within $cls%-13s ${mean(vs, vs)}%.3f")
    assert(a.nonEmpty)
  }

  test("live: Other as a rejection rather than a class") {
    assume(reachable, s"no endpoint at $url")
    val rows = IntentFixture.labelled
    val vecs = rows.grouped(32).flatMap(g => embed(g.map((t, _) => prefix + t))).toVector
    val all = rows.zip(vecs).map { case ((t, g), v) => (t, v, g) }
    val (train, test) = all.zipWithIndex.partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

    // fitted on the THREE real classes only; Other never appears
    val positive = train.filterNot(_._3 == "Other")
    val probe = Probe.train(positive.map((_, v, c) => (v, c)))
    val cen = Centroid.train(positive.map((_, v, c) => (v, c)))

    def sweep(name: String, floors: Seq[Double], conf: Embedding => (String, Double)): Unit =
      println(f"\n  [$name] Other as abstention, three classes fitted")
      for floor <- floors do
        val calls = test.map { (_, v, gold) =>
          val (best, margin) = conf(v)
          val said = if margin >= floor then best else "Other"
          (gold, said)
        }
        val acc = calls.count((g, s) => g == s) * 100.0 / calls.length
        val otherRecall =
          val o = calls.filter(_._1 == "Other")
          if o.isEmpty then 0.0 else o.count(_._2 == "Other") * 100.0 / o.length
        println(f"    floor $floor%.2f   accuracy $acc%5.1f%%   Other recall $otherRecall%5.1f%%")

    // the floors are NOT comparable across the two: a probe margin is a
    // difference of PROBABILITIES and a centroid margin a difference of
    // COSINES, which live on different scales — sharing one sweep made
    // the centroid abstain on everything above 0.1 and reported 25%
    // accuracy as if it were a finding. Each gets the range its own
    // measure occupies.
    sweep("probe", Seq(0.0, 0.1, 0.2, 0.3, 0.5),
      v => Probe.score(probe, v).map(x => (x.best, x.margin)).getOrElse(("", 0.0)))
    sweep("centroid", Seq(0.0, 0.01, 0.02, 0.05, 0.10),
      v => Centroid.score(cen, v).map(x => (x.best, x.margin)).getOrElse(("", 0.0)))
    assert(test.nonEmpty)
  }
}
