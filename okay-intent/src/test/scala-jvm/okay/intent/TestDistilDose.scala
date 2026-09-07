package okay.intent

import okay.given
import okay.codec.{Json, Schema}
import okay.llm.{OpenAi, Transports}
import okay.rag.{Embedding, embedding}

import java.nio.file.{Files, Paths}

/**
 * The dose of distilled data, found properly (specs/intent-classify.md,
 * intent-distil-dose).
 *
 * intent-distil-for-probe found +40 distilled rows worth ten centroid
 * points and more worse, monotonically — an optimum found by accident
 * between two arms, on one split, with the self-consistency filter NOT
 * applied. Three questions, one run: (1) the dose, on a grid, on both
 * mirror splits; (2) whether the filter (the model re-judging its own
 * rows) moves the optimum; (3) whether the right knob is a dose at all
 * or a WEIGHT — distilled rows counted at less than one in the
 * centroid's mean, which a centroid can express and a grid over doses
 * cannot. The filter's verdicts are written beside the corpus once
 * (`intent-distilled-kept.json`), so the expensive half runs once.
 */
class TestDistilDose extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(3600, "s")

  private val embedUrl = sys.env.getOrElse("OKAY_EMBED_URL", "http://127.0.0.1:8089/v1/embeddings")
  private val llmUrl = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  private val llmModel = sys.env.getOrElse("OKAY_LLM_MODEL", "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  private val corpusFile = Paths.get("okay-agent/src/test/resources/intent-distilled.json")
  private val keptFile = Paths.get("okay-agent/src/test/resources/intent-distilled-kept.json")

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(embedUrl.replace("/embeddings", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def embed(texts: Seq[String]): Vector[Embedding] =
    texts.grouped(64).flatMap { batch =>
      val body = Json.JObj(Vector("input" -> Json.JArr(batch.map(Json.JStr(_)).toVector)))
      val conn = java.net.URI.create(embedUrl).toURL.openConnection().asInstanceOf[java.net.HttpURLConnection]
      conn.setRequestMethod("POST")
      conn.setRequestProperty("Content-Type", "application/json")
      conn.setDoOutput(true)
      conn.setConnectTimeout(10000)
      conn.setReadTimeout(300000)
      val out = conn.getOutputStream
      out.write(Json.print(body).getBytes("UTF-8"))
      out.close()
      val text = scala.io.Source.fromInputStream(conn.getInputStream, "UTF-8").mkString
      Json.parseValue(text) match
        case Json.JObj(fields) =>
          fields.collectFirst { case ("data", Json.JArr(rows)) => rows }.getOrElse(Vector.empty)
            .flatMap {
              case Json.JObj(row) =>
                row.collectFirst { case ("embedding", Json.JArr(xs)) =>
                  embedding(xs.collect { case Json.JNum(d) => d.toFloat }.toArray) }
              case _ => None
            }
        case _ => Vector.empty
    }.toVector

  final case class Corpus(rows: Vector[Phrasing]) derives Schema
  /** the filter's verdict, as data: the texts the model agreed with on second sight */
  final case class Kept(texts: Vector[String]) derives Schema

  private def readCorpus: Vector[Phrasing] =
    if !Files.exists(corpusFile) then Vector.empty
    else Json.decode(summon[Schema[Corpus]])(Json.parseValue(Files.readString(corpusFile))).map(_.rows).getOrElse(Vector.empty)

  private def ask(prompt: String, maxTokens: Int = 400): String =
    OpenAi.complete(Transports.http(), "none",
      OpenAi.request(llmModel, Seq(OpenAi.message("user", prompt)), maxTokens = Some(maxTokens)), llmUrl)
      .runWith.choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")

  /** the self-consistency filter, exactly as intent-label-distillation
   * ran it, its verdicts written once */
  private def kept(corpus: Vector[Phrasing]): Set[String] =
    if Files.exists(keptFile) then
      Json.decode(summon[Schema[Kept]])(Json.parseValue(Files.readString(keptFile))).map(_.texts.toSet).getOrElse(Set.empty)
    else
      val ks = corpus.grouped(10).flatMap { batch =>
        val numbered = batch.zipWithIndex.map((p, i) => s"${i + 1}. ${p.text}").mkString("\n")
        val reply = ask(
          s"For each numbered message, answer with its class on its own line as " +
          s"'<number>. <class>', nothing else. The classes are: " +
          s"${IntentFixture.classes.mkString(", ")}.\n\n$numbered")
        val said = reply.split("\n").iterator.flatMap { l =>
          raw"(\d+)\.\s*(\w+)".r.findFirstMatchIn(l).map(x => x.group(1).toInt -> x.group(2))
        }.toMap
        batch.zipWithIndex.collect { case (p, i) if said.get(i + 1).contains(p.cls) => p.text }
      }.toVector
      Files.writeString(keptFile, Json.write(Kept(ks))): Unit
      ks.toSet

  /** `d` distilled rows, round-robin over the classes in file order,
   * so a dose is as balanced as the corpus allows */
  private def dose(rows: Vector[(String, Embedding, String)], d: Int): Vector[(String, Embedding, String)] =
    val byClass = rows.groupBy(_._3).toVector.sortBy(_._1).map(_._2)
    val out = Vector.newBuilder[(String, Embedding, String)]
    var i = 0; var taken = 0
    while taken < d && byClass.exists(_.length > i) do
      for c <- byClass if c.length > i && taken < d do { out += c(i); taken += 1 }
      i += 1
    out.result()

  /** the centroid with the distilled rows weighted `w` in each class's
   * mean — `Centroid.train`'s fit, one weight added */
  private def weightedCentroid(human: Seq[(Embedding, String)], distilled: Seq[(Embedding, String)], w: Double): Centroid.Trained =
    val classes = (human ++ distilled).map(_._2).distinct.sorted
    val byClass = classes.map { cls =>
      val dim = human.head._1.length
      val acc = Array.fill(dim)(0.0f)
      for (v, c) <- human if c == cls do { val u = Centroid.normalise(v); var i = 0; while i < dim do { acc(i) += u(i); i += 1 } }
      for (v, c) <- distilled if c == cls do { val u = Centroid.normalise(v); var i = 0; while i < dim do { acc(i) += (u(i) * w).toFloat; i += 1 } }
      cls -> Centroid.normalise(embedding(acc))
    }.toMap
    Centroid.Trained(byClass, Taxon.parsed(classes.toVector))

  test("live: the dose on a grid, both splits, filtered and not; and a weight instead of a dose") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val corpus = readCorpus
    assume(corpus.length >= 100, s"only ${corpus.length} distilled rows on disk")
    val agreed = kept(corpus)
    println(f"\n=== distilled dose: ${corpus.length} rows, ${agreed.size} survive self-consistency ===")

    val human = IntentFixture.labelled
    val hv = embed(human.map(_._1))
    val dv = embed(corpus.map(_.text))
    assertEquals(hv.length, human.length); assertEquals(dv.length, corpus.length)
    val humanAll = human.zip(hv).map { case ((t, g), v) => (t, v, g) }
    val distAll = corpus.zip(dv).map { case (p, v) => (p.text, v, p.cls) }
    val distKept = distAll.filter((t, _, _) => agreed(t))

    val doses = Seq(0, 10, 20, 30, 40, 50, 60, 80, 100, 120, 160, 200, 320)
    def splitOf(parity: Int) = humanAll.zipWithIndex.partition(_._2 % 2 == parity) match
      case (a, b) => (a.map(_._1), b.map(_._1))   // (test, train)

    def sweep(name: String, pool: Vector[(String, Embedding, String)]): Seq[(Int, Double, Double)] =
      println(s"\n  -- $name (${pool.length} rows) --")
      println("  dose   probe(odd/even)   centroid(odd/even)    means")
      doses.filter(_ <= pool.length).map { d =>
        val cells = Seq(1, 0).map { p =>
          val (test, train) = splitOf(p)
          val tr = (train ++ dose(pool, d)).map((_, v, c) => (v, c))
          val probe = Probe.train(tr); val cen = Centroid.train(tr)
          (test.count((_, v, g) => Probe.score(probe, v).exists(_.best == g)) * 100.0 / test.length,
           test.count((_, v, g) => Centroid.score(cen, v).exists(_.best == g)) * 100.0 / test.length)
        }
        val mp = cells.map(_._1).sum / 2; val mc = cells.map(_._2).sum / 2
        println(f"  $d%4d   ${cells(0)._1}%5.1f / ${cells(1)._1}%5.1f      ${cells(0)._2}%5.1f / ${cells(1)._2}%5.1f       probe $mp%5.1f  centroid $mc%5.1f")
        (d, mp, mc)
      }

    val raw = sweep("unfiltered", distAll)
    val fil = sweep("self-consistent only", distKept)
    def best(s: Seq[(Int, Double, Double)], f: ((Int, Double, Double)) => Double) = s.maxBy(f)
    println(f"\n  best centroid dose: unfiltered ${best(raw, _._3)._1} (${best(raw, _._3)._3}%.1f%%), filtered ${best(fil, _._3)._1} (${best(fil, _._3)._3}%.1f%%); " +
            f"best probe dose: unfiltered ${best(raw, _._2)._1} (${best(raw, _._2)._2}%.1f%%), filtered ${best(fil, _._2)._1} (${best(fil, _._2)._2}%.1f%%)")

    println("\n  -- a weight instead of a dose: every distilled row, counted at w in the centroid's mean --")
    println("  pool          w     centroid(odd/even)   mean")
    for (name, pool) <- Seq("unfiltered" -> distAll, "filtered" -> distKept); w <- Seq(0.1, 0.25, 0.5, 1.0) do
      val cells = Seq(1, 0).map { p =>
        val (test, train) = splitOf(p)
        val cen = weightedCentroid(train.map((_, v, c) => (v, c)), pool.map((_, v, c) => (v, c)), w)
        test.count((_, v, g) => Centroid.score(cen, v).exists(_.best == g)) * 100.0 / test.length
      }
      println(f"  $name%-12s $w%4.2f   ${cells(0)}%5.1f / ${cells(1)}%5.1f        ${cells.sum / 2}%5.1f")
    println(Conditions.line(Conditions(Conditions.SmallEmbedder, Conditions.Bare, 60, 60,
      extra = s"corpus=human+distilled  distilled=${corpus.length}  kept=${agreed.size}"), "conditions", ""))
    assert(agreed.nonEmpty)
  }
}
