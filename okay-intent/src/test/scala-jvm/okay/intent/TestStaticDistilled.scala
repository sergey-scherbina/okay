package okay.intent

import okay.codec.{Json, Schema}
import okay.rag.{Embedding, embedding}

import java.nio.file.{Files, Paths}

/**
 * The static table fed with the distilled corpus (specs/intent-classify.md,
 * intent-distil-more, the static half).
 *
 * intent-distil-more asked two things: more generated rows (machine
 * time, and intent-distil-dose has since shown the dose's gain was one
 * split's), and "worth trying on the STATIC table, which was not fed
 * here because its vocabulary would have to be re-embedded — a second
 * pass over the teacher rather than a change of method". This is that
 * second pass. A static table has two inputs a corpus can feed: its
 * VOCABULARY (which units exist, embedded once by the teacher) and its
 * WEIGHTS (the SIF counts); and the classifier over it has a third,
 * its TRAINING ROWS. The distilled corpus is tried at each, on both
 * mirror splits, against the best table so far (words + pairs +
 * triples at PCA 256, intent-static-trigrams-and-pca).
 */
class TestStaticDistilled extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1800, "s")

  private val embedUrl = sys.env.getOrElse("OKAY_EMBED_URL", "http://127.0.0.1:8089/v1/embeddings")
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
  final case class Kept(texts: Vector[String]) derives Schema

  private def distilled: Vector[(String, String)] =
    val rows = Json.decode(summon[Schema[Corpus]])(Json.parseValue(Files.readString(corpusFile))).map(_.rows).getOrElse(Vector.empty)
    val kept = if Files.exists(keptFile) then
      Json.decode(summon[Schema[Kept]])(Json.parseValue(Files.readString(keptFile))).map(_.texts.toSet).getOrElse(Set.empty)
      else rows.map(_.text).toSet
    rows.filter(p => kept(p.text)).map(p => p.text -> p.cls)

  private final case class Cell(probe: Double, centroid: Double)

  private def score(tbl: Static.Table, train: Seq[(String, String)], test: Seq[(String, String)]): Cell =
    val tr = train.flatMap((t, g) => Static.encode(tbl, t).map(v => (v, g)))
    val probe = Probe.train(tr); val cen = Centroid.train(tr)
    val enc = test.map((t, g) => (Static.encode(tbl, t), g))
    Cell(enc.count((v, g) => v.flatMap(Probe.score(probe, _)).exists(_.best == g)) * 100.0 / test.length,
         enc.count((v, g) => v.flatMap(Centroid.score(cen, _)).exists(_.best == g)) * 100.0 / test.length)

  test("live: the distilled corpus as vocabulary, as weights, and as training rows for the static table, both splits") {
    assume(reachable, s"no embeddings endpoint at $embedUrl")
    val human = IntentFixture.labelled
    val dist = distilled
    assume(dist.length >= 100, s"only ${dist.length} distilled rows on disk")
    println(s"\n=== static table fed with ${dist.length} distilled rows (self-consistent), ${human.length} human ===")

    // one distillation: every unit anyone wrote, human or generated
    val humanUnits = human.map(_._1).flatMap(Static.units3).distinct
    val allUnits = (human.map(_._1) ++ dist.map(_._1)).flatMap(Static.units3).distinct.sorted
    val t0 = System.nanoTime()
    val vecs = embed(allUnits)
    assertEquals(vecs.length, allUnits.length, "the teacher returned a different number of vectors")
    val all = allUnits.zip(vecs).toMap
    println(f"  ${allUnits.length} units distilled in ${(System.nanoTime() - t0) / 1e9}%.1f s (${humanUnits.length} from the human fixture)")

    def table(vocab: Map[String, Embedding], corpus: Seq[String]): Static.Table =
      val t = Static.table(vocab, corpus, split = Static.units3)
      Static.projected(t, Static.fitPca(t.vectors.values, 256))

    val humanVocab = all.filter((k, _) => humanUnits.contains(k))
    val arms: Seq[(String, () => Static.Table, Boolean)] = Seq(
      ("human vocab, human weights (baseline)", () => table(humanVocab, human.map(_._1)), false),
      ("+ distilled VOCABULARY", () => table(all, human.map(_._1)), false),
      ("+ distilled vocabulary and WEIGHTS", () => table(all, human.map(_._1) ++ dist.map(_._1)), false),
      ("+ distilled vocabulary, weights, and 20 TRAINING rows", () => table(all, human.map(_._1) ++ dist.map(_._1)), true))

    println("  arm                                                   units   KB    probe(odd/even)   centroid(odd/even)    means")
    for (name, mk, withRows) <- arms do
      val tbl = mk()
      val cells = Seq(1, 0).map { p =>
        val (test, train) = human.zipWithIndex.partition(_._2 % 2 == p) match
          case (a, b) => (a.map(_._1), b.map(_._1))
        val extra = if withRows then dist.groupBy(_._2).toSeq.sortBy(_._1).flatMap(_._2.take(5)) else Nil
        score(tbl, train ++ extra, test)
      }
      println(f"  $name%-52s ${tbl.size}%5d ${tbl.size.toLong * tbl.dim * 4 / 1024}%5d   ${cells(0).probe}%5.1f / ${cells(1).probe}%5.1f      ${cells(0).centroid}%5.1f / ${cells(1).centroid}%5.1f      probe ${(cells(0).probe + cells(1).probe) / 2}%5.1f  centroid ${(cells(0).centroid + cells(1).centroid) / 2}%5.1f")
    println(Conditions.line(Conditions(Conditions.SmallEmbedder, Conditions.Bare, 60, 60,
      extra = s"table=units3@pca256  distilled=${dist.length}(kept)"), "conditions", ""))
    assert(allUnits.length > humanUnits.length)
  }
}
