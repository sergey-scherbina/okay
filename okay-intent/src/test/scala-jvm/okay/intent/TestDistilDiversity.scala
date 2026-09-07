package okay.intent

import okay.codec.{Json, Schema}

import java.nio.file.{Files, Paths}

/**
 * How diverse is the distilled corpus, measured rather than felt
 * (specs/intent-classify.md, intent-distil-diversity).
 *
 * The entry: the distilled corpus alone scores ten points below a human
 * fixture a third its size, which says its distribution is narrow
 * rather than its labels wrong; prompting for a persona or a register
 * is the cheap thing to try, and "measuring the corpus's own diversity
 * (say, distinct trigram ratio against the human fixture's) is the
 * honest way to tell whether it worked". This is the measurement, so
 * that a later corpus — generated any way — has a number to beat
 * before it is embedded. No teacher: the corpus and the fixture are
 * on disk, and the statistics are counts.
 *
 * Distinct-n ratios are length-sensitive (more text, more repeats), so
 * every human-vs-distilled comparison is taken on SAMPLES OF EQUAL
 * SIZE, drawn by a fixed seed and averaged over draws.
 */
class TestDistilDiversity extends munit.FunSuite {

  private val corpusFile = Paths.get("okay-agent/src/test/resources/intent-distilled.json")
  private val keptFile = Paths.get("okay-agent/src/test/resources/intent-distilled-kept.json")

  final case class Corpus(rows: Vector[Phrasing]) derives Schema
  final case class Kept(texts: Vector[String]) derives Schema

  private def corpus: Vector[(String, String)] =
    if !Files.exists(corpusFile) then Vector.empty
    else Json.decode(summon[Schema[Corpus]])(Json.parse(Files.readString(corpusFile))).map(_.rows).getOrElse(Vector.empty).map(p => p.text -> p.cls)

  private def kept: Set[String] =
    if !Files.exists(keptFile) then Set.empty
    else Json.decode(summon[Schema[Kept]])(Json.parse(Files.readString(keptFile))).map(_.texts.toSet).getOrElse(Set.empty)

  private def words(t: String): Vector[String] = Static.tokens(t)

  /** distinct n-grams over all n-grams, pooled over the texts */
  private def distinctN(texts: Seq[String], n: Int): Double =
    val grams = texts.flatMap(t => words(t).sliding(n).filter(_.length == n).map(_.mkString(" ")))
    if grams.isEmpty then 0.0 else grams.distinct.length.toDouble / grams.length

  private final case class Stats(rows: Int, words: Double, distinct1: Double, distinct2: Double, distinct3: Double,
                                 vocab: Int, openers: Double)

  /** the statistics of a sample; `openers` is how many distinct
   * first-two-words the texts start with, per text — the register's
   * signature (a corpus that begins every message the same way is
   * narrow however its middles vary) */
  private def stats(texts: Seq[String]): Stats =
    Stats(texts.length,
      texts.map(t => words(t).length.toDouble).sum / math.max(texts.length, 1),
      distinctN(texts, 1), distinctN(texts, 2), distinctN(texts, 3),
      texts.flatMap(words).distinct.length,
      texts.map(t => words(t).take(2).mkString(" ")).distinct.length.toDouble / math.max(texts.length, 1))

  /** equal-size samples, several draws, the mean of each statistic */
  private def sampled(texts: Seq[String], size: Int, draws: Int = 20): Stats =
    val rnd = new scala.util.Random(11)
    val ss = (1 to draws).map(_ => stats(rnd.shuffle(texts).take(size)))
    def m(f: Stats => Double) = ss.map(f).sum / ss.length
    Stats(size, m(_.words), m(_.distinct1), m(_.distinct2), m(_.distinct3), m(_.vocab.toDouble).round.toInt, m(_.openers))

  private def line(name: String, s: Stats): String =
    f"  $name%-34s rows ${s.rows}%4d  words/msg ${s.words}%5.1f  distinct-1 ${s.distinct1 * 100}%5.1f%%  distinct-2 ${s.distinct2 * 100}%5.1f%%  distinct-3 ${s.distinct3 * 100}%5.1f%%  vocab ${s.vocab}%4d  openers/msg ${s.openers * 100}%5.1f%%"

  test("the distilled corpus against the human fixture, on equal-size samples: distinct n-grams, vocabulary, openers") {
    val human = IntentFixture.labelled
    val dist = corpus
    assume(dist.length >= 100, s"only ${dist.length} distilled rows on disk")
    val keptSet = kept
    val distKept = dist.filter((t, _) => keptSet(t))
    val size = math.min(human.length, distKept.length)
    println(s"\n=== diversity: ${human.length} human rows, ${dist.length} distilled (${distKept.length} self-consistent); samples of $size, 20 draws ===")
    println(line("human fixture", sampled(human.map(_._1), size)))
    println(line("distilled, all", sampled(dist.map(_._1), size)))
    println(line("distilled, self-consistent", sampled(distKept.map(_._1), size)))
    println("\n  per class, human vs distilled (self-consistent), samples of the smaller side:")
    for c <- IntentFixture.classes do
      val h = human.filter(_._2 == c).map(_._1); val d = distKept.filter(_._2 == c).map(_._1)
      val n = math.min(h.length, d.length)
      if n >= 5 then
        println(line(s"  $c, human", sampled(h, n)))
        println(line(s"  $c, distilled", sampled(d, n)))
    // what the generated text shares with the human text: the share of
    // the distilled vocabulary the fixture already has, and the reverse
    val hv = human.map(_._1).flatMap(words).toSet; val dv = distKept.map(_._1).flatMap(words).toSet
    println(f"\n  vocabulary: human ${hv.size}, distilled ${dv.size}, shared ${(hv intersect dv).size} — ${(hv intersect dv).size * 100.0 / dv.size}%.0f%% of the distilled words are the fixture's, ${(hv intersect dv).size * 100.0 / hv.size}%.0f%% of the fixture's are in the corpus")
    val topOpeners = distKept.map((t, _) => words(t).take(2).mkString(" ")).groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2).take(6)
    println(s"  the distilled corpus's commonest openers: ${topOpeners.map((o, n) => s"'$o' x$n").mkString(", ")}")
    val humanOpeners = human.map((t, _) => words(t).take(2).mkString(" ")).groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2).take(6)
    println(s"  the human fixture's commonest openers:    ${humanOpeners.map((o, n) => s"'$o' x$n").mkString(", ")}")
    assert(human.nonEmpty && distKept.nonEmpty)
  }
}
