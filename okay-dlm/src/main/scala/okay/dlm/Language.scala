package okay.dlm

import okay.codec.Json
import okay.codec.Json.*
import okay.rag.{Embedding, Vectors, embedding}
import java.nio.file.{Files, Path}

/**
 * Which language to answer in (specs/dlm.md, "Language").
 *
 * Detection is deliberately shallow: the alphabet first, then the
 * letters and short words that separate the languages sharing one,
 * and only then a character-trigram nearest neighbour over the
 * authored phrasings themselves. A turn is often three words long,
 * and no statistical detector is reliable at that length — so a
 * message that carries no evidence does not GUESS, it answers `None`,
 * and the caller falls back to what this person wrote last.
 *
 * Languages are CODES, as in `okay.frame`: a caller with an enum
 * passes its code, and a code can be written down in a journal, which
 * an opaque type cannot.
 */
object Language:

  /**
   * THE RULE LAYER: letters that exist in one language only, and the
   * everyday words that split the languages sharing a script — the
   * shortest messages a service gets are greetings, which is exactly
   * where a word list has to work and a trigram detector cannot.
   *
   * The words and the letters are the caller's data; the counting is
   * here. Languages of a different script than the text's are never
   * candidates (`alphabet`), and among the rest the one with STRICTLY
   * the most evidence wins — a tie is `None`.
   *
   * @param ignore a pattern taken out before the letters are counted:
   *               a price with its currency («40 zł») is written the
   *               same way in every language and is evidence of none
   */
  final case class Cues(alphabet: Alphabet,
                        words: Map[String, Set[String]],
                        ignore: Option[scala.util.matching.Regex] = None):

    def of(text: String): Option[String] =
      val t = ignore.fold(text.toLowerCase)(_.replaceAllIn(text.toLowerCase, " "))
      val ws = t.split("[^\\p{L}]+").toVector.filter(_.nonEmpty)
      val candidates = Script.native(t) match
        case Some(script) if !alphabet.isEmpty => alphabet.of(script)
        case _ => alphabet.languages.keySet ++ words.keySet
      val scored = candidates.toVector.map { l =>
        l -> (t.count(alphabet.marks.getOrElse(l, Set.empty)) + ws.count(words.getOrElse(l, Set.empty)))
      }.sortBy(-_._2)
      scored match
        case (best, n) +: rest if n > 0 && rest.headOption.forall(_._2 < n) => Some(best)
        case _ => None

  /**
   * THE TRIGRAM LAYER: a detector built from the authored phrasings
   * themselves.
   *
   * `Vectors.hashing` is a character-trigram stand-in embedder. It is
   * useless for MEANING, which is why nothing routes on it — and it is
   * exactly right for LANGUAGE, which is a question about character
   * sequences and nothing else. One vector per authored phrase, and a
   * language scores by its NEAREST one: summing a language into a
   * single profile lets the biggest class win on shared trigrams
   * alone.
   */
  final class Detector(val profiles: Vector[(String, Embedding)],
                       margin: Float = 0.03f, floor: Float = 0.5f,
                       strong: Float = 0.9f, minLetters: Int = 12,
                       alphabet: Alphabet = Alphabet.none):

    /** an empty detector claims nothing; the caller falls back */
    def nonEmpty: Boolean = profiles.nonEmpty
    private val f = Vectors.hashing()

    def scores(text: String): Vector[(String, Float)] =
      val q = f(text)
      profiles.groupBy(_._1).map((l, vs) => l -> vs.map((_, v) => Vectors.cosine(q, v)).max)
        .toVector.sortBy(-_._2)

    /**
     * Three conditions, and the short-text rule has one exception.
     *
     * Trigram evidence needs text to be evidence OF: two tokens are
     * not enough, and the caller has something better, namely what
     * this person wrote a moment ago. But a SHORT message that matches
     * an authored phrase almost exactly is not a guess, so a strong
     * enough score overrides the length rule and nothing else does.
     *
     * AND THE ALPHABET GATE REFUSES rather than reaches past the
     * winner. `hashing` is a HASHED space: unrelated trigrams share
     * buckets, so a Latin message can collect Cyrillic evidence out of
     * collisions alone. Dropping the other alphabet from the ranking
     * and taking the best survivor is wrong twice over: the survivor
     * did not win, and every margin measured against a deleted rival
     * is inflated. So a cross-alphabet winner yields `None`.
     */
    def of(text: String): Option[String] = scores(text) match
      case (best, s) +: rest =>
        val separated = rest.headOption.forall(o => s - o._2 >= margin)
        val longEnough = text.count(_.isLetter) >= minLetters
        val said =
          if s >= strong && separated then Some(best)
          else if longEnough && s >= floor && separated then Some(best)
          else None
        said.filter(l => alphabet.agrees(l, text))
      case _ => None

  object Detector:
    def of(byLang: Map[String, Vector[String]],
           margin: Float = 0.03f, floor: Float = 0.5f,
           strong: Float = 0.9f, minLetters: Int = 12,
           alphabet: Alphabet = Alphabet.none): Detector =
      Detector(Profiles.rows(byLang), margin, floor, strong, minLetters, alphabet)

  /**
   * The detector as an ARTIFACT: one hashed vector per authored
   * phrase, and no phrase. Building it from the phrases at startup was
   * the last thing in a running service that needed them, and it kept
   * the whole authored corpus in the shipped image for a table of
   * trigram counts. The profiles are the same numbers either way.
   *
   * The encoder written down is `hashing` and not a model name,
   * because that is what made these numbers; a checkpoint that says
   * so is one a later reader can refuse by the same rule as any
   * other, if the hashing ever changes.
   */
  object Profiles:

    val encoder = "hashing"

    def rows(byLang: Map[String, Vector[String]]): Vector[(String, Embedding)] =
      val f = Vectors.hashing()
      byLang.toVector.sortBy(_._1).flatMap((code, phrases) => phrases.map(p => code -> f(p)))

    def print(rows: Vector[(String, Embedding)]): String =
      Json.print(JObj(Vector("profiles" -> JArr(
        rows.map((code, v) => JObj(Vector("lang" -> JStr(code),
          "vec" -> JArr(v.map(x => JNum(x.toDouble)).toVector))))))))

    def parse(raw: String): Vector[(String, Embedding)] =
      Json.parse(raw) match
        case JObj(fs) => fs.collectFirst { case ("profiles", JArr(xs)) => xs }
          .getOrElse(Vector.empty).flatMap {
            case JObj(g) =>
              for
                l <- g.collectFirst { case ("lang", JStr(x)) => x }
                v <- g.collectFirst { case ("vec", JArr(ns)) =>
                  embedding(ns.collect { case JNum(d) => d.toFloat }.toArray) }
              yield l -> v
            case _ => Vector.empty
          }
        case _ => Vector.empty

    def checkpoint(rows: Vector[(String, Embedding)], path: Path, f16: Boolean = false): Unit =
      Checkpoint.write(path, encoder, rows.headOption.fold(0)(_._2.length),
        rows.map(_._1), rows.map(_._2.toArray), f16 = f16)

    def ofCheckpoint(l: Checkpoint.Loaded): Vector[(String, Embedding)] =
      l.labels.zip(l.vecs).map((code, v) => code -> scala.collection.immutable.ArraySeq.unsafeWrapArray(v))

    /** both artifacts beside each other, derived once */
    def write(json: Path, rows: Vector[(String, Embedding)], f16: Boolean = true): Unit =
      Files.createDirectories(json.toAbsolutePath.getParent)
      Files.writeString(json, print(rows))
      checkpoint(rows, Checkpoint.binaryOf(json), f16)

    /** the binary first, the JSON beside it; `None` when neither is
     * in the image — a caller with the phrases in hand builds the
     * profiles itself, which is what every test does */
    def resource(json: String, warn: String => Unit = _ => ()): Option[Vector[(String, Embedding)]] =
      Checkpoint.resource(Checkpoint.binaryOf(json), Some((encoder, 64))) match
        case Right(l) => Some(ofCheckpoint(l))
        case Left(why) =>
          if !Checkpoint.absent(why) then warn(why)
          val path = json.stripPrefix("/")
          Option(Thread.currentThread.getContextClassLoader.getResourceAsStream(path))
            .orElse(Option(getClass.getResourceAsStream("/" + path))).map { in =>
              val raw = try new String(in.readAllBytes(), "UTF-8") finally in.close()
              parse(raw)
            }
