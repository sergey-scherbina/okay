package okay.dlm

import okay.codec.Json
import okay.codec.Json.*
import okay.rag.{Embedding, embedding}
import java.nio.file.{Files, Path}

/** one authored phrase, with the vector a build-time encoder gave it.
 * `phrase` is present while COMPILING and empty in anything loaded
 * back: it does not travel in the artifact — see `Exemplars.print` */
final case class Exemplar(label: String, phrase: String, vec: Embedding)

/**
 * THE COMPILED TABLE: a label and a vector per authored phrase, and
 * the encoder that made them. This is what a deterministic model's
 * "weights" are — the outputs of a frozen encoder over text a person
 * wrote, never the parameters of a function (specs/dlm.md, "What the
 * model is").
 *
 * `encoder` and `dim` are stamped in so a mismatched pair is caught
 * at load instead of silently scoring noise: vectors compiled by one
 * encoder are meaningless to another.
 *
 * Every head of the model — which intent, which act, yes or no, which
 * frame — is one of these. The label happens to name a different kind
 * of class each time, and the table does not care.
 */
final case class Exemplars(encoder: String, dim: Int, rows: Vector[Exemplar]):
  def isEmpty: Boolean = rows.isEmpty
  def labels: Vector[String] = rows.map(_.label).distinct
  /** what a tier is fitted from */
  def labelled: Vector[(Embedding, String)] = rows.map(e => (e.vec, e.label))
  /** THE TABLE BY HASH: SHA-256 of its checkpoint bytes, which are a
   * pure function of the numbers — so two builds of the same corpus
   * under the same encoder ON THE SAME PLATFORM hash the same, and an
   * audit names the table a decision was made with
   * (specs/dlm-learning.md). Not across CPUs: an int8 encoder's numbers
   * move with the kernels that run it (measured by the first consumer,
   * 2026-09-29: cosine 0.97–0.99, every table a new hash) */
  lazy val hash: String =
    val bytes = Checkpoint.bytes(encoder, dim, rows.map(_.label), rows.map(_.vec.toArray))
    val arr = new Array[Byte](bytes.remaining()); bytes.get(arr)
    java.security.MessageDigest.getInstance("SHA-256").digest(arr).take(16).map(b => f"${b & 0xff}%02x").mkString

  /** THE TABLE AS A CHECKPOINT OF IT READS BACK. F16 rounds every
   * number, and what a boot serves is the rounded table — so this, not
   * the full-precision one a build holds, is what the shelf, `Rebuilt`
   * and an explanation name by hash (specs/dlm-learning.md §9).
   * Rounding twice rounds nothing: every half is exact as a float. */
  def stored(f16: Boolean = true): Exemplars =
    if !f16 then this
    else copy(rows = rows.map(r => r.copy(vec = embedding(
      r.vec.toArray.map(x => Checkpoint.halfToFloat(Checkpoint.floatToHalf(x)))))))

object Exemplars:

  val empty: Exemplars = Exemplars("", 0, Vector.empty)

  /**
   * `(label, phrase)` and nothing else — which is all any of these
   * artifacts ever were. An authored corpus, a harvested row from the
   * journal and a lesson a person taught all reduce to this, and
   * having one function say so is what lets them join without a
   * second code path.
   */
  def compile(rows: Seq[(String, String)], embed: String => Embedding, encoder: String): Exemplars =
    val entries = rows.toVector.map((label, p) => Exemplar(label, p, embed(p)))
    Exemplars(encoder, entries.headOption.map(_.vec.length).getOrElse(0), entries)

  /** the same, through the encoder in scope, under its own name */
  def compile(rows: Seq[(String, String)])(using e: Embedder): Exemplars =
    compile(rows, e(_), e.name)

  // ---- the JSON artifact: the one a person can open and diff ----------

  /**
   * THE PHRASE DOES NOT GO IN THE ARTIFACT. It would put the whole
   * authored corpus into a shipped image a second time, in plain
   * text, for a diagnostic — and the corpus is the one asset that
   * cannot be rebuilt by reading the code. An error names the INDEX
   * instead.
   */
  def print(e: Exemplars): String =
    Json.print(JObj(Vector(
      "model" -> JStr(e.encoder),
      "dim" -> JNum(e.dim.toDouble),
      "entries" -> JArr(e.rows.map(x => JObj(Vector(
        "label" -> JStr(x.label),
        "vec" -> JArr(x.vec.map(f => JNum(f.toDouble)).toVector))))))))

  def parse(raw: String): Either[String, Exemplars] =
    Json.parse(raw) match
      case JObj(fs) =>
        val encoder = fs.collectFirst { case ("model", JStr(x)) => x }.getOrElse("")
        val dim = fs.collectFirst { case ("dim", JNum(x)) => x.toInt }.getOrElse(0)
        val entries = fs.collectFirst { case ("entries", JArr(xs)) => xs }
          .getOrElse(Vector.empty).flatMap {
            case JObj(g) =>
              for
                // `intent` is the key the first consumer wrote for
                // years; an artifact written under it is read, not
                // refused — a build output outlives its format
                l <- g.collectFirst { case ("label", JStr(x)) => x }
                  .orElse(g.collectFirst { case ("intent", JStr(x)) => x })
                v <- g.collectFirst { case ("vec", JArr(ns)) =>
                  embedding(ns.collect { case JNum(d) => d.toFloat }.toArray) }
              yield Exemplar(l, "", v)
            case _ => None
          }
        val wrong = entries.zipWithIndex.filter((e, _) => e.vec.length != dim).map(_._2)
        if dim > 0 && wrong.nonEmpty then
          Left(s"dimension $dim contradicted by ${wrong.length} entries (first at index ${wrong.head})")
        else Right(Exemplars(encoder, dim, entries))
      case _ => Left("not a JSON object")

  // ---- the checkpoint: the one a boot reads -----------------------------

  def checkpoint(e: Exemplars, path: Path, extra: Map[String, String] = Map.empty,
                 f16: Boolean = false): Unit =
    Checkpoint.write(path, e.encoder, e.dim, e.rows.map(_.label), e.rows.map(_.vec.toArray), extra, f16 = f16)

  def ofCheckpoint(l: Checkpoint.Loaded): Exemplars =
    Exemplars(l.encoder, l.dim,
      // `Embedding` is an `ArraySeq[Float]`, and `unsafeWrapArray` is
      // the point of the container: the floats become the vector
      // without a copy
      l.labels.zip(l.vecs).map((label, v) =>
        Exemplar(label, "", scala.collection.immutable.ArraySeq.unsafeWrapArray(v))))

  // ---- the two together -------------------------------------------------

  /**
   * Refuse to overwrite an artifact compiled under a different encoder
   * than the one that wrote it, unless told the change is deliberate.
   * The vectors of one encoder are noise to another, the two share a
   * dimension, and nothing downstream would notice — a container was
   * once built on exactly that.
   */
  def guard(json: Path, encoder: String, force: Boolean = false): Either[String, Unit] =
    if !Files.exists(json) || force then Right(())
    else
      "\"model\"\\s*:\\s*\"([^\"]+)\"".r.findFirstMatchIn(Files.readString(json))
        .map(_.group(1)).filter(_ != encoder) match
        case Some(was) => Left(s"$json was compiled by «$was» and this would write «$encoder» — " +
          "the vectors of one encoder are noise to another; if the encoder really changed, say so")
        case None => Right(())

  /** both artifacts beside each other: the JSON a person diffs and
   * the checkpoint a boot reads — the same numbers, derived once */
  def write(json: Path, e: Exemplars, extra: Map[String, String] = Map.empty,
            f16: Boolean = true): Unit =
    Files.createDirectories(json.toAbsolutePath.getParent)
    Files.writeString(json, print(e))
    checkpoint(e, Checkpoint.binaryOf(json), extra, f16)

  /**
   * ONE ARTIFACT, TWO FORMATS, AND THE BINARY FIRST. A checkpoint that
   * is absent, older than this reader, or made by another encoder is
   * not an error here: the JSON beside it is the same numbers, and
   * saying which one answered is `warn`'s job rather than a crash's
   * («refusal is a first-class outcome»).
   */
  def read(json: Path, expect: Option[(String, Int)] = None,
           warn: String => Unit = _ => ()): Either[String, Exemplars] =
    Checkpoint.read(Checkpoint.binaryOf(json), expect) match
      case Right(l) => Right(ofCheckpoint(l))
      case Left(why) =>
        if !Checkpoint.absent(why) then warn(why)
        if !Files.exists(json) then Left(s"$json: no artifact")
        else parse(Files.readString(json))

  /** the same from a jar: `None` when the image carries neither, which
   * is a supported deployment — the model then runs on its rules */
  def resource(json: String, expect: Option[(String, Int)] = None,
               warn: String => Unit = _ => ()): Option[Exemplars] =
    Checkpoint.resource(Checkpoint.binaryOf(json), expect) match
      case Right(l) => Some(ofCheckpoint(l))
      case Left(why) =>
        if !Checkpoint.absent(why) then warn(why)
        val path = json.stripPrefix("/")
        Option(Thread.currentThread.getContextClassLoader.getResourceAsStream(path))
          .orElse(Option(getClass.getResourceAsStream("/" + path))).map { in =>
            val raw = try new String(in.readAllBytes(), "UTF-8") finally in.close()
            parse(raw).fold(e => throw IllegalStateException(s"$json: $e"), identity)
          }
