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
final case class Exemplars(encoder: String, dim: Int, rows: Vector[Exemplar],
                           provenance: Exemplars.Provenance = Exemplars.Provenance.unknown):
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

  /**
   * THE TABLE BY WHAT IT WAS BUILT FROM (specs/dlm-learning.md §11):
   * the encoder's name, its fingerprint, and the digest of the rows it
   * embedded. The same on every processor, where `hash` — over the
   * numbers — moves with the last bits; so this is what an audit and a
   * rebuild compare, and `hash` stays what a pin serves. "" for a table
   * from before this stage, which knows no corpus.
   */
  lazy val origin: String =
    if provenance.corpus.isEmpty then ""
    else Exemplars.digest(Vector(encoder, provenance.fingerprint, provenance.corpus))

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
   * WHAT A TABLE WAS BUILT FROM: the digest of exactly the `(label,
   * phrase)` pairs it embedded, in order — not of the files they came
   * from — and the fingerprint of the encoder that embedded them. The
   * phrases never travel; their digest does.
   */
  final case class Provenance(corpus: String, fingerprint: String)
  object Provenance:
    val unknown: Provenance = Provenance("", "")

  private[dlm] def digest(parts: Seq[String]): String =
    val md = java.security.MessageDigest.getInstance("SHA-256")
    parts.foreach { p =>
      val b = p.getBytes("UTF-8")
      // each part length-prefixed, so no two different lists of parts
      // can be spelled into the same bytes
      md.update(java.nio.ByteBuffer.allocate(4).putInt(b.length).array()); md.update(b)
    }
    md.digest().take(16).map(x => f"${x & 0xff}%02x").mkString

  /** the digest of the rows a build embeds — what `Provenance.corpus` holds */
  def corpusOf(rows: Seq[(String, String)]): String =
    digest(rows.flatMap((l, p) => Vector(l, p)))

  /**
   * `(label, phrase)` and nothing else — which is all any of these
   * artifacts ever were. An authored corpus, a harvested row from the
   * journal and a lesson a person taught all reduce to this, and
   * having one function say so is what lets them join without a
   * second code path.
   */
  def compile(rows: Seq[(String, String)], embed: String => Embedding, encoder: String,
              fingerprint: String = ""): Exemplars =
    val entries = rows.toVector.map((label, p) => Exemplar(label, p, embed(p)))
    Exemplars(encoder, entries.headOption.map(_.vec.length).getOrElse(0), entries,
      Provenance(corpusOf(rows), fingerprint))

  /** the same, through the encoder in scope, under its own name and fingerprint */
  def compile(rows: Seq[(String, String)])(using e: Embedder): Exemplars =
    compile(rows, e(_), e.name, e.fingerprint)

  /**
   * THE SAME TABLE BY MEANING (specs/dlm-learning.md §11): one encoder,
   * the same labels in the same order, and every row at cosine ≥ `min`
   * to its counterpart. What two builds of one corpus on two processors
   * are, and what a build gate asks of the table it is about to ship
   * against the one committed. Right is the lowest cosine; Left names
   * the first disagreement.
   */
  def agrees(a: Exemplars, b: Exemplars, min: Double = 0.9999): Either[String, Double] =
    if a.encoder != b.encoder then Left(s"encoders differ: «${a.encoder}» and «${b.encoder}»")
    else if a.rows.length != b.rows.length then Left(s"${a.rows.length} rows against ${b.rows.length}")
    else
      a.rows.zip(b.rows).zipWithIndex.collectFirst {
        case ((x, y), i) if x.label != y.label => s"row $i: «${x.label}» against «${y.label}»"
      } match
        case Some(why) => Left(why)
        case None =>
          val cos = a.rows.zip(b.rows).map((x, y) => cosine(x.vec, y.vec))
          cos.zipWithIndex.find(_._1 < min) match
            case Some((c, i)) => Left(f"row $i («${a.rows(i).label}») at cosine $c%.6f, under $min%.6f")
            case None => Right(if cos.isEmpty then 1.0 else cos.min)

  private def cosine(a: Embedding, b: Embedding): Double =
    var d = 0.0; var na = 0.0; var nb = 0.0; var i = 0
    while i < a.length do
      d += a(i).toDouble * b(i); na += a(i).toDouble * a(i); nb += b(i).toDouble * b(i); i += 1
    if na == 0.0 || nb == 0.0 then (if na == nb then 1.0 else 0.0) else d / math.sqrt(na * nb)

  /**
   * A TABLE THIS ENCODER MAY SERVE: refused by name, as ever, and by
   * fingerprint when the table and the encoder both carry one — the
   * case a name misses when one model's int8 file replaces its fp32
   * file under the same name. A table from before this stage carries
   * none and is taken on its name.
   */
  def accept(table: Exemplars, by: Embedder): Either[String, Exemplars] =
    if table.encoder != by.name then
      Left(s"compiled by «${table.encoder}», this process runs «${by.name}» — refused")
    else if table.provenance.fingerprint.nonEmpty && by.fingerprint.nonEmpty &&
            table.provenance.fingerprint != by.fingerprint then
      Left(s"compiled by «${table.encoder}» at ${table.provenance.fingerprint.take(12)}, " +
        s"this process runs it at ${by.fingerprint.take(12)} — the same name, other numbers; refused")
    else Right(table)

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
      "dim" -> JNum(e.dim.toDouble)) ++
      Option.when(e.provenance.corpus.nonEmpty)("corpus" -> JStr(e.provenance.corpus)) ++
      Option.when(e.provenance.fingerprint.nonEmpty)("fingerprint" -> JStr(e.provenance.fingerprint)) ++ Vector(
      "entries" -> JArr(e.rows.map(x => JObj(Vector(
        "label" -> JStr(x.label),
        "vec" -> JArr(x.vec.map(f => JNum(f.toDouble)).toVector))))))))

  def parse(raw: String): Either[String, Exemplars] =
    Json.parse(raw) match
      case JObj(fs) =>
        val encoder = fs.collectFirst { case ("model", JStr(x)) => x }.getOrElse("")
        val dim = fs.collectFirst { case ("dim", JNum(x)) => x.toInt }.getOrElse(0)
        def str(k: String) = fs.collectFirst { case (`k`, JStr(x)) => x }.getOrElse("")
        val provenance = Provenance(str("corpus"), str("fingerprint"))
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
        else Right(Exemplars(encoder, dim, entries, provenance))
      case _ => Left("not a JSON object")

  // ---- the checkpoint: the one a boot reads -----------------------------

  def checkpoint(e: Exemplars, path: Path, extra: Map[String, String] = Map.empty,
                 f16: Boolean = false): Unit =
    Checkpoint.write(path, e.encoder, e.dim, e.rows.map(_.label), e.rows.map(_.vec.toArray),
      provenanceMeta(e) ++ extra, f16 = f16)

  /** what a checkpoint's `__metadata__` says a table was built from */
  def provenanceMeta(e: Exemplars): Map[String, String] =
    Option.when(e.provenance.corpus.nonEmpty)("corpus" -> e.provenance.corpus).toMap ++
      Option.when(e.provenance.fingerprint.nonEmpty)("fingerprint" -> e.provenance.fingerprint) ++
      Option.when(e.origin.nonEmpty)("origin" -> e.origin)

  def ofCheckpoint(l: Checkpoint.Loaded): Exemplars =
    Exemplars(l.encoder, l.dim,
      // `Embedding` is an `ArraySeq[Float]`, and `unsafeWrapArray` is
      // the point of the container: the floats become the vector
      // without a copy
      l.labels.zip(l.vecs).map((label, v) =>
        Exemplar(label, "", scala.collection.immutable.ArraySeq.unsafeWrapArray(v))),
      Provenance(l.meta.getOrElse("corpus", ""), l.meta.getOrElse("fingerprint", "")))

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
