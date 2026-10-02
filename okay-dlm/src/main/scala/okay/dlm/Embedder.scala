package okay.dlm

import okay.rag.{Embedding, Vectors}

/**
 * THE ENCODER, AS A SEAM (specs/dlm.md, "Backends").
 *
 * Every vector in the model is the output of one function over a
 * sentence, and everything else is arithmetic over those outputs. So
 * the function is the one thing a deployment chooses: a sentence
 * encoder in process, a distilled static table, a remote embeddings
 * API — or, with nothing on disk and no network, character trigrams.
 *
 * The NAME travels with every artifact (`Exemplars.encoder`, the
 * checkpoint's `__metadata__`): a table compiled by one encoder is
 * refused by another, because numbers out of two different functions
 * are not comparable. That is what makes this seam safe to plug: the
 * wrong plug is refused by name at boot, never scored as noise.
 *
 * The default is OURS and needs nothing: `Embedder.hashing`. A caller
 * with a model on disk, or a key for a remote one, brings its own
 * `given Embedder` and every `(using Embedder)` door takes it.
 */
trait Embedder:
  /** what made these numbers — the identity every artifact is stamped with */
  def name: String
  def dim: Int
  def apply(text: String): Embedding
  /** WHICH NUMBERS the name stands for (specs/dlm-learning.md §11): a
   * hash of a model's tensors, or of an algorithm and its parameters.
   * The name says what a caller MEANT to run; this says what it ran —
   * an int8 file and an fp32 file of one model share a name and not a
   * number. "" when the caller cannot know, as for a remote API. */
  def fingerprint: String = ""

object Embedder:

  /** the encoder as a plain function, for the tiers that take one */
  given toFunction: Conversion[Embedder, String => Embedding] = e => e(_)

  /**
   * OURS BY DEFAULT: hashed character trigrams (`okay.rag.Vectors.hashing`).
   * Useless for meaning and exactly right for a question about
   * character sequences — which is why the language detector runs on
   * it and the router does not. It exists here so that a model with
   * no encoder configured still boots, still refuses foreign
   * artifacts by name, and still routes on its rules.
   */
  given ours: Embedder = hashing()

  /** ours has a fingerprint of its own: the algorithm, its version and
   * its width — nothing on disk to hash, and nothing that differs by CPU */
  def hashing(dim: Int = 256): Embedder =
    of(s"hashing-$dim", dim, Vectors.hashing(dim), fingerprint = s"okay.rag.Vectors.hashing/1/$dim")

  /** any encoder a caller already has — an ONNX session's `embed`, a
   * client's call — under the name its artifacts will carry, and the
   * fingerprint of what it runs when the caller knows one */
  def of(name: String, dim: Int, f: String => Embedding, fingerprint: String = ""): Embedder =
    val n = name; val d = dim; val fp = fingerprint
    new Embedder:
      val name = n
      val dim = d
      override val fingerprint = fp
      def apply(text: String): Embedding = f(text)

  /**
   * The distilled tier: a static table of unit vectors (`okay.intent.Static`),
   * no inference at request time. A message made only of units the
   * table has never seen encodes to the ZERO vector on purpose: every
   * cosine is then zero, no head has a margin, and the router asks
   * instead of guessing — the honest answer for text the table cannot
   * see. The name is the teacher's, prefixed, so a table distilled
   * from one encoder never answers for another.
   */
  def static(teacher: String, table: okay.intent.Static.Table): Embedder = new Embedder:
    val name = s"static:$teacher"
    val dim = table.dim
    def apply(text: String): Embedding =
      okay.intent.Static.encode(table, text).getOrElse(okay.rag.embedding(new Array[Float](table.dim)))
