package okay.codec

/**
 * One door for a codec over a schema VALUE (specs/codecs.md,
 * staging-seam): `Codecs.json(s)` / `Codecs.cbor(s)` answer a
 * `JsonCodec` / `CborCodec` for any `Schema`, and WHO builds it is a
 * `Provider` installed at run time — the interpreter by default, on
 * every platform, with nothing to configure; the run-time staged
 * generator when a program that carries `okay-staging` installs it
 * (`RuntimeStaged.install()`, or `Staging.autoInstall()` on the JVM
 * by reflection, so a module that cannot depend on the compiler still
 * benefits from it when it is there).
 *
 * A generic door — `def put[A](a: A)(using Schema[A])`, a persisted
 * topic's `Typed`, a session's state — has no Mirror for the
 * compile-time `Staged.json[A]` macro; from the door's point of view
 * every schema is a run-time value. Those doors go through here, so
 * one `install` reaches all of them, and a program that installs
 * nothing pays only a thin wrapper around the fold it already paid.
 *
 * The seam is exactly one `@volatile` reference. A provider caches
 * by whatever key it likes (the staged one, by the schema's identity);
 * the interpreter allocates a small wrapper per call, which a caller
 * on a hot path avoids by holding the codec once per schema.
 */
object Codecs {

  /**
   * How deep a message this stack carries, on either wire
   * (input-depth-both-wires). Stated once, here, because it is a
   * property of a MESSAGE and not of one codec: both wires read
   * nesting by recursive descent, and nesting is the SENDER's number,
   * not the schema's.
   *
   * That is the whole reason a limit exists. Every other dimension a
   * decoder walks is the program's own — the fields of a product, the
   * cases of a sum, the shape of a schema — and a program cannot hand
   * itself a hundred thousand of them by accident. Depth is the one
   * the sender picks, so without a bound `"[" * 20000` is a 20 KB
   * document that costs a `StackOverflowError` in a module whose
   * doors promise an `Either` or a value. MEASURED 2026-09-10, before
   * this existed: the fast JSON value parser died at 20 000 nested
   * arrays and the lossless projection between 1 000 and 5 000 on a
   * default stack; the CBOR decoder at 5 000 and the strict JSON door
   * at 20 000 levels of a RECURSIVE schema with sbt's `-Xss8m`.
   *
   * 256 is deliberately well under every death measured and well over
   * any message anyone writes: serde_json's limit is 128, Jackson's
   * 1000, CPython's about 1000. The number is one number so that the
   * two wires cannot drift into answering differently — which is the
   * defect the whole unknown-fields arc was about.
   */
  val maxDepth: Int = 256

  trait Provider:
    def name: String
    def json[A](s: Schema[A]): JsonCodec[A]
    def cbor[A](s: Schema[A]): CborCodec[A]
    /** the strict read, text straight into the schema with no tree
     * (staged-strict). Defaulted to the interpreter's own
     * `JsonStrict.read`, so a provider that has nothing better —
     * and every provider written before this door existed — stays
     * correct without saying anything. */
    def strict[A](s: Schema[A]): StrictJsonCodec[A] = new StrictJsonCodec[A]:
      def decode(text: String): Either[String, A] = JsonStrict.read[A](text)(using s)

  /** the fold, as codecs — what every door answers until something
   * else is installed, on every platform */
  object Interpreter extends Provider:
    def name = "interpreter"
    def json[A](s: Schema[A]): JsonCodec[A] = new JsonCodec[A]:
      def encode(a: A): String = Json.encode(s)(a)
      def decode(j: Json): Either[String, A] = Json.decode(s)(j)
    def cbor[A](s: Schema[A]): CborCodec[A] = new CborCodec[A]:
      def encode(a: A): Array[Byte] = Cbor.write(a)(using s)
      def decode(bytes: Array[Byte]): Either[String, A] = Cbor.read[A](bytes)(using s)
    // `strict` is the trait's default: JsonStrict.read, the fold

  @volatile private var current: Provider = Interpreter

  /** the provider every door answers through, by name */
  def provider: Provider = current

  /** installs a provider for every door from now on; codecs already
   * handed out are unchanged (they are values) */
  def install(p: Provider): Unit = current = p

  /** back to the interpreter */
  def reset(): Unit = current = Interpreter

  def json[A](s: Schema[A]): JsonCodec[A] = current.json(s)
  def cbor[A](s: Schema[A]): CborCodec[A] = current.cbor(s)
  def strict[A](s: Schema[A]): StrictJsonCodec[A] = current.strict(s)

  /** `Json.write` through the door */
  def writeJson[A](a: A)(using s: Schema[A]): String = current.json(s).encode(a)
  /** `Json.read` through the door: the lossless parse, then the
   * installed decoder — the same answer as `Json.read` */
  def readJson[A](text: String)(using s: Schema[A]): Either[String, A] =
    current.json(s).decode(Json.parse(text))
  /** `Cbor.write` through the door */
  def writeCbor[A](a: A)(using s: Schema[A]): Array[Byte] = current.cbor(s).encode(a)
  /** `Cbor.read` through the door */
  def readCbor[A](bytes: Array[Byte])(using s: Schema[A]): Either[String, A] = current.cbor(s).decode(bytes)
  /** `Json.readStrict` through the door: a complete document from a
   * source trusted to be well formed, no tree built */
  def readStrict[A](text: String)(using s: Schema[A]): Either[String, A] = current.strict(s).decode(text)
}
