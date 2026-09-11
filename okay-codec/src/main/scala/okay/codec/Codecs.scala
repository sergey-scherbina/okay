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
   * There is deliberately NO depth limit here — `Codecs.maxDepth`
   * (256, then 64) lived for one reason: the interpreted CBOR/JSON
   * decoders cost real native stack per level of a RECURSIVE schema,
   * MEASURED 2026-09-10 at ~4-8 KB/level, so an unbounded message
   * from an adversarial sender was a `StackOverflowError` in a module
   * whose doors promise an `Either` or a value. That reason is gone:
   * `iterative-recursive-decode.md`'s arc gave every recursive door
   * (`Cbor.get`, `Json.decode`, `Cbor.In.skipItem`, `JsonValue`'s fast
   * parser, `Json.lossless`'s projection, `JsonStrict.Reader.get`) a
   * `Cont.defer` trampoline past `NativeThreshold`, so native stack no
   * longer scales with input depth anywhere in this module — measured
   * directly (a scratch depth of 100 000-500 000 decodes correctly on
   * each door before this removal landed) rather than assumed.
   *
   * Removing the number is not "depth is now free": a `Cont.defer`
   * trampoline still allocates one node per level, and the decoded
   * value tree still lives on the heap. An adversarially deep message
   * now costs HEAP instead of native stack — a different failure mode
   * (a whole-JVM `OutOfMemoryError` rather than one thread's
   * `StackOverflowError`), not no failure mode. Reintroducing a limit,
   * if that trade is ever wanted back, is a pure wire-contract policy
   * choice at this point — nothing in the decoders needs it for their
   * own correctness or safety.
   */

  /**
   * How deep the interpreted fold recurses NATIVELY before switching
   * to a `Cont.defer` trampoline (iterative-recursive-decode; the
   * design landed once for `Cbor.get`/`Cbor.In.skipItem`, once for
   * `Json.decode`, both reusing this same number). Centralized here,
   * beside `maxDepth`, rather than a `private val = 24` copied into
   * every file that needs it — cbor-decode-threshold-trampoline and
   * json-decode-threshold-trampoline each carried their own before
   * this, and json-raw-nesting-threshold-trampoline was about to add
   * a third and fourth copy.
   *
   * Well under any depth a real message reaches (this arc's own
   * repo-wide grep found no consumer nesting real data past a
   * handful of levels), so the switch is never on a caller's hot
   * path, and well under `maxDepth` so a document at the wire's own
   * limit still costs no native stack proportional to its depth.
   */
  val NativeThreshold: Int = 24

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
