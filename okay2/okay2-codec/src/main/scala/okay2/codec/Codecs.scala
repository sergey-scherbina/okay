package okay2.codec

/** a JSON codec for one type: text out, a value back */
trait JsonCodec[A] {
  def encode(a: A): String
  def decode(j: Json): Either[String, A]
}

/** the CBOR twin of JsonCodec: bytes in, bytes out */
trait CborCodec[A] {
  def encode(a: A): Array[Byte]
  def decode(bytes: Array[Byte]): Either[String, A]
}

/** the strict JSON reader for one type: text straight into the value */
trait StrictJsonCodec[A] {
  def decode(text: String): Either[String, A]
}

/**
 * One door for a codec over a schema VALUE (okay-codec's Codecs.scala):
 * `Codecs.json(s)` / `Codecs.cbor(s)` answer a codec for any `Schema`,
 * and WHO builds it is a `Provider` installed at run time — the
 * interpreter by default, on every platform, with nothing to configure.
 * A generic door (`def put[A](a: A)(implicit s: Schema[A])`) sees every
 * schema as a run-time value; those doors go through here, so one
 * `install` reaches all of them. The seam is one `@volatile` reference.
 */
object Codecs {

  /** how many open containers a recursive walk takes on the native
   * stack before it continues on a `Cont.defer` trampoline: fast for
   * every ordinary document, bounded by the heap for a deep one */
  val NativeThreshold: Int = 24

  trait Provider {
    def name: String
    def json[A](s: Schema[A]): JsonCodec[A]
    def cbor[A](s: Schema[A]): CborCodec[A]
    /** the strict read, text straight into the schema with no tree;
     * the interpreter's own `JsonStrict.read` unless a provider has
     * something better */
    def strict[A](s: Schema[A]): StrictJsonCodec[A] = new StrictJsonCodec[A] {
      def decode(text: String): Either[String, A] = JsonStrict.read[A](text)(s)
    }
  }

  /** the fold, as codecs — what every door answers until something
   * else is installed */
  object Interpreter extends Provider {
    def name = "interpreter"
    def json[A](s: Schema[A]): JsonCodec[A] = new JsonCodec[A] {
      def encode(a: A): String = Json.encode(s)(a)
      def decode(j: Json): Either[String, A] = Json.decode(s)(j)
    }
    def cbor[A](s: Schema[A]): CborCodec[A] = new CborCodec[A] {
      def encode(a: A): Array[Byte] = Cbor.write(a)(s)
      def decode(bytes: Array[Byte]): Either[String, A] = Cbor.read[A](bytes)(s)
    }
  }

  @volatile private var current: Provider = Interpreter

  /** the provider every door answers through */
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
  def writeJson[A](a: A)(implicit s: Schema[A]): String = current.json(s).encode(a)
  /** `Json.read` through the door: the parse, then the installed decoder */
  def readJson[A](text: String)(implicit s: Schema[A]): Either[String, A] =
    current.json(s).decode(Json.parse(text))
  /** `Cbor.write` through the door */
  def writeCbor[A](a: A)(implicit s: Schema[A]): Array[Byte] = current.cbor(s).encode(a)
  /** `Cbor.read` through the door */
  def readCbor[A](bytes: Array[Byte])(implicit s: Schema[A]): Either[String, A] = current.cbor(s).decode(bytes)
  /** `Json.readStrict` through the door */
  def readStrict[A](text: String)(implicit s: Schema[A]): Either[String, A] = current.strict(s).decode(text)
}
