package okay.compress

/**
 * `Compression` over io.airlift:aircompressor (JVM only), the pure-Java
 * implementation okay-compress is measured against, behind an OPTIONAL
 * dependency: `import okay.compress.Aircompressor.given`. Without the jar
 * the first use is refused by name ([[Aircompressor.missing]]).
 *
 * ZSTD is aircompressor's frame codec as it is. LZ4 is OUR frame envelope
 * (magic, descriptor, checksums, block sizes) over aircompressor's BLOCK
 * codec — aircompressor has no LZ4 frame writer — so what it writes is a
 * frame every LZ4 reads, and a frame whose blocks depend on one another
 * (a flag no writer here sets) is refused by name rather than misread.
 */
object Aircompressor extends Compression:
  given aircompressor: Compression = this

  def name = "aircompressor"

  /** why this cannot run here, or None: a class of aircompressor's missing
   * from the classpath (the dependency is optional) */
  def missing(className: String = "io.airlift.compress.zstd.ZstdCompressor"): Option[String] =
    try { Class.forName(className, false, getClass.getClassLoader); None }
    catch case _: ClassNotFoundException => Some(
      s"okay.compress.Aircompressor needs io.airlift:aircompressor, an optional dependency of okay-compress ($className is not on the classpath): " +
        "add io.airlift:aircompressor:2.0.3 — or use okay.compress.Compression.Okay, the default, which needs nothing")

  private lazy val ready: Unit = missing().foreach(why => throw IllegalStateException(why))

  private def malformed[A](what: String)(f: => A): A =
    try f
    catch case e: io.airlift.compress.MalformedInputException => throw Corrupt(s"aircompressor refused $what: ${e.getMessage}")

  object zstd extends Codec:
    def name = "zstd"
    def compress(bytes: Array[Byte]): Array[Byte] =
      ready
      val c = io.airlift.compress.zstd.ZstdCompressor()
      val out = new Array[Byte](c.maxCompressedLength(bytes.length))
      val n = c.compress(bytes, 0, bytes.length, out, 0, out.length)
      java.util.Arrays.copyOf(out, n)
    def decompress(bytes: Array[Byte]): Array[Byte] =
      ready
      val size = malformed("a ZSTD frame's header")(io.airlift.compress.zstd.ZstdDecompressor.getDecompressedSize(bytes, 0, bytes.length))
      if size < 0 then throw Corrupt("a ZSTD frame without its content size: aircompressor needs one to decompress")
      if size > Int.MaxValue - 16 then throw Corrupt(s"a ZSTD frame of $size bytes")
      val out = new Array[Byte](size.toInt)
      val n = malformed("a ZSTD frame")(io.airlift.compress.zstd.ZstdDecompressor().decompress(bytes, 0, bytes.length, out, 0, out.length))
      if n == out.length then out else java.util.Arrays.copyOf(out, n)

  /** aircompressor's LZ4 block codec under our frame */
  object blocks extends Lz4Blocks:
    def bound(n: Int): Int = io.airlift.compress.lz4.Lz4Compressor().maxCompressedLength(n)
    def compress(src: Array[Byte], from: Int, len: Int, dst: Array[Byte], at: Int): Int =
      ready
      io.airlift.compress.lz4.Lz4Compressor().compress(src, from, len, dst, at, dst.length - at)
    def decompress(src: Array[Byte], from: Int, len: Int, dst: Array[Byte], at: Int, limit: Int, floor: Int): Int =
      ready
      // `floor` is where the frame's output begins; the library's
      // decompressor sees only `dst` from `at`, so a DEPENDENT block (one
      // whose matches reach into an earlier block's output, a flag no
      // writer here sets) fails as malformed and is refused, not misread
      at + malformed("an LZ4 block (a dependent block needs okay's block codec)")(
        io.airlift.compress.lz4.Lz4Decompressor().decompress(src, from, len, dst, at, limit - at))

  val lz4: Codec = Lz4FrameCodec(blocks)

/** the implementations by NAME, for a config value or a flag — as
 * `WireChoice.named` picks the wire (JVM: the library one lives here) */
object Compressions:
  def byName(name: String): Either[String, Compression] = name match
    case "okay" => Right(Compression.Okay)
    case "aircompressor" => Right(Aircompressor)
    case other => Left(s"unknown compression implementation '$other' (okay, aircompressor)")
