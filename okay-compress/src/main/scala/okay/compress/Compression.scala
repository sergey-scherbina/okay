package okay.compress

/**
 * WHICH IMPLEMENTATION (compress-crypto-facades): the same shape as
 * okay-arrow's `ArrowCodec`. Ours is the default given — LZ4 and ZSTD in
 * pure Scala on the JVM, Scala.js and Native, no dependency. A library's
 * stands behind an import with an OPTIONAL dependency: on the JVM
 * `okay.compress.Aircompressor.given` (io.airlift:aircompressor, the pure
 * Java implementation okay-compress is measured against), refused by name
 * when the jar is not on the classpath.
 *
 * {{{
 * import okay.compress.Aircompressor.given   // a library's, on the JVM
 * summon[Compression].zstd.compress(bytes)
 * }}}
 *
 * The formats are the same either way — an LZ4 frame, a ZSTD frame — so
 * each reads the other's output, and the choice is one of speed and
 * dependency, never of compatibility.
 */
trait Compression:
  def name: String
  def lz4: Codec
  def zstd: Codec
  /** the RAW Snappy format — what a Parquet page holds (stage 6) */
  def snappy: Codec

object Compression:
  /** THE DEFAULT: ours, on every platform */
  given okay: Compression = Okay

  object Okay extends Compression:
    def name = "okay"
    def lz4: Codec = Lz4Frame
    def zstd: Codec = Zstd
    def snappy: Codec = Snappy
