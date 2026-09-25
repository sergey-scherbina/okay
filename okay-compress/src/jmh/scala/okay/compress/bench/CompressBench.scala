package okay.compress.bench

import io.airlift.compress.lz4.{Lz4Compressor, Lz4Decompressor}
import io.airlift.compress.zstd.{ZstdCompressor, ZstdDecompressor}
import okay.compress.{Lz4Block, Zstd}
import org.openjdk.jmh.annotations.*

/**
 * STAGE 5 of specs/okay-compress.md: okay-compress against aircompressor
 * 2.0.3 (pure Java), one input of 4 MiB of CSV-like lines, and the other
 * of mixed text. Lanes pair the same work: an LZ4 BLOCK into a buffer made
 * once (aircompressor's LZ4 is the block format), a ZSTD FRAME in and out
 * (ours allocates its output per call; aircompressor writes into a buffer
 * made once — a difference the numbers carry, stated here). Interop first:
 * each decodes the other's output, or no number.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(java.util.concurrent.TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
class CompressBench:

  @Param(Array("lines", "text"))
  var input: String = "lines"

  private var src: Array[Byte] = null
  private var lz4Ours: Array[Byte] = null
  private var lz4OursLen = 0
  private var lz4Air: Array[Byte] = null
  private var lz4AirLen = 0
  private var zstdOurs: Array[Byte] = null
  private var zstdAir: Array[Byte] = null
  private var out: Array[Byte] = null
  private val airLz4c = Lz4Compressor(); private val airLz4d = Lz4Decompressor()
  private val airZc = ZstdCompressor(); private val airZd = ZstdDecompressor()

  @Setup def setup(): Unit =
    val all = if input == "lines" then okay.compress.Samples.big
      else Array.tabulate(40)(_ => okay.compress.Samples.all.flatMap(_._2)).flatten
    src = java.util.Arrays.copyOf(all, math.min(all.length, 4 << 20))
    out = new Array[Byte](src.length)
    lz4Ours = new Array[Byte](Lz4Block.bound(src.length))
    lz4OursLen = Lz4Block.compress(src, 0, src.length, lz4Ours, 0)
    lz4Air = new Array[Byte](airLz4c.maxCompressedLength(src.length))
    lz4AirLen = airLz4c.compress(src, 0, src.length, lz4Air, 0, lz4Air.length)
    zstdOurs = Zstd.compress(src)
    val zb = new Array[Byte](airZc.maxCompressedLength(src.length))
    zstdAir = java.util.Arrays.copyOf(zb, airZc.compress(src, 0, src.length, zb, 0, zb.length))
    // interop, both ways, or no number
    def same(n: Int, b: Array[Byte], what: String): Unit =
      if n != src.length || !java.util.Arrays.equals(java.util.Arrays.copyOf(b, n), src) then
        throw IllegalStateException(s"$what: the two do not read each other's output")
    val a = new Array[Byte](src.length)
    same(airLz4d.decompress(lz4Ours, 0, lz4OursLen, a, 0, a.length), a, "aircompressor reading our LZ4")
    same(Lz4Block.decompress(lz4Air, 0, lz4AirLen, a, 0, a.length), a, "ours reading aircompressor's LZ4")
    same(airZd.decompress(zstdOurs, 0, zstdOurs.length, a, 0, a.length), a, "aircompressor reading our ZSTD")
    val back = Zstd.decompress(zstdAir)
    same(back.length, back, "ours reading aircompressor's ZSTD")
    println(s"\nRATIO input=$input bytes=${src.length} lz4 ours=$lz4OursLen air=$lz4AirLen zstd ours=${zstdOurs.length} air=${zstdAir.length}")

  @Benchmark def lz4_compress_okay(): Int = Lz4Block.compress(src, 0, src.length, lz4Ours, 0)
  @Benchmark def lz4_compress_air(): Int = airLz4c.compress(src, 0, src.length, lz4Air, 0, lz4Air.length)
  @Benchmark def lz4_decompress_okay(): Int = Lz4Block.decompress(lz4Ours, 0, lz4OursLen, out, 0, out.length)
  @Benchmark def lz4_decompress_air(): Int = airLz4d.decompress(lz4Air, 0, lz4AirLen, out, 0, out.length)
  @Benchmark def zstd_compress_okay(): Array[Byte] = Zstd.compress(src)
  @Benchmark def zstd_compress_air(): Int =
    val zb = new Array[Byte](airZc.maxCompressedLength(src.length))
    airZc.compress(src, 0, src.length, zb, 0, zb.length)
  @Benchmark def zstd_decompress_okay(): Array[Byte] = Zstd.decompress(zstdOurs)
  @Benchmark def zstd_decompress_air(): Int = airZd.decompress(zstdAir, 0, zstdAir.length, out, 0, out.length)
