package okay.py.bench

import okay.codec.{Json, WireCompression, WireFormat}
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Param, Scope, Setup, State, Warmup}

/**
 * THE WIRE'S DEFAULT, PRICED (wire-compression-measured). DEFLATE became the
 * default without a number. A message goes out as encode then compress, and
 * comes in as decompress then decode; this is that round trip, per format,
 * per compression, per size of message:
 *   - small:  a `continue` step, which most of a program-as-data's wire is;
 *   - medium: a call with a kilobyte of arguments;
 *   - large:  a frame of 2000 rows.
 * The bytes on the wire are printed once per trial beside the time, since
 * the time alone cannot say what the compression bought.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(java.util.concurrent.TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class WireCodecBench:

  @Param(Array("small", "medium", "large"))
  var size: String = "small"

  @Param(Array("json", "cbor"))
  var format: String = "json"

  @Param(Array("none", "deflate", "deflate-fresh"))
  var compress: String = "none"

  private var msg: Json = Json.JNull
  private var fmt: WireFormat = WireFormat.json
  private var cmp: WireCompression = WireCompression.Off.off

  @Setup def setup(): Unit =
    msg = size match
      case "small" => Json.JObj(Vector("id" -> Json.JNum(41), "op" -> Json.JStr("continue"), "run" -> Json.JNum(7),
        "k" -> Json.JNum(3), "answer" -> Json.JNum(10)))
      case "medium" => Json.JObj(Vector("id" -> Json.JNum(42), "op" -> Json.JStr("call"), "fn" -> Json.JStr("shop:total"),
        "args" -> Json.JArr(Vector.tabulate(40)(i => Json.JObj(Vector("sku" -> Json.JStr(s"item-$i"), "qty" -> Json.JNum(i), "price" -> Json.JNum(i * 1.25)))))))
      case _ => Json.JObj(Vector("id" -> Json.JNum(43), "ok" -> Json.JArr(Vector.tabulate(2000)(i =>
        Json.JObj(Vector("row" -> Json.JNum(i), "name" -> Json.JStr(s"customer number $i"), "balance" -> Json.JNum(i * 3.5), "active" -> Json.JBool(i % 3 == 0)))))))
    fmt = if format == "cbor" then WireFormat.Cbor.cbor else WireFormat.json
    cmp = compress match
      case "deflate" => WireCompression.Deflate.deflate
      case "deflate-fresh" => WireCodecBench.Fresh
      case _ => WireCompression.Off.off
    val wire = cmp.compress(fmt.encode(msg))
    println(s"\nWIRE-BYTES size=$size format=$format compress=$compress bytes=${wire.length}")

  /** one message out and back in: what each side of the wire does to it */
  @Benchmark def roundTrip(): Json = fmt.decode(cmp.decompress(cmp.compress(fmt.encode(msg))))

object WireCodecBench:
  /** the codec as it was before wire-compression-measured, kept as the lane's
   * control: a new Deflater and Inflater, with 8 KB buffers, per message */
  object Fresh extends WireCompression:
    def name = "deflate"
    def compress(bytes: Array[Byte]): Array[Byte] =
      val d = java.util.zip.Deflater(java.util.zip.Deflater.DEFAULT_COMPRESSION, true)
      try
        d.setInput(bytes)
        d.finish()
        val out = java.io.ByteArrayOutputStream()
        val buf = new Array[Byte](8192)
        while !d.finished() do out.write(buf, 0, d.deflate(buf))
        out.toByteArray
      finally d.end()
    def decompress(bytes: Array[Byte]): Array[Byte] =
      val i = java.util.zip.Inflater(true)
      try
        i.setInput(bytes)
        val out = java.io.ByteArrayOutputStream()
        val buf = new Array[Byte](8192)
        while !i.finished() do
          val n = i.inflate(buf)
          if n == 0 && (i.needsInput() || i.needsDictionary()) then
            throw IllegalStateException("a DEFLATE message ended before its data did (cut short?)")
          out.write(buf, 0, n)
        out.toByteArray
      finally i.end()
