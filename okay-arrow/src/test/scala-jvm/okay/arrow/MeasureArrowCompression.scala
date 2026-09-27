package okay.arrow

import okay.compress.{Aircompressor, Compression}

/**
 * compress-crypto-facades' number where the codec actually runs: a
 * 500k-row Arrow body (float64, int64, utf8) compressed per buffer by ours
 * and by aircompressor, written and read, each reading the other. Not JMH:
 * medians of five, printed, the box load beside; Live-tagged.
 */
class MeasureArrowCompression extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(20, "min")

  private def table(n: Int): Table =
    val ok = Array.fill(n)(true)
    Table(Vector(
      "price" -> Column.Float64(Array.tabulate(n)(i => (i % 977) * 0.25 + 100.0), ok),
      "qty" -> Column.Int64(Array.tabulate(n)(i => (i % 13).toLong * 10), ok),
      "sym" -> Column.Utf8(Array.tabulate(n)(i => "SYM" + (i % 257)), ok)), Vector.empty)

  private def ms(body: => Any): Double =
    val t0 = System.nanoTime(); body: Unit; (System.nanoTime() - t0) / 1e6
  private def median(k: Int)(body: => Any): Double =
    val xs = Vector.fill(k)(ms(body)).sorted
    xs(xs.length / 2)
  private def load: String =
    scala.util.Try(String(ProcessBuilder("sysctl", "-n", "vm.loadavg").start().getInputStream.readAllBytes()).trim).getOrElse("?")

  test("a 500k-row body per buffer: ours and aircompressor, write and read, each reading the other") {
    val t = table(500000)
    val plain = OkayArrow.write(t)
    println(s"load before: $load; uncompressed body ${plain.length / 1e6}%.1f MB".replace("%.1f", ""))
    println("%-22s | %9s | %9s | %9s | %9s".format("codec", "bytes", "write ms", "read ms", "x-read ms"))
    val impls = Vector(Compression.Okay, Aircompressor)
    for name <- Vector("zstd", "lz4"); impl <- impls do
      val codec = if name == "zstd" then impl.zstd else impl.lz4
      val other = if impl eq Compression.Okay then Aircompressor else Compression.Okay
      val bytes = OkayArrow.write(t, Some(codec))
      assertEquals(Tables.same(t, OkayArrow.read(bytes)(using impl)), None, s"${impl.name} $name")
      val w = median(5)(OkayArrow.write(t, Some(codec)))
      val r = median(5)(OkayArrow.read(bytes)(using impl))
      // the other implementation reading this one's body: the interop cost
      val x = median(5)(OkayArrow.read(bytes)(using other))
      println(f"${impl.name + " " + name}%-22s | ${bytes.length}%9d | $w%9.0f | $r%9.0f | $x%9.0f")
    println(s"load after: $load")
  }
