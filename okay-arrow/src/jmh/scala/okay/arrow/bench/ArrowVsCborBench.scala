package okay.arrow.bench

import okay.arrow.{OkayArrow, Rows}
import okay.codec.{Cbor, Schema}
import okay.compress.Zstd
import org.openjdk.jmh.annotations.*

object Shapes:
  final case class Trade(id: Long, symbol: String, price: Double, qty: Int, buy: Boolean) derives Schema
  enum Kind derives Schema:
    case View, Click, Buy
  final case class Event(user: String, kind: Kind, at: Long, tags: List[String], meta: Option[String]) derives Schema
  final case class Line(product: String, n: Int, price: Double) derives Schema
  final case class Order(id: Long, customer: String, lines: Vector[Line]) derives Schema

  private val symbols = Vector("AAPL", "MSFT", "GOOG", "AMZN", "NVDA", "META")
  def trades(n: Int) = Vector.tabulate(n)(i => Trade(1_000_000L + i, symbols(i % 6), 100.0 + (i % 997) * 0.25, i % 500, i % 3 == 0))
  def events(n: Int) = Vector.tabulate(n)(i => Event(s"user${i % 4096}", Kind.fromOrdinal(i % 3), 1_727_000_000_000L + i * 17L,
    List("web", if i % 2 == 0 then "mobile" else "desktop"), Option.when(i % 5 == 0)(s"campaign-${i % 11}")))
  def orders(n: Int) = Vector.tabulate(n)(i => Order(i.toLong, s"customer-${i % 1000}",
    Vector.tabulate(1 + i % 4)(k => Line(s"sku-${(i + k) % 200}", 1 + k, 9.99 + k))))

/**
 * STAGE 7a of specs/okay-arrow.md: where does Arrow beat CBOR? The same
 * typed rows (okay-codec `Schema`) through both, round trip (encode and
 * decode), plain and ZSTD-compressed, for three shapes — flat trades,
 * mixed events (an enum, a list, an option), nested orders (a vector of
 * structs) — at 1 000 and 100 000 rows. The bytes are printed per trial.
 * Both sides build their own Schema-driven codec once (Arrow: Columns;
 * CBOR: its Schema fold), so neither lane pays setup the other does not.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(java.util.concurrent.TimeUnit.MILLISECONDS)
@Warmup(iterations = 4, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class ArrowVsCborBench:
  import Shapes.*

  @Param(Array("trades", "events", "orders"))
  var shape: String = "trades"

  @Param(Array("1000", "100000"))
  var rows: Int = 1000

  /** one shape's four round trips, typed once */
  private abstract class Lanes:
    def arrow(): Int
    def arrowZstd(): Int
    def cbor(): Int
    def cborZstd(): Int
    def sizes(): String

  private final class Of[A](data: Vector[A])(using s: Schema[A]) extends Lanes:
    private def arrowBytes = OkayArrow.write(Rows.table(data))
    private def cborBytes = Cbor.write(data)
    def arrow(): Int = Rows.rows[A](OkayArrow.read(OkayArrow.write(Rows.table(data)))).fold(e => throw IllegalStateException(e), _.length)
    def arrowZstd(): Int =
      Rows.rows[A](OkayArrow.read(OkayArrow.write(Rows.table(data), Some(Zstd)))).fold(e => throw IllegalStateException(e), _.length)
    def cbor(): Int = Cbor.read[Vector[A]](Cbor.write(data)).fold(e => throw IllegalStateException(e), _.length)
    def cborZstd(): Int = Cbor.read[Vector[A]](Zstd.decompress(Zstd.compress(Cbor.write(data)))).fold(e => throw IllegalStateException(e), _.length)
    def sizes(): String =
      val a = arrowBytes; val c = cborBytes
      s"arrow=${a.length} arrow+zstd=${OkayArrow.write(Rows.table(data), Some(Zstd)).length} cbor=${c.length} cbor+zstd=${Zstd.compress(c).length}"
    // both must give back what went in, or no number
    require(Rows.rows[A](OkayArrow.read(arrowBytes)) == Right(data), "Arrow does not round-trip")
    require(Cbor.read[Vector[A]](cborBytes) == Right(data), "CBOR does not round-trip")

  private var lanes: Lanes = null

  @Setup def setup(): Unit =
    lanes = shape match
      case "trades" => Of(trades(rows))
      case "events" => Of(events(rows))
      case _ => Of(orders(rows))
    println(s"\nSIZES shape=$shape rows=$rows ${lanes.sizes()}")

  @Benchmark def arrow_roundtrip(): Int = lanes.arrow()
  @Benchmark def arrow_zstd_roundtrip(): Int = lanes.arrowZstd()
  @Benchmark def cbor_roundtrip(): Int = lanes.cbor()
  @Benchmark def cbor_zstd_roundtrip(): Int = lanes.cborZstd()
