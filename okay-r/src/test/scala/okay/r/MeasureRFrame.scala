package okay.r

import RValue.*

/**
 * What a FRAME costs on the JSON wire, and where the cost sits
 * (r-measure-harden). `r-arrow` is filed on a condition — "once the
 * JSON-frame road hurts" — and okay-r has never had a number, while
 * its Python twin's number once overturned the whole plan: 60% of a
 * 500k-row round trip turned out to be OUR OWN parser taking a
 * lossless road, and fixing that made Arrow ten times less urgent.
 * So this measures the same split for R before anyone builds
 * anything:
 *
 *   - ENCODE: RFrame -> tagged Json -> the string we write
 *   - ROUND TRIP: the whole `identity` call through R
 *   - DECODE: the answer's text -> Json -> RFrame
 *   - TYPED: RFrame -> Vector[case class] (r-finish's layer)
 *
 * Not JMH, for MeasureSqlFold's reason: the numbers are seconds per
 * frame, the variance is the box's and R's startup, and a fork per
 * lane would pay the container again for a shape that is already
 * clear. Medians of five, warmup discarded, printed as a table,
 * Live-tagged. The assertions are sanity only — that the frame
 * survives — never a millisecond threshold a loaded box turns red.
 */
class MeasureRFrame extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(20, "min")

  final case class Obs(id: Int, temp: Double, site: String) derives okay.codec.Schema

  private val sizes = Vector(10000, 100000)
  private val rounds = 5

  private def frameOf(n: Int): RFrame =
    RFrame(Vector(
      "id" -> Vector.tabulate(n)(i => I32(i)),
      "temp" -> Vector.tabulate(n)(i => F64(i * 0.5)),
      "site" -> Vector.tabulate(n)(i => Str(if i % 2 == 0 then "kyiv" else "lviv"))))

  private def ms(body: => Any): Double =
    val t0 = System.nanoTime(); body: Unit; (System.nanoTime() - t0) / 1e6

  private def median(xs: Vector[Double]): Double =
    val s = xs.sorted; s(s.length / 2)

  test("a frame's round trip, split into the four places the time can be") {
    val r = RSubprocess.start(TestR.rscript.get)
    try
      println("%-10s %8s %10s %10s %10s %10s %9s".format(
        "rows", "MB", "encode", "round", "decode", "typed", "ours %"))
      for n <- sizes do
        val f = frameOf(n)
        // warmup: the first call pays R's lazy loading and our JIT
        r.handler.handle(REval.Frame("identity", frameOf(1000), Vector.empty)): Unit
        val encodes = Vector.fill(rounds)(ms(okay.codec.Json.print(Wire.encFrame(f))))
        val text = okay.codec.Json.print(Wire.encFrame(f))
        val decodes = Vector.fill(rounds)(ms(Wire.decFrame(okay.codec.Json.parse(text))))
        val trips = Vector.fill(rounds)(ms(
          r.handler.handle(REval.Frame("identity", f, Vector.empty))))
        val back = r.handler.handle(REval.Frame("identity", f, Vector.empty))
          .fold(c => fail(s"identity: $c"), identity)
        val typed = Vector.fill(rounds)(ms(back.rows[Obs]))
        val (e, d, t, ty) = (median(encodes), median(decodes), median(trips), median(typed))
        // what share of the round trip is OURS (encode + decode), the
        // question r-arrow turns on: Arrow removes our halves, not R's
        println("%-10d %8.2f %10.1f %10.1f %10.1f %10.1f %9.1f".format(
          n, text.length / 1048576.0, e, t, d, ty, (e + d) / t * 100))
        assertEquals(back.cols.map(_._1), Vector("id", "temp", "site"))
        assertEquals(back.cols.head._2.length, n)
        assertEquals(back.rows[Obs].map(_.length), Right(n))
    finally r.close()
  }
