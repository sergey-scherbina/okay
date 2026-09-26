package okay.cluster.foreign

import okay.given
import okay.cluster.{Flow, Flows}
import okay.foreign.{Foreign, TestPy}
import FacadeConformance.Rec

object MeasureFacadeMod:
  val mod = Foreign.module("facademeasure", """
    def echo(rec):
        return rec

    def fecho(frame):
        return frame
  """)

/**
 * THE MEASUREMENT TABLE (specs/foreign-facade.md, "Measurements"): each
 * tier through the facade, per language, beside the language's OWN road
 * — a cell worse than that road is the facade's overhead and a defect.
 * Live; medians of five; the assertions are sanity only. Numbers go into
 * the spec's Results with the load and the sha.
 */
class MeasureFacade extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private val python = TestPy.python.getOrElse("python3")
  given Calls[okay.foreign.PyModule] = Calls.py(python)
  given Frames[okay.foreign.PyModule] = Frames.py(python)

  private def ms(body: => Any): Double =
    val t0 = System.nanoTime(); body: Unit; (System.nanoTime() - t0) / 1e6
  private def median(n: Int)(body: => Any): Double =
    val xs = Vector.fill(n)(ms(body)).sorted
    xs(n / 2)
  private def rows(n: Int) = Vector.tabulate(n)(i => Rec(i, i * 0.5, s"r$i"))

  test("tier 1, 2 and 3 through the facade, python3 and the JVM, beside their own roads") {
    val py = MeasureFacadeMod.mod
    val jvm = JvmModule("facademeasure").fn[Rec, Rec]("echo")(identity).frame("fecho")(identity)
    val one = Rec(1, 1.5, "x")
    val n = 100000
    val rs = rows(n)
    val table = okay.arrow.Rows.table(rs)
    // warm the pools
    Road.value[okay.foreign.PyModule, Rec, Rec](py, "echo")(one): Unit
    Road.rows[okay.foreign.PyModule, Rec, Rec](py, "fecho")(rs.take(10)): Unit
    val pool = PyPool.of(py, python, Stage.Workers)
    val ownFrame = okay.foreign.ArrowFrames.frame(table)

    val callFacade = median(5)(Road.value[okay.foreign.PyModule, Rec, Rec](py, "echo")(one))
    val callOwn = median(5)(PyPool.call(pool, python, "facademeasure:echo", Vector(okay.foreign.PyCodec.encode(one))))
    val frameFacade = median(5)(Road.rows[okay.foreign.PyModule, Rec, Rec](py, "fecho")(rs))
    // the own road for a caller with ROWS: rows to a PyFrame, over, its rows back (PyStage's)
    val frameOwn = median(5)(okay.foreign.PyFrame.of(rs).flatMap(f => PyPool.frame(pool, python, "facademeasure:fecho", f, Vector.empty)).flatMap(_.rows[Rec]))
    val frameBare = median(5)(PyPool.frame(pool, python, "facademeasure:fecho", ownFrame, Vector.empty))
    val tableFacade = median(5)(summon[Frames[okay.foreign.PyModule]].frame(py, "fecho")(table))
    val streamFacade = median(3)(Flows.collect(Road.flow[okay.foreign.PyModule, Rec, Rec](py, "fecho", 4096)(Flow.slices(rs, 1))).runWith)
    val jvmCall = median(5)(Road.value[JvmModule, Rec, Rec](jvm, "echo")(one))
    val jvmFrame = median(5)(summon[Frames[JvmModule]].frame(jvm, "fecho")(table))
    val jvmRows = median(5)(Road.rows[JvmModule, Rec, Rec](jvm, "fecho")(rs))
    val load = java.lang.management.ManagementFactory.getOperatingSystemMXBean.getSystemLoadAverage
    println(f"facade measure: load $load%.1f, python $python, frames ${summon[Speaks[okay.foreign.PyModule]].speaks(py).frames}")
    println(f"  python tier 1 (one value):        facade $callFacade%8.3f ms   own road $callOwn%8.3f ms")
    println(f"  python tier 2 ($n rows, one frame): rows facade $frameFacade%8.1f ms   rows own road $frameOwn%8.1f ms   frame alone $frameBare%8.1f ms   a Table through Frames.frame $tableFacade%8.1f ms")
    println(f"  python tier 3 ($n rows, 4096/frame): facade $streamFacade%8.1f ms")
    println(f"  jvm    tier 1 (one value):        facade $jvmCall%8.3f ms")
    println(f"  jvm    tier 2 (Table by reference): facade $jvmFrame%8.3f ms;  rows in and out (Rows.table + Rows.rows): $jvmRows%8.1f ms")
    assert(callFacade < 1000 && frameFacade < 60000, "sanity")
  }
