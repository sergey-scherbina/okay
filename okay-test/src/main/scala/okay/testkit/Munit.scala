package okay.testkit

import _root_.munit.{FailExceptionLike, FunSuite}
import okay.diagnose.{Diagnostics, FailureFormat}
import scala.util.Failure

/**
 * THE MUNIT ADAPTER, over an OPTIONAL dependency (the operator's rule,
 * 2026-09-27; the shape is okay-compress's `Aircompressor`): okay-test's
 * core knows no framework, and this object is where munit is named. A
 * module that tests with munit already has it on its test classpath. One
 * that does not never loads this object, and `missing` names what to add.
 *
 *     class TestX extends munit.FunSuite with Munit.Diagnosed:
 *       test("...") { note("..."); onFailure(state) ... }
 */
object Munit:
  /** why munit cannot be used here, or None */
  def missing: Option[String] = MunitPlatform.missing

  /** munit's own failures keep their class and their diff: the diagnosis
   * is appended to the message the runner prints */
  given failures: FailureFormat with
    def extend(e: Throwable, diagnosis: String): Throwable =
      if diagnosis.isEmpty then e
      else e match
        case f: FailExceptionLike[?] => f.withMessage(f.getMessage + "\n" + diagnosis)
        case other => FailureFormat.suppressed.extend(other, diagnosis)

  /** a FunSuite whose failures carry the test's `Diagnostics` */
  trait Diagnosed extends FunSuite:
    @volatile private var current = Diagnostics()

    override def beforeEach(context: BeforeEach): Unit =
      super.beforeEach(context)
      current = Diagnostics()

    def note(msg: => String): Unit = current.note(msg)
    def onFailure(snapshot: => String): Unit = current.onFailure(snapshot)
    def recorded: String = current.recorded
    private[testkit] def diagnostics: Diagnostics = current

    override def munitTestTransforms: List[TestTransform] =
      super.munitTestTransforms :+ new TestTransform("okay.testkit.Munit.Diagnosed", t =>
        t.withBody(() =>
          t.body().transform {
            case Failure(e) => Failure(failures.extend(e, current.report))
            case ok => ok
          }(using munitExecutionContext)))

  /** one spelling of the Live tag (AGENTS.md: a suite reaching outside the
   * JVM is `Live`-tagged and out of `sbt test`) */
  trait LiveTests extends FunSuite:
    def liveTest(name: String)(body: => Any)(using loc: _root_.munit.Location): Unit =
      test(name.tag(Live))(body)

  val Live: _root_.munit.Tag = new _root_.munit.Tag("Live")
