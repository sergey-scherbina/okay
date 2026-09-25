package okay.py

import okay.{Choose, Reader, effect, runChoice, given}

/**
 * ONE crash suite over every stdio worker (supervised-crash-every-language):
 * the far side's PROCESS is killed (SIGKILL, by the pid this suite started)
 * in the middle of a program, and `ForeignWorker.supervised` must bring
 * every answer back — a new process, and the program replayed from its
 * answers. `TestSupervised` proved it on Python by an `os._exit` from inside
 * a program; this proves it from OUTSIDE, the way a crash or an OOM kill
 * arrives, on each language the far side can be written in.
 */
abstract class CrashConformance extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  /** the worker's command line, started once per (re)open */
  def worker: WorkerCommand
  /** how this far side addresses a served name */
  def address(name: String): String = name
  /** whether the far side serves direct-style functions (`quote`) */
  def direct: Boolean = true
  /** the value rules the far side is read by (Python's; R's for R) */
  def shape: Shape = Shape.python
  given Shape = shape
  /** a link to this far side as a worker: the handshake of its shim family */
  def speaking(link: WireLink, name: String): ForeignWorker = ForeignWorker.over(link, name)

  /** the process the supervisor is talking to now */
  @volatile private var current: Option[Process] = None

  private def open(): ForeignWorker =
    val c = worker
    val pb = ProcessBuilder(c.command*)
    pb.environment().clear()
    c.env.foreach((k, v) => pb.environment().put(k, v))
    pb.redirectError(ProcessBuilder.Redirect.INHERIT)
    val p = pb.start()
    current = Some(p)
    speaking(WireLink.pipes(p), s"the worker ${c.command.head}")

  /** SIGKILL the far side, and wait until it is gone — its whole process
   * TREE, since a worker command may be a wrapper (R through a container
   * shim) whose child holds the pipes open after the wrapper dies */
  private def kill(): Unit =
    current.foreach { p =>
      p.descendants().forEach(d => d.destroyForcibly(): Unit)
      p.destroyForcibly().waitFor(): Unit
    }

  private val choose = Foreign.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))

  test("killed between two choices: every branch of a multi-shot program comes back") {
    val w = ForeignWorker.supervised(open())
    given okay.Handler[ForeignEval] = w.handler
    var killed = false
    val choose = Foreign.callback[Vector[Long], Long]("choose") { xs =>
      if !killed && xs == Vector(10L, 20L) then { killed = true; kill() }
      effect[Choose, Long](Choose(xs))
    }
    try
      val pairs = Foreign.program[Long](address("pairs")).calling(Foreign.callbacks(choose))()
      assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
      assert(killed)
      assertEquals(w.restarts, 1)
    finally w.close()
  }

  test("killed while idle: the next program runs on a fresh worker") {
    val w = ForeignWorker.supervised(open())
    given okay.Handler[ForeignEval] = w.handler
    try
      val pairs = () => Foreign.program[Long](address("pairs")).calling(Foreign.callbacks(choose))()
      assertEquals(runChoice(pairs().program).runWith.size, 4)
      kill()
      assertEquals(runChoice(pairs().program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
      assertEquals(w.restarts, 1)
    finally w.close()
  }

  test("killed mid-ask in DIRECT STYLE: WorkerDied as data, and the next call runs") {
    assume(direct, "this far side serves programs only")
    val w = ForeignWorker.supervised(open())
    given okay.Handler[ForeignEval] = w.handler
    var killed = false
    val priceOf = Foreign.callback[String, Double]("price_of") { sku =>
      if !killed then { killed = true; kill() }
      Reader.ask[Map[String, Double]].map(_(sku))
    }
    val discount = Foreign.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))
    val quote = () => Foreign.fn[Double](address("quote")).calling(Foreign.callbacks(priceOf, discount))("tea", 3L)
    try
      val first = Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(quote()).runWith
      assert(first.left.exists(_.kind == "WorkerDied"), first.toString)
      assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(quote()).runWith, Right(6.0))
      assertEquals(w.restarts, 1)
    finally w.close()
  }

class TestCrashPython extends CrashConformance:
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  def worker: WorkerCommand = ForeignWorker.pythonCommand(TestPy.python.get, modules = Seq(PyConformance.conf))

class TestCrashTypeScript extends CrashConformance:
  override def munitIgnore: Boolean = !TsConformance.node
  override def address(name: String): String = s"conf:$name"
  private lazy val dir =
    val d = java.nio.file.Files.createTempDirectory("okay-ts-crash")
    java.nio.file.Files.writeString(d.resolve("conf.ts"), TsConformance.conf): Unit
    d
  def worker: WorkerCommand = TsWorker.command(dir, modules = Seq("conf"))

class TestCrashGo extends CrashConformance:
  override def munitIgnore: Boolean = !GoWorkerBinary.available
  def worker: WorkerCommand = WorkerCommand(Vector(GoWorkerBinary.binary.toString), Map.empty)

class TestCrashRust extends CrashConformance:
  override def munitIgnore: Boolean = !RustWorkerBinary.available
  def worker: WorkerCommand = WorkerCommand(Vector(RustWorkerBinary.binary.toString), Map.empty)

/** programs only: GHC's worker has no direct style */
class TestCrashHaskell extends CrashConformance:
  override def munitIgnore: Boolean = !HsConformance.ghc
  override def direct: Boolean = false
  def worker: WorkerCommand = WorkerCommand(Vector(HsConformance.binary), Map.empty)
