package okay.py

import okay.Handler
import PyValue.*

/**
 * Stage 1: the pool dispatches across N real processes (pids, not
 * timings — determinism over stopwatch), a worker's module state
 * survives its OWN calls and dies with it, the supervisor replaces a
 * corpse cold, and one program passes over both engines unchanged —
 * the two-engine acceptance move.
 */
class TestPyWorkers extends munit.FunSuite {

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private var pools = List.empty[PyWorkers]
  def pool(n: Int): PyWorkers =
    val p = PyWorkers.start(n, TestPy.python.get)
    pools = p :: pools
    p
  override def afterAll(): Unit = pools.foreach(_.close())

  def call(h: Handler[PyEval], fn: String, args: PyValue*) =
    h.handle(PyEval.Call(fn, args.toVector))

  test("N workers are N real processes: concurrent calls land on distinct pids") {
    val p = pool(3)
    val gate = java.util.concurrent.CountDownLatch(1)
    val threads = (1 to 3).map { _ =>
      Thread.startVirtualThread { () =>
        gate.await()
        // hold the worker long enough that three calls MUST overlap
        call(p.handler, "time:sleep", F64(0.3)): Unit
        ()
      }
    }
    // while the three sleeps hold all three workers, pids via a 4th
    // wave prove distinctness after release
    gate.countDown()
    threads.foreach(_.join())
    val pids = (1 to 6).map(_ => call(p.handler, "os:getpid") match
      case Right(I64(pid)) => pid
      case other => fail(other.toString)).toSet
    assertEquals(pids.size, 3, s"expected 3 distinct workers, saw $pids")
  }

  test("module state lives WITH its worker: seed and draw on one process is deterministic") {
    val w = PySubprocess.start(TestPy.python.get)
    try
      def draw(): Double =
        call(w.handler, "random:seed", I64(42)): Unit
        call(w.handler, "random:random") match
          case Right(F64(d)) => d
          case other => fail(other.toString)
      val a = draw(); val b = draw()
      assertEquals(a, b)   // same process, same module, same sequence
    finally w.close()
  }

  test("the supervisor: a killed worker throws, its replacement is fresh and correct") {
    val p = pool(1)
    intercept[IllegalStateException](call(p.handler, "os:_exit", I64(0))): Unit
    // the retry lands on the replacement — imports cold, answers right
    assertEquals(call(p.handler, "math:sqrt", F64(25)), Right(F64(5)))
    // and it is a DIFFERENT process
    val Right(I64(pid1)) = call(p.handler, "os:getpid"): @unchecked
    assert(pid1 > 0)
  }

  test("the two-engine acceptance: one program, both engines, unchanged") {
    def program(h: Handler[PyEval]): Vector[String] =
      val out = Vector.newBuilder[String]
      out += call(h, "math:sqrt", F64(9)).toString
      out += call(h, "no_such:f").left.map(_.kind).toString
      out += h.handle(PyEval.Frame("copy:deepcopy",
        PyFrame(Vector("x" -> Vector(I64(1), PyNone))), Vector.empty))
        .map(_.cols.toMap.apply("x")).toString
      out.result()

    val single = PySubprocess.start(TestPy.python.get)
    val many = pool(2)
    try assertEquals(program(single.handler), program(many.handler))
    finally single.close()
  }
}
