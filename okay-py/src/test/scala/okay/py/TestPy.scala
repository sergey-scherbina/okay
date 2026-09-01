package okay.py

import okay.{!, effect, pure}
import okay.given
import PyValue.*

/**
 * Stage 0 against a LIVE python3 (skipped where absent): addressing,
 * conditions that leave the worker alive, None-vs-NaN honesty, the
 * clean environment, frames, the handshake refusal, verify's
 * wrong-venv story, and the dead-worker throw.
 */
object TestPy:
  lazy val python: Option[String] =
    sys.env.getOrElse("PATH", "").split(":").iterator
      .map(d => java.nio.file.Paths.get(d, "python3"))
      .find(java.nio.file.Files.isExecutable(_)).map(_.toString)

class TestPy extends munit.FunSuite {

  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private var workers = List.empty[PySubprocess]
  def worker(env: Map[String, String] = Map.empty): PySubprocess =
    val w = PySubprocess.start(TestPy.python.get, env)
    workers = w :: workers
    w
  override def afterAll(): Unit = workers.foreach(_.close())

  def call(w: PySubprocess, fn: String, args: PyValue*) =
    w.handler.handle(PyEval.Call(fn, args.toVector))

  test("module:name addressing — plain, dotted, and the program is data, not code") {
    val w = worker()
    assertEquals(call(w, "math:sqrt", F64(9)), Right(F64(3)))
    assertEquals(call(w, "os.path:join", Str("a"), Str("b")), Right(Str("a/b")))
    // dotted attribute path after the colon
    assertEquals(call(w, "math:pi"), // not callable -> a condition, named
      Left(Condition("TypeError", "'float' object is not callable")))
  }

  test("a missing module and a missing attribute are conditions; the worker SURVIVES") {
    val w = worker()
    call(w, "no_such_module_ever:f") match
      case Left(c) => assertEquals(c.kind, "ModuleNotFoundError")
      case other => fail(other.toString)
    call(w, "math:no_such_fn") match
      case Left(c) => assertEquals(c.kind, "AttributeError")
      case other => fail(other.toString)
    assertEquals(call(w, "math:sqrt", F64(16)), Right(F64(4)))   // alive
  }

  test("None and NaN round-trip DISTINCTLY") {
    val w = worker()
    assertEquals(call(w, "copy:deepcopy", PyNone), Right(PyNone))
    call(w, "copy:deepcopy", F64(Double.NaN)) match
      case Right(F64(d)) => assert(d.isNaN)
      case other => fail(other.toString)
    // and bytes, which JSON also lacks
    call(w, "copy:deepcopy", Bytes(Array[Byte](1, 2, 250.toByte))) match
      case Right(Bytes(bs)) => assertEquals(bs.toList, List[Byte](1, 2, 250.toByte))
      case other => fail(other.toString)
  }

  test("the environment is CLEAN: HOME is invisible; a named var is visible") {
    val w = worker()
    assertEquals(call(w, "os:getenv", Str("HOME")), Right(PyNone))
    val w2 = worker(env = Map("OKAY_TOKEN" -> "t-42"))
    assertEquals(call(w2, "os:getenv", Str("OKAY_TOKEN")), Right(Str("t-42")))
    assertEquals(call(w2, "os:getenv", Str("HOME")), Right(PyNone))
  }

  test("a frame goes over as dict-of-lists and comes back columnar, Nones intact") {
    val w = worker()
    val in = PyFrame(Vector(
      "x" -> Vector(I64(1), I64(2), I64(3)),
      "label" -> Vector(Str("a"), PyNone, Str("c"))))
    w.handler.handle(PyEval.Frame("copy:deepcopy", in, Vector.empty)) match
      case Right(out) =>
        assertEquals(out.cols.map(_._1).toSet, Set("x", "label"))
        assertEquals(out.cols.toMap.apply("label"), Vector(Str("a"), PyNone, Str("c")))
      case other => fail(other.toString)
    // a function answering a non-frame is a condition, named
    w.handler.handle(PyEval.Frame("builtins:len", in, Vector.empty)) match
      case Left(c) => assertEquals(c.kind, "TypeError")
      case other => fail(other.toString)
  }

  test("the shim/host handshake refuses drift loudly") {
    val fake = java.nio.file.Files.createTempFile("okay-py-fake", ".py")
    java.nio.file.Files.write(fake,
      """import json,sys;print(json.dumps({"shim":99,"python":"3.0.0"}));sys.stdout.flush()""".getBytes)
    val e = intercept[IllegalStateException](
      PySubprocess.startWith(TestPy.python.get, fake, Map.empty))
    assert(e.getMessage.contains("v99"), e.getMessage)
  }

  test("verify: a missing package is named; the wrong interpreter refuses at start") {
    val w = worker()
    val drifts = w.verify(Map("okay-surely-not-installed" -> "1.0"))
    assertEquals(drifts.length, 1)
    assert(drifts.head.contains("okay-surely-not-installed"), drifts.head)
    assert(w.pythonVersion.startsWith("3"), w.pythonVersion)
    val e = intercept[IllegalStateException](PySubprocess.start("/no/such/venv/bin/python3"))
    assert(e.getMessage.contains("wrong-venv"), e.getMessage)
  }

  test("a dead worker makes the NEXT exchange throw — the supervisor's signal") {
    val w = worker()
    // os._exit skips the shim's loop entirely; the reply never comes
    intercept[IllegalStateException](call(w, "os:_exit", I64(0)))
  }
}
