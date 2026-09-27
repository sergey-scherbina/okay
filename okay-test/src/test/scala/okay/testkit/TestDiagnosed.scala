package okay.testkit

import okay.diagnose.Diagnostics

class TestDiagnosed extends munit.FunSuite with Munit.Diagnosed {
  @volatile var snapshotTaken = false

  test("munit's instance keeps a FailException's class and appends to its message") {
    val d = Diagnostics()
    d.note("offered 3")
    val e = Munit.failures.extend(new munit.FailException("boom", munit.Location.empty), d.report)
    assert(e.isInstanceOf[munit.FailException], e.getClass)
    assert(e.getMessage.startsWith("boom") && e.getMessage.contains("offered 3"), e.getMessage)
    assertEquals(Munit.missing, None)
  }

  test("a passing test never evaluates its snapshot; the recorder is per test (part 1)") {
    note("from test A")
    onFailure { snapshotTaken = true; "expensive" }
  }

  test("a passing test never evaluates its snapshot; the recorder is per test (part 2)") {
    assert(!snapshotTaken, "the snapshot of a PASSING test was evaluated")
    assert(!recorded.contains("from test A"), recorded)
  }

  test("end to end: a SYNCHRONOUS test body that fails leaves with the diagnosis") {
    val sub = new munit.FunSuite with Munit.Diagnosed {}
    sub.note("saved e=1")
    val failing = new munit.Test("x", () => throw new munit.FailException("boom", munit.Location.empty))
    val transformed = sub.munitTestTransforms.last(failing)
    // a Future-answering test: Await does not exist on Scala.js
    transformed.body().failed.map { boxed =>
      // the Future boxes the AssertionError again on the way out; munit unboxes it
      val out = boxed match
        case x: java.util.concurrent.ExecutionException if x.getCause != null => x.getCause
        case other => other
      assert(out.isInstanceOf[munit.FailException], out.getClass)
      assert(out.getMessage.startsWith("boom") && out.getMessage.contains("saved e=1"), out.getMessage)
    }(using munitExecutionContext)
  }
}
