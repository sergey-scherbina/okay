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
}
