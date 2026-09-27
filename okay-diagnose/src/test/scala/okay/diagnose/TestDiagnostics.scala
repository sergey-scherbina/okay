package okay.diagnose

class TestDiagnostics extends munit.FunSuite {
  test("the core needs no framework: around adds the report, suppressed, class and cause kept") {
    val d = Diagnostics()
    d.note("offered 3")
    d.onFailure("state: closing=true")
    val cause = IllegalStateException("root")
    val e = intercept[RuntimeException](Diagnostics.around(d)(throw RuntimeException("outer", cause))(using FailureFormat.suppressed))
    assertEquals(e.getCause, cause)
    val s = e.getSuppressed.toList.map(_.getMessage).mkString
    assert(s.contains("offered 3") && s.contains("state: closing=true"), s)
    assertEquals(Diagnostics.around(Diagnostics())(42)(using FailureFormat.suppressed), 42)
  }

  test("Flight keeps the newest notes in order, and says how many fell off") {
    val f = Flight(3)
    (1 to 5).foreach(i => f.note(s"n$i"))
    val d = f.dump
    assert(d.contains("2 older notes dropped"), d)
    assert(d.indexOf("n3") < d.indexOf("n4") && d.indexOf("n4") < d.indexOf("n5") && !d.contains("n2"), d)
  }

  test("Diagnosable: a component's state as a snapshot, taken only on failure") {
    final case class Gauge(n: Int)
    given Diagnosable[Gauge] = Diagnosable.of(g => s"gauge=${g.n}")
    val d = Diagnostics()
    d.snapshot(Gauge(7))
    assert(d.report.contains("gauge=7"), d.report)
  }
}
