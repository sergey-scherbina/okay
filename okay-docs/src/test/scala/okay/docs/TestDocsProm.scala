package okay.docs

/** the counters as okay-ops serves them — rendered here since
 * ops-docs-edge, so okay-ops needs no dependency on this module */
class TestDocsProm extends munit.FunSuite:

  test("Docs.Stats render as named counters with the engine label; no pieces, no text") {
    val ds = Docs.Stats("dynamo", 5, 4, 3, 2, 1, 0, 6, 0)
    val out = Docs.prom(Vector(("orders", () => ds)))
    assert(out.contains("# TYPE okay_docs_stale_total counter\n"), out)
    assert(out.contains("okay_docs_stale_total{name=\"orders\",engine=\"dynamo\"} 1"), out)
    assert(out.contains("okay_docs_queries_total{name=\"orders\",engine=\"dynamo\"} 6"), out)
    assertEquals(Docs.prom(Vector.empty), "")
  }

  test("a label is escaped the Prometheus way: a quote and a newline cannot end the line") {
    val out = Docs.prom(Vector(("a\"b\nc", () => Docs.Stats("m", 0, 0, 0, 0, 0, 0, 0, 0))))
    assert(out.contains("name=\"a\\\"b\\nc\""), out)
  }
