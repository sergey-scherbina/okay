package okay.dlm

import munit.FunSuite

class TestCalibration extends FunSuite:
  import Calibration.*

  val rows = Vector(
    Seen(Layer.Rule, "need", 1f, false, "a", "нужен сантехник"),
    Seen(Layer.Rule, "need", 1f, true, "a", "нужен отдых"),
    Seen(Layer.Rule, "need", 1f, true, "a", "нужен отдых"),
    Seen(Layer.Semantic, "offer", 0.9f, false),
    Seen(Layer.Semantic, "offer", 0.9f, true),
    Seen(Layer.Semantic, "unclear", 0.4f, false),
    Seen(Layer.Memory, "listings", 1f, false))

  test("a recorded route becomes a row under its own layer; Missing is nothing to score") {
    assertEquals(seen(Route.Fires("need", Map.empty, Support.Typo(1)), wrong = false).map(_.layer), Some(Layer.Fuzzy))
    assertEquals(seen(Route.Unclear(Vector.empty, 0.3f), wrong = true).map(r => (r.intent, r.score)), Some(("unclear", 0.3f)))
    assertEquals(seen(Route.Missing("a", "b"), wrong = false), None)
  }

  test("Brier only where a probability exists") {
    val b = brier(rows).get
    // (0.9-1)² + (0.9-0)² + (0.4-1)² over three
    assertEqualsDouble(b, (0.01 + 0.81 + 0.36) / 3, 1e-6)
    assertEquals(brier(rows.filter(_.layer == Layer.Rule)), None)
  }

  test("bands and layers count what they claim") {
    val sem = rows.filter(_.layer == Layer.Semantic)
    assertEquals(bands(sem).filter(_.n > 0).map(b => (b.lo, b.n, b.wrong)), Vector((0.3, 1, 0), (0.8, 2, 1)))
    assertEquals(byLayer(rows).map(r => (r.layer, r.n, r.wrong)),
      Vector((Layer.Rule, 3, 2), (Layer.Semantic, 3, 1), (Layer.Memory, 1, 0)))
  }

  test("a rule's own number carries the people and the sentences beside the count") {
    val per = perIntent(rows, minFired = 1)
    assertEquals(per.map(p => (p.intent, p.fired, p.wrong, p.people, p.sentences)), Vector(("need", 3, 2, 1, 1)))
    assertEquals(perIntent(rows, minFired = 10), Vector.empty)
  }
