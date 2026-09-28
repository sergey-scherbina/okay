package okay.dlm

import munit.FunSuite

class TestSupport extends FunSuite:

  val routes = Vector(
    Route.Fires("need", Map("what" -> "сантехник"), Support.Exact(Some("(?iU)\\bнужен\\b"))),
    Route.Fires("need", Map.empty, Support.Typo(1)),
    Route.Fires("offer", Map.empty, Support.Semantic(0.83f, Some("need"))),
    Route.Fires("offer", Map.empty, Support.Remembered(42L, 0.8f)),
    Route.Missing("accept", "deal"),
    Route.Unclear(Vector("need", "offer"), 0.41f))

  test("every route survives the wire") {
    for r <- routes do
      val back = Route.decode(Route.encode(r))
      assertEquals(back, Some(r), Route.encode(r).toString)
  }

  test("the wire always carries a layer and a number, derived and never compared") {
    assertEquals(Support.Exact(None).score, 1.0f)
    assertEquals(Support.Typo(2).score, 0.6f)
    assertEquals(Support.Semantic(0.7f, None).score, 0.7f)
    assertEquals(Support.Remembered(1L, 0.8f).score, 0.8f)
    assertEquals(Support.Typo(2).layer, Layer.Fuzzy)
    assertEquals(Support.Remembered(1L, 1f).layer, Layer.Memory)
  }

  test("a record written before the rule travelled decodes to Exact(None) — the honest reading") {
    val old = okay.codec.Json.parse("""{"route":"fires","intent":"need","by":"Rule","score":1.0}""")
    assertEquals(Route.decode(old), Some(Route.Fires("need", Map.empty, Support.Exact(None))))
  }

  test("a lesson rides in the rule field as taught:<offset>") {
    assertEquals(Support.of(Layer.Memory, 1f, Some("taught:9"), None), Support.Remembered(9L, 1f))
    assertEquals(Support.of(Layer.Memory, 1f, None, None), Support.Remembered(-1L, 1f))
  }

  test("`named` is the intent a route names, and Unclear names none") {
    assertEquals(routes.map(_.named), Vector(Some("need"), Some("need"), Some("offer"), Some("offer"), Some("accept"), None))
  }
