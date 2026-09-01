package okay.ui

import okay.codec.{Cbor, Json, Schema}

/**
 * The exhibit codec-vector was filed for: the WHOLE Ui tree — a
 * recursive sum whose cases hold Vectors — now derives its Schema and
 * round-trips both wires. WireJson stays as the wire's own compact
 * dialect (the MCP-dialect precedent); what this buys is that the
 * hand mapping is a CHOICE now, not a workaround, and any OTHER type
 * built like Ui derives for free.
 */
class TestDerivedUi extends munit.FunSuite {

  given Schema[Style] = Schema.derived
  given Schema[Ui] = Schema.derived
  given Schema[Event] = Schema.derived
  given Schema[Patch] = Schema.derived

  val tree: Ui = Ui.Column(Vector(
    Ui.Text("title", Style(bold = true)),
    Ui.Row(Vector(Ui.Button("ok", "ok"), Ui.Select(Vector("a", "b"), 1, "sel")), "row"),
    Ui.Input("v", "name", "label"),
    Ui.Check(true, "sure", "sure?")), "root")

  test("the recursive Ui sum derives and round-trips JSON and CBOR") {
    assertEquals(Json.read[Ui](Json.write(tree)), Right(tree))
    assertEquals(Cbor.read[Ui](Cbor.write(tree)), Right(tree))
  }

  test("events and patches derive too — the whole wire vocabulary") {
    val events: Vector[Event] = Vector(Event.Pressed("k"), Event.Edited("k", "v"),
      Event.Toggled("k", true), Event.Chosen("k", 2), Event.Key('x'),
      Event.Resized(80, 24), Event.Closed)
    assertEquals(Json.read[Vector[Event]](Json.write(events)), Right(events))

    val patches: Vector[Patch] = Vector(
      Patch.Replace(List(1), tree), Patch.SetText(List(0), "s"),
      Patch.Reorder(Nil, Vector(1, 0)), Patch.Insert(List(2), 0, tree),
      Patch.Remove(Nil, 1))
    assertEquals(Cbor.read[Vector[Patch]](Cbor.write(patches)), Right(patches))
  }
}
