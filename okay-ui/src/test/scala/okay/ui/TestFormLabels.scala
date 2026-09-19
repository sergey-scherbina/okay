package okay.ui

import okay.codec.Schema

/**
 * form-labels (specs/form-labels.md): a caller-stated label per
 * field, keyed by a dotted path or, failing that, a bare field name —
 * an override on what `Form.render` shows, never on what it decodes.
 *
 * `Addr` appears TWICE in `Person`, at `addr` and `birthplace`, so the
 * same bare field name `city` names two different fields at two
 * different depths — the shape the bare/dotted distinction exists for.
 */
class TestFormLabels extends munit.FunSuite {

  final case class Addr(city: String) derives Schema
  final case class Person(addr: Addr, birthplace: Addr, age: Option[Int], perMinute: Int)
    derives Schema

  private val blank = Form.blank[Person]

  /** every string a rendered form shows a reader: a Text's own string,
   * an Input's or a Check's label */
  private def strings(ui: Ui): Vector[String] = ui match
    case Ui.Text(s, _) => Vector(s)
    case Ui.Input(_, _, label, _, _) => Vector(label)
    case Ui.Check(_, _, label) => Vector(label)
    case Ui.Column(children, _) => children.flatMap(strings)
    case Ui.Row(children, _) => children.flatMap(strings)
    case _ => Vector.empty

  test("no labels: every name is the field name, exactly as Form.of[A] today") {
    val s = strings(Form.of[Person](Map.empty)(blank))
    assertEquals(strings(Form.of[Person](blank)), s, "the overload must not change the default")
    assert(s.contains("city"), s.toString)
    assert(s.contains("perMinute"), s.toString)
    assert(!s.exists(_.contains("City")), s.toString)
  }

  test("a dotted-path entry labels ONE occurrence, not the bare name's other ones") {
    val s = strings(Form.of[Person](Map("addr.city" -> "City (primary address)"))(blank))
    assert(s.contains("City (primary address)"), s.toString)
    // birthplace.city is untouched: the dotted entry did not leak to it
    assert(s.contains("city"), s.toString)
  }

  test("a bare-name entry labels every occurrence with no more specific dotted entry") {
    val s = strings(Form.of[Person](Map("city" -> "City"))(blank))
    assertEquals(s.count(_ == "City"), 2, s.toString)
    assert(!s.contains("city"), s.toString)
  }

  test("a dotted entry wins over a bare one for the same field") {
    val s = strings(Form.of[Person](Map("city" -> "City", "addr.city" -> "Primary city"))(blank))
    assert(s.contains("Primary city"), s.toString)
    assert(s.contains("City"), s.toString)
    assert(!s.contains("city"), s.toString)
  }

  test("the (optional) suffix applies to the LOOKED-UP label, not the raw field name") {
    val s = strings(Form.of[Person](Map("age" -> "Age"))(blank))
    assert(s.contains("Age (optional)"), s.toString)
    assert(!s.exists(_.contains("age (optional)")), s.toString)
  }

  test("a nested product's own title uses the lookup too, not only its leaves") {
    val s = strings(Form.of[Person](Map("addr" -> "Primary address"))(blank))
    assert(s.contains("Primary address"), s.toString)
    // its OWN field label is untouched by its parent's rename
    assert(s.contains("city"), s.toString)
  }

  test("ofWith carries the same labels, alongside its errors") {
    val s = strings(Form.ofWith[Person](Vector.empty, Map("perMinute" -> "requests per minute"))(blank))
    assert(s.contains("requests per minute"), s.toString)
  }
}
