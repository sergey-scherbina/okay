package okay.codec

object TestStubs:
  final case class Line(sku: String, qty: Int, price: Double) derives Schema
  final case class Order(id: Long, lines: Vector[Line], note: Option[String], total: BigInt, raw: Array[Byte]) derives Schema
  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)
  enum Tree derives Schema:
    case Leaf(v: Int)
    case Node(children: List[Tree])

class TestStubs extends munit.FunSuite {
  import TestStubs.*

  private val py = Stubs.python(summon[Schema[Order]], summon[Schema[Shape]], summon[Schema[Tree]])
  private val ts = Stubs.typescript(summon[Schema[Order]], summon[Schema[Shape]], summon[Schema[Tree]])

  test("Python: a TypedDict per product, a Union of type-tagged cases per sum, dependencies first") {
    assert(py.contains("class Line(TypedDict):\n    sku: str\n    qty: int\n    price: float"), py)
    assert(py.contains("class Order(TypedDict):\n    id: int\n    lines: list[Line]\n    note: Optional[str]\n    total: int\n    raw: bytes"), py)
    assert(py.contains("class Circle(TypedDict):\n    type: Literal[\"Circle\"]\n    r: float"), py)
    assert(py.contains("Shape = Union[Circle, Rect]"), py)
    assert(py.indexOf("class Line(") < py.indexOf("class Order("), "a dependency after its user")
  }

  test("Python: a recursive type refers to itself by name, declared once") {
    assert(py.contains("class Node(TypedDict):\n    type: Literal[\"Node\"]\n    children: list[Tree]"), py)
    assertEquals(py.split("class Node\\(").length - 1, 1)
    assert(py.contains("from __future__ import annotations"), "the forward reference needs it")
  }

  test("TypeScript: the JSON codec's shapes — a sum externally tagged, null, digits for a BigInt") {
    assert(ts.contains("export type Shape =\n  | { Circle: Circle }\n  | { Rect: Rect };"), ts)
    assert(ts.contains("  note: string | null;"), ts)
    assert(ts.contains("  total: string;"), ts)
    assert(ts.contains("  id: number; // a Long: exact as a JS number only to 2^53"), ts)
    assert(ts.contains("export interface Node {\n  children: Tree[];\n}"), ts)
  }

  test("deterministic: the same schemas give the same text") {
    assertEquals(Stubs.python(summon[Schema[Order]], summon[Schema[Shape]], summon[Schema[Tree]]), py)
    assertEquals(Stubs.typescript(summon[Schema[Order]], summon[Schema[Shape]], summon[Schema[Tree]]), ts)
  }
}
