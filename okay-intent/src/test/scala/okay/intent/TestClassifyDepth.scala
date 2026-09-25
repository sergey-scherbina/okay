package okay.intent

import okay.codec.Schema

/** stack-safety-rest: `Classify.example` builds a skeleton from the
 * schema, once per level of the TYPE — and a derived schema of a
 * recursive type is a cycle, so the walk never ended */
class TestClassifyDepth extends munit.FunSuite:
  final case class Tree(label: String, kids: Vector[Tree]) derives Schema
  final case class Flat(name: String, tags: Vector[String]) derives Schema

  test("an example of a recursive type ends, showing the type once") {
    val ex = Classify.example[Tree]
    assert(ex.contains("\"label\""), ex)
    assert(ex.length < 200, ex)
  }

  test("a flat example is what it was") {
    assertEquals(Classify.example[Flat], """{"name":"...","tags":["..."]}""")
  }
