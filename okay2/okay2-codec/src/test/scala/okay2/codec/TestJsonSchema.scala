package okay2.codec

final case class Defaulted(city: String, zip: Int = 0, note: Option[String])

/** the JSON Schema algebra (okay-codec's JsonSchema, the parts of
 * TestSchemaFold and TestEnumeration that waited for it): a recursive
 * schema folds to a FINITE value — the back edge a `$ref`, the node
 * under `$defs` — and a vocabulary is an `enum` */
class TestJsonSchema extends munit.FunSuite {

  private def print(j: Json) = Json.print(j)
  /** spelled apart: a literal `"$defs"` reads to -Xlint as a missing interpolator */
  private val defsKey = "$" + "defs"

  test("scalars, containers and a product: required unless optional or defaulted, the default written") {
    assertEquals(print(JsonSchema.of(Schema.SInt)), """{"type":"integer"}""")
    assertEquals(print(JsonSchema.of(implicitly[Schema[Vector[Boolean]]])), """{"type":"array","items":{"type":"boolean"}}""")
    assertEquals(print(JsonSchema.of(implicitly[Schema[Defaulted]])),
      """{"type":"object","properties":{"city":{"type":"string"},"zip":{"type":"integer","default":0},"note":{"type":"string"}},"required":["city"]}""")
  }

  test("a recursive product folds to a finite schema: the back edge is $ref, the name is under $defs") {
    val j = JsonSchema.of(implicitly[Schema[Tree]])
    assert(print(j).contains("\"$ref\":\"#/$defs/Tree\""), print(j))
    j match {
      case Json.JObj(fs) =>
        val defs = fs.collectFirst { case ("$defs", Json.JObj(d)) => d }.getOrElse(fail("no $defs at the root"))
        assertEquals(defs.map(_._1), Vector("Tree"))
        assertEquals(print(defs.head._2), print(Json.JObj(fs.filterNot(_._1 == defsKey))))
      case other => fail(s"expected an object, got $other")
    }
  }

  test("a recursive sum folds the same way, and a self-edge met twice is one $ref each time") {
    val ref = "\"$ref\":\"#/$defs/Expr\""
    def refs(j: Json) = print(j).sliding(ref.length).count(_ == ref)
    JsonSchema.of(implicitly[Schema[Expr]]) match {
      case Json.JObj(fs) =>
        assertEquals(refs(Json.JObj(fs.filterNot(_._1 == defsKey))), 2)
        val defs = fs.collectFirst { case ("$defs", Json.JObj(d)) => d }.getOrElse(fail("no $defs"))
        assertEquals(defs.map(_._1), Vector("Expr"))
        assertEquals(refs(defs.head._2), 2)
      case other => fail(s"expected an object, got $other")
    }
  }

  test("a schema met twice without a cycle is inlined, not a $ref, and nothing is declared") {
    val s = print(JsonSchema.of(implicitly[Schema[(Defaulted, Defaulted)]]))
    assert(!s.contains("$ref") && !s.contains("$defs"), s)
  }

  test("an enumeration is an enum beside the underlying type; a plain refine stays plain; the switch turns it off") {
    val enumerated = print(JsonSchema.of(Hue.schema))
    assert(enumerated.contains("\"type\":\"string\"") && enumerated.contains("\"enum\":[\"red\",\"green\",\"blue\"]"), enumerated)
    assert(print(JsonSchema.of(implicitly[Schema[Paint]])).contains("\"enum\":[\"red\",\"green\",\"blue\"]"))
    val plain = Schema.refine[Hue, String](s => Hue.values.find(_.toString.equalsIgnoreCase(s)).toRight(s"unknown '$s'"), _.toString)
    assert(!print(JsonSchema.of(plain)).contains("enum"))
    assert(!print(JsonSchema.of(Hue.schema, vocabularies = false)).contains("enum"))
  }
}
