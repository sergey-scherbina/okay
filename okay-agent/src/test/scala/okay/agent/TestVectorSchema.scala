package okay.agent

import okay.codec.{Json, Schema}

/** the fourth algebra covers the new case (codec-vector) */
class TestVectorSchema extends munit.FunSuite {
  test("a Vector field declares as a JSON-Schema array") {
    final case class Args(tags: Vector[String])
    given Schema[Args] = Schema.derived
    val Json.JObj(fs) = ToolSpec.jsonSchema(summon[Schema[Args]]): @unchecked
    val props = fs.toMap.apply("properties")
    assertEquals(Json.print(props),
      """{"tags":{"type":"array","items":{"type":"string"}}}""")
  }
}
