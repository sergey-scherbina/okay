package okay.agent

import okay.codec.{Json, Schema}

/** codec-defaults at the tool boundary: a defaulted field is one the
 * model MAY omit — so it leaves `required` and advertises `default`,
 * and ToolSpec.args survives the omission by falling back */
class TestDefaultsSchema extends munit.FunSuite {

  final case class Args(query: String, limit: Int = 10, deep: Option[Boolean] = None)
  given Schema[Args] = Schema.derived

  test("a defaulted field is not required and carries its default") {
    val Json.JObj(fs) = ToolSpec.jsonSchema(summon[Schema[Args]]): @unchecked
    val m = fs.toMap
    assertEquals(Json.print(m("required")), """["query"]""")
    val Json.JObj(props) = m("properties"): @unchecked
    assertEquals(Json.print(props.toMap.apply("limit")),
      """{"type":"integer","default":10}""")
  }

  test("args decode survives the omission the schema just allowed") {
    assertEquals(ToolSpec.args[Args](ToolCall("t1", "search", Json.parse("""{"query":"q"}"""))),
      Right(Args("q", 10, None)))
  }
}
