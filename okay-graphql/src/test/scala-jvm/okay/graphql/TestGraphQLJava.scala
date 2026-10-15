package okay.graphql

class TestGraphQLJava extends okay.testkit.Munit.Diagnosed:
  private val documents = List(
    """query Q($a: [Int!] = [1, 2], $b: String, $c: Filter = {text: "x", max: null}) {
      |  x: media(first: 3) @skip(if: false) { id ... on Book @include(if: $b) { pages } ...F }
      |  search(q: "a\"bé", f: 1.5e3, e: ENUM) { __typename }
      |}
      |fragment F on Film { minutes }
      |mutation M { say(word: "one") }
      |subscription S { tick }""".stripMargin,
    "{ a { b { c } } }",
    "query { a(x: [[1], [2, 3]], y: {z: {w: true}}) }")

  test("graphql-java reads what ours reads: the same document, positions too") {
    val theirs = GraphQLJava()
    for d <- documents do
      val ours = GraphQLParser.default.parse(d)
      note(s"$d\n  ours: $ours")
      assertEquals(theirs.parse(d), ours, d)
  }

  test("each reads the other's output: ours printed, read by graphql-java, is ours") {
    val theirs = GraphQLJava()
    for d <- documents do
      val printed = GraphQLParser.default.parse(d).toOption.get.print
      assertEquals(theirs.parse(printed).map(_.print), Right(printed), printed)
      assertEquals(theirs.parse(printed), GraphQLParser.default.parse(printed), printed)
  }

  test("both refuse a broken document; both bound the nesting") {
    val theirs = GraphQLJava()
    assert(theirs.parse("{ a { b }").isLeft)
    assert(GraphQLParser.default.parse("{ a { b }").isLeft)
    val deep = "{ a " * 70 + "{ b }" + " }" * 70
    assertEquals(theirs.parse(deep).left.map(_.message), Left("nesting deeper than 64"))
    assertEquals(GraphQLParser.default.parse(deep).left.map(_.message), Left("nesting deeper than 64"))
  }

  test("the executor with graphql-java's parser behind the facade") {
    given GraphQLParser = GraphQLJava()
    val schema = Schema(ObjectType[Unit]("Query")(List(Field("hello", Scalar.string)(_ => "world"))))
    assertEquals(okay.codec.Json.print(Executor.execute(schema, "{ hello }")), """{"data":{"hello":"world"}}""")
  }

  test("refused by name when the jar is absent") {
    assertEquals(GraphQLJava.missing(), None)
    assert(GraphQLJava.missing("graphql.NoSuchClass").exists(_.contains("com.graphql-java:graphql-java")))
  }
