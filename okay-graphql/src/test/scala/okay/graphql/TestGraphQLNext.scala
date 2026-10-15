package okay.graphql

import okay.{Async, AsyncCont, StreamCont}
import okay.codec.Json
import okay.codec.Json.*
import okay.one.Cont.pure
import okay.one.row.*
import scala.collection.mutable

/** a library: books and films, both media (an interface), a search answering either (a union) */
object Library:
  enum Medium:
    case Book(id: String, title: String, pages: Int)
    case Film(id: String, title: String, minutes: Int)

    def ident: String = this match
      case Book(i, _, _) => i
      case Film(i, _, _) => i
    def name: String = this match
      case Book(_, t, _) => t
      case Film(_, t, _) => t
  import Medium.*

  val all: List[Medium] = List(Book("b1", "Dune", 412), Film("f1", "Alien", 117), Book("b2", "Solaris", 204))

  lazy val bookT: ObjectType[Book] = ObjectType[Book]("Book", Some("A printed medium"))(List(
    Field("id", Scalar.id)(_.id),
    Field("title", Scalar.string)(_.title),
    Field("pages", Scalar.int)(_.pages),
    Field[Book, Option[String]]("isbn", Nullable(Scalar.string))(_ => None).deprecate("no longer kept"),
  ))
  lazy val filmT: ObjectType[Film] = ObjectType[Film]("Film")(List(
    Field("id", Scalar.id)(_.id),
    Field[Film, String]("title", Scalar.string)(_.title).describe("The title as released"),
    Field("minutes", Scalar.int)(_.minutes),
  ))
  lazy val mediumT: InterfaceType[Medium] = InterfaceType[Medium]("Medium")(
    List(Field("id", Scalar.id)(_.ident), Field("title", Scalar.string)(_.name)),
    List(Member[Medium, Book](bookT) { case b: Book => b }, Member[Medium, Film](filmT) { case f: Film => f }))
  lazy val resultT: UnionType[Medium] = UnionType[Medium]("Result")(
    List(Member[Medium, Book](bookT) { case b: Book => b }, Member[Medium, Film](filmT) { case f: Film => f }))
  val genre: InEnum[String] = InEnum("Genre", List("SF" -> "sf", "DRAMA" -> "drama"))
  val filter: InObject[(String, Option[Int])] = InObject[(String, Option[Int])]("Filter")(
    List(Arg("text", InScalar.string), Arg("max", Optional(InScalar.int))),
    a => (a(Arg("text", InScalar.string)), a(Arg("max", Optional(InScalar.int)))))

  val q = Arg("q", InScalar.string)
  val f = Arg("filter", Optional(filter))
  val g = Arg("genre", Optional(genre))
  val schema: Schema = Schema(ObjectType[Unit]("Query")(List(
    Field("media", ListOf(mediumT))(_ => all),
    Field.fetch("search", ListOf(resultT), q)((_, a) => pure(all.filter(_.toString.contains(a(q))))),
    Field.fetch("find", ListOf(mediumT), f, g)((_, a) =>
      pure(all.filter(m => a(f).forall((t, _) => m.toString.contains(t))))),
    Field("odd", resultT)(_ => Book("x", "?", 0)),
  )))

class TestGraphQLNext extends okay.testkit.Munit.Diagnosed:
  import Library.*
  private given scala.concurrent.ExecutionContext = munitExecutionContext

  private def run(s: Schema, q: String, vars: Json = JObj(Vector.empty)): Json =
    val out = Executor.execute(s, q, vars)
    note(s"$q => ${Json.print(out)}")
    out
  private def json(s: String): Json = Json.parse(s)
  private def messages(out: Json): List[String] = out match
    case JObj(fs) => fs.collectFirst { case ("errors", JArr(es)) => es.toList.collect {
      case JObj(e) => e.collectFirst { case ("message", JStr(m)) => m }.getOrElse("")
    } }.getOrElse(Nil)
    case _ => Nil

  test("interfaces: their fields, fragments on members, __typename the member's") {
    assertEquals(run(schema, """{ media { __typename id ... on Book { pages } ... on Film { minutes } } }"""), json(
      """{"data":{"media":[{"__typename":"Book","id":"b1","pages":412},{"__typename":"Film","id":"f1","minutes":117},
        |{"__typename":"Book","id":"b2","pages":204}]}}""".stripMargin))
  }

  test("unions: only fragments and __typename; a fragment on the interface applies to its members") {
    assertEquals(run(schema, """{ search(q: "Alien") { __typename ... on Medium { title } } }"""),
      json("""{"data":{"search":[{"__typename":"Film","title":"Alien"}]}}"""))
    assertEquals(messages(run(schema, """{ search(q: "x") { title } }""")), List("Result has no field title"))
    assertEquals(messages(run(schema, """{ media { ... on Result { __typename } } }""")), Nil)
  }

  test("a value no member picks is a field error") {
    val s = Schema(ObjectType[Unit]("Query")(List(Field("odd", Nullable(UnionType[Int]("Num")(
      List(Member[Int, Medium.Book](bookT) { case 1 => Medium.Book("1", "one", 1) }))))(_ => Some(2)))))
    assertEquals(messages(run(s, """{ odd { __typename } }""")), List("no member of Num is the value 2"))
  }

  test("Schema.check: a member missing an interface's field") {
    lazy val iT: InterfaceType[Int] = InterfaceType[Int]("Named")(List(Field("name", Scalar.string)(_.toString)),
      List(Member[Int, Int](noName) { case i => i }))
    lazy val noName: ObjectType[Int] = ObjectType[Int]("Nameless")(List(Field("id", Scalar.int)(identity)))
    val s = Schema(ObjectType[Unit]("Query")(List(Field("x", iT)(_ => 1))))
    assertEquals(s.check, List("Nameless is a member of Named but has no field name"))
    assertEquals(schema.check, Nil)
  }

  test("SDL: interfaces, unions, implements, descriptions, deprecation, inputs") {
    val sdl = schema.sdl
    note(sdl)
    assert(sdl.contains("interface Medium {\n  id: ID!\n  title: String!\n}"), sdl)
    assert(sdl.contains("\"A printed medium\"\ntype Book implements Medium {"), sdl)
    assert(sdl.contains("  isbn: String @deprecated(reason: \"no longer kept\")"), sdl)
    assert(sdl.contains("  \"The title as released\"\n  title: String!"), sdl)
    assert(sdl.contains("union Result = Book | Film"), sdl)
    assert(sdl.contains("input Filter {\n  text: String!\n  max: Int\n}"), sdl)
    assert(sdl.contains("enum Genre {\n  SF\n  DRAMA\n}"), sdl)
    assert(!sdl.contains("__"), sdl)
  }

  test("introspection: __type of an object, an interface, a union, an input, an enum") {
    assertEquals(run(schema, """{ __type(name: "Book") { kind name description interfaces { name }
        fields { name type { kind name ofType { kind name } } } } }"""), json(
      """{"data":{"__type":{"kind":"OBJECT","name":"Book","description":"A printed medium","interfaces":[{"name":"Medium"}],
        |"fields":[{"name":"id","type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"ID"}}},
        |{"name":"title","type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String"}}},
        |{"name":"pages","type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Int"}}}]}}}""".stripMargin))
    assertEquals(run(schema, """{ __type(name: "Book") { fields(includeDeprecated: true) { name isDeprecated deprecationReason } } }"""),
      json("""{"data":{"__type":{"fields":[{"name":"id","isDeprecated":false,"deprecationReason":null},
        |{"name":"title","isDeprecated":false,"deprecationReason":null},{"name":"pages","isDeprecated":false,"deprecationReason":null},
        |{"name":"isbn","isDeprecated":true,"deprecationReason":"no longer kept"}]}}}""".stripMargin))
    assertEquals(run(schema, """{ m: __type(name: "Medium") { kind possibleTypes { name } } r: __type(name: "Result") { kind possibleTypes { name } }
        f: __type(name: "Filter") { kind inputFields { name type { name kind } defaultValue } }
        g: __type(name: "Genre") { kind enumValues { name } } none: __type(name: "Nope") { name } }"""), json(
      """{"data":{"m":{"kind":"INTERFACE","possibleTypes":[{"name":"Book"},{"name":"Film"}]},
        |"r":{"kind":"UNION","possibleTypes":[{"name":"Book"},{"name":"Film"}]},
        |"f":{"kind":"INPUT_OBJECT","inputFields":[{"name":"text","type":{"name":null,"kind":"NON_NULL"},"defaultValue":null},
        |{"name":"max","type":{"name":"Int","kind":"SCALAR"},"defaultValue":null}]},
        |"g":{"kind":"ENUM","enumValues":[{"name":"SF"},{"name":"DRAMA"}]},"none":null}}""".stripMargin))
  }

  test("introspection: GraphiQL's query answers, every type named once, the roots and directives") {
    val out = run(schema, Introspection.query)
    assertEquals(messages(out), Nil)
    val text = Json.print(out)
    for t <- List("Query", "Book", "Film", "Medium", "Result", "Filter", "Genre", "__Schema", "__Type", "String") do
      assertEquals(s"\"name\":\"$t\",\"description\"".r.findAllIn(text).size, 1, t)
    assert(text.contains("\"queryType\":{\"name\":\"Query\"}"), text.take(300))
    assert(text.contains("\"name\":\"skip\""), text)
  }

  test("validation: each rule refuses its document, with locations") {
    def refused(q: String, vars: Json = JObj(Vector.empty)) = messages(run(schema, q, vars))
    assertEquals(refused("query A { media { id } } query A { media { id } }"), List("there can be only one operation named A"))
    assertEquals(refused("{ media { id } } query B { media { id } }"), List("an anonymous operation must be alone"))
    assertEquals(refused("{ media { id: title } media { id } }"), List("fields id conflict: title and id"))
    assertEquals(refused("""{ a: search(q: "x") { __typename } a: search(q: "y") { __typename } }"""),
      List("fields a conflict: search(q: \"x\") and search(q: \"y\")"))
    assertEquals(refused("{ media { ... on Book { x: pages } ... on Film { x: minutes } } }"), Nil)
    assertEquals(refused("""{ search { __typename } }"""), List("Query.search: argument q of type String! is required"))
    assertEquals(refused("""{ search(q: 1) { __typename } }"""), List("Query.search(q:): String cannot represent 1"))
    assertEquals(refused("""{ find(filter: {text: "x", nope: 1}) { id } }"""), List("Query.find(filter:): Filter has no field nope"))
    assertEquals(refused("""{ find(filter: {max: 1}) { id } }"""), List("Query.find(filter:): Filter.text of type String! is required"))
    assertEquals(refused("""{ find(genre: HORROR) { id } }"""), List("Query.find(genre:): Genre has no value HORROR"))
    assertEquals(refused("{ media { ...F } } fragment F on Book { pages } fragment G on Film { minutes }"),
      List("fragment G is never used"))
    assertEquals(refused("{ media { ...F } } fragment F on Nope { id }"), List("fragment F is on an unknown type Nope"))
    assertEquals(refused("{ media { ...F } }"), List("unknown fragment F"))
    assertEquals(refused("{ media { id ... on Query { __typename } } }"), List("an inline fragment on Query can never apply to Medium"))
    assertEquals(refused("{ media { id @nope } }"), List("unknown directive @nope"))
    assertEquals(refused("{ media { id @skip } }"), List("@skip: argument if of type Boolean! is required"))
    assertEquals(refused("query($x: Int) { media { id } }"), List("variable $x is never used"))
    assertEquals(refused("{ search(q: $x) { __typename } }"), List("variable $x is not defined"))
    assertEquals(refused("query($x: Int) { search(q: $x) { __typename } }"), List("variable $x of type Int where String! is expected"))
    assertEquals(refused("query($x: String) { search(q: $x) { __typename } }"), List("variable $x of type String where String! is expected"))
    assertEquals(refused("query($x: String = \"a\") { search(q: $x) { __typename } }"), Nil)
    assertEquals(refused("query($x: Book) { media { id } }").head, "variable $x of Book: Book is not an input type")
    assertEquals(refused("mutation { x }"), List("the schema has no mutations"))
    assertEquals(run(schema, "{\n  media {\n    nope\n  }\n}"),
      json("""{"errors":[{"message":"Medium has no field nope","locations":[{"line":3,"column":5}]}]}"""))
  }

  test("variables coerced to their declared types: a single value to a list, an enum, an input object") {
    val s = Schema(ObjectType[Unit]("Query")(List(
      Field.fetch("echo", ListOf(Scalar.string), Arg("xs", InList(genre)))((_, a) => pure(a(Arg("xs", InList(genre))))))))
    assertEquals(run(s, "query($g: [Genre!]!) { echo(xs: $g) }", json("""{"g": "SF"}""")), json("""{"data":{"echo":["sf"]}}"""))
    assertEquals(messages(run(s, "query($g: [Genre!]!) { echo(xs: $g) }", json("""{"g": ["NOIR"]}"""))),
      List("variable $g: Genre has no value NOIR"))
    assertEquals(run(schema, """query($f: Filter) { find(filter: $f) { id } }""", json("""{"f": {"text": "Dune"}}""")),
      json("""{"data":{"find":[{"id":"b1"}]}}"""))
  }

  test("prepare once, execute many; the cache keeps it") {
    val p = Executor.prepare(schema, """query($q: String!) { search(q: $q) { ... on Medium { id } } }""").toOption.get
    assertEquals(p.execute(json("""{"q": "Dune"}""")), json("""{"data":{"search":[{"id":"b1"}]}}"""))
    assertEquals(p.execute(json("""{"q": "Solaris"}""")), json("""{"data":{"search":[{"id":"b2"}]}}"""))
    val cache = QueryCache(schema, capacity = 2)
    cache.execute("{ media { id } }"): Unit
    cache.execute("{ media { id } }"): Unit
    assertEquals(cache.size, 1)
    assert(cache.prepare("{ media { id } }") eq cache.prepare("{ media { id } }"))
    cache.execute("{ media { title } }"): Unit
    cache.execute("{ media { __typename } }"): Unit
    assertEquals(cache.size, 2)
  }

  test("Limits.fields: a fragment bomb is refused without being expanded") {
    val bomb = (1 to 20).map(i => s"fragment F$i on Book { ${List.fill(10)(s"...F${i + 1}").mkString(" ")} }").mkString("\n") +
      "\nfragment F21 on Book { id }\n{ media { ... on Book { ...F1 } } }"
    val out = run(schema, bomb)
    assertEquals(messages(out), List("the operation selects more than 10000 fields (Limits.fields)"))
  }

  test("async sources: a round's sources called at once, in parallel") {
    // two sources each answering only once BOTH were called: sequential calls would never finish
    val called = mutable.ArrayBuffer.empty[(String, Map[String, Int] => Unit)]
    def gate(name: String): AsyncSource[String, Int] = AsyncSource[String, Int](name) { keys =>
      AsyncCont.await[Map[String, Int]] { k =>
        called += ((name, k))
        if called.size == 2 then called.foreach((n, k) => k(keys.map(_ -> n.length).toMap))
      }.at[Async +: Pure]
    }
    val a = gate("a")
    val bb = gate("bb")
    val s = Schema(ObjectType[Unit]("Query")(List(
      Field.fetch("x", Scalar.int)((_, _) => Fetch.get(a, "k")),
      Field.fetch("y", Scalar.int)((_, _) => Fetch.get(bb, "k")))))
    AsyncCont.runAsync(Executor.executeAsync(s, "{ x y }")).map { out =>
      assertEquals(out, json("""{"data":{"x":1,"y":2}}"""))
      assertEquals(called.map(_._1).toList, List("a", "bb"))
    }
  }

  test("an async source run synchronously is its fields' error") {
    val src = AsyncSource[String, Int]("slow")(keys => pure(keys.map(_ -> 1).toMap))
    val s = Schema(ObjectType[Unit]("Query")(List(Field.fetch("x", Nullable(Scalar.int))((_, _) => Fetch.get(src, "k").map(Some(_))))))
    assertEquals(messages(run(s, "{ x }")), List("slow is an AsyncSource: run the query by executeAsync"))
  }

  test("subscriptions: each value of the stream executed as the selection") {
    val s = Schema(ObjectType[Unit]("Query")(List(Field("ok", Scalar.boolean)(_ => true))),
      subscription = Some(SubscriptionType("Subscription")(List(
        Subscription("released", mediumT, Arg("kind", InScalar.string))(a =>
          StreamCont.fromList(all.filter(_.productPrefix == a(Arg("kind", InScalar.string)))))))))
    val stream = Executor.subscribe(s, """subscription { released(kind: "Book") { title ... on Book { pages } } }""")
    assertEquals(messages(Executor.subscribe(s, "subscription { a: released(kind: \"Book\") { id } b: released(kind: \"Film\") { id } }")
      .left.getOrElse(JNull)), List("a subscription must select exactly one top-level field"))
    assert(s.sdl.contains("type Subscription {\n  released(kind: String!): Medium!\n}"), s.sdl)
    AsyncCont.runAsync(stream.toOption.get.toVector).map { events =>
      assertEquals(events.toList, List(
        json("""{"data":{"released":{"title":"Dune","pages":412}}}"""),
        json("""{"data":{"released":{"title":"Solaris","pages":204}}}""")))
    }
  }

  test("the printer: a document printed parses back to itself") {
    val q = """query Q($a: [Int!] = [1, 2], $b: String) @skip(if: false) { x: media { id ... on Book @include(if: true) { pages }
              |...F } search(q: "a\"b") { __typename } } fragment F on Film { minutes }""".stripMargin
    val d = GraphQLParser.default.parse(q).toOption.get
    val again = GraphQLParser.default.parse(d.print).toOption.get
    assertEquals(Printer.document(again), d.print)
    assertEquals(strip(again), strip(d))
  }

  /** a document without its positions */
  private def strip(d: Document): String = d.print
