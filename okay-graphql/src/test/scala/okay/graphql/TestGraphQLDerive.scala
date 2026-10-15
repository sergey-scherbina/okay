package okay.graphql

import okay.codec.Json
import okay.codec.Json.*
import okay.one.Cont.pure

/** a shop described by its case classes: the schema derived, the fetched fields by hand */
object DerivedShop:
  enum Tier:
    case Free, Pro
  final case class Address(city: String, zip: Option[String])
  final case class User(id: String, name: String, tier: Tier, address: Address, tags: List[String],
      friend: Option[User])
  final case class Order(id: Int, total: Double, userId: String)
  final case class Filter(text: String, tier: Option[Tier], limit: Option[Int])

  val alice: User = User("u1", "Alice", Tier.Pro, Address("Kyiv", Some("01001")), List("vip"), None)
  val bob: User = User("u2", "Bob", Tier.Free, Address("Lviv", None), Nil, Some(alice))
  val orders: Map[String, List[Order]] = Map("u1" -> List(Order(1, 9.5, "u1")), "u2" -> Nil)
  val ordersOf: Source[String, List[Order]] = Source("orders")(ids => ids.map(id => id -> orders(id)).toMap)

  given EnumType[Tier] = EnumType.derive[Tier]
  given InEnum[Tier] = InEnum.derive[Tier]
  given ObjectType[Address] = ObjectType.derive[Address]()
  given orderT: ObjectType[Order] = ObjectType.derive[Order]()
  given userT: ObjectType[User] = ObjectType.derive[User](
    Field.fetch("orders", ListOf(orderT))((u, _) => Fetch.get(ordersOf, u.id)),
    Field("name", Scalar.string)(_.name.toUpperCase))
  given InObject[Filter] = InObject.derive[Filter]

  val filter: Arg[Filter] = Arg("filter", summon[InObject[Filter]])
  val schema: Schema = Schema(ObjectType[Unit]("Query")(List(
    Field("users", ListOf(userT))(_ => List(alice, bob)),
    Field.fetch("find", ListOf(userT), filter)((_, a) =>
      val f = a(filter)
      pure(List(alice, bob).filter(u => u.name.contains(f.text) && f.tier.forall(_ == u.tier)).take(f.limit.getOrElse(10)))),
    Field.fetch("missing", Nullable(Scalar.string))((_, _) =>
      FieldError.raise[Option[String]]("no such thing", "code" -> JStr("NOT_FOUND"), "id" -> JNum(7))),
    Field.fetch("broken", Nullable(Scalar.string))((_, _) => okay.one.Cont.defer(throw IllegalStateException("boom"))),
  )))

class TestGraphQLDerive extends okay.testkit.Munit.Diagnosed:
  import DerivedShop.*

  private def run(q: String, vars: Json = JObj(Vector.empty)): Json =
    val out = Executor.execute(schema, q, vars)
    note(s"$q => ${Json.print(out)}")
    out

  test("a case class's object type: names, types, nullables, lists, nested, an enum; extra fields added and replacing") {
    assertEquals(run("""{ users { id name tier address { city zip } tags friend { id } orders { id total userId } } }"""),
      Json.parse("""{"data":{"users":[
        |{"id":"u1","name":"ALICE","tier":"Pro","address":{"city":"Kyiv","zip":"01001"},"tags":["vip"],"friend":null,
        | "orders":[{"id":1,"total":9.5,"userId":"u1"}]},
        |{"id":"u2","name":"BOB","tier":"Free","address":{"city":"Lviv","zip":null},"tags":[],"friend":{"id":"u1"},
        | "orders":[]}]}}""".stripMargin))
  }

  test("the SDL of derived types is a hand-written one's") {
    val sdl = schema.sdl
    note(sdl)
    assert(sdl.contains("type User {\n  id: String!\n  name: String!\n  tier: Tier!\n  address: Address!\n  tags: [String!]!\n" +
      "  friend: User\n  orders: [Order!]!\n}"), sdl)
    assert(sdl.contains("type Address {\n  city: String!\n  zip: String\n}"), sdl)
    assert(sdl.contains("enum Tier {\n  Free\n  Pro\n}"), sdl)
    assert(sdl.contains("input Filter {\n  text: String!\n  tier: Tier\n  limit: Int\n}"), sdl)
    assertEquals(schema.check, Nil)
  }

  test("an input object derived and decoded, from a literal and from a variable") {
    assertEquals(run("""{ find(filter: {text: "o", tier: Free}) { id } }"""), Json.parse("""{"data":{"find":[{"id":"u2"}]}}"""))
    assertEquals(run("""query($f: Filter!) { find(filter: $f) { id } }""", Json.parse("""{"f": {"text": "", "limit": 1}}""")),
      Json.parse("""{"data":{"find":[{"id":"u1"}]}}"""))
    val bad = run("""{ find(filter: {text: "o", tier: Gold}) { id } }""")
    assert(Json.print(bad).contains("Tier has no value Gold"), Json.print(bad))
  }

  test("introspection sees derived types as written ones") {
    assertEquals(run("""{ __type(name: "Address") { kind fields { name type { kind name } } } }"""), Json.parse(
      """{"data":{"__type":{"kind":"OBJECT","fields":[{"name":"city","type":{"kind":"NON_NULL","name":null}},
        |{"name":"zip","type":{"kind":"SCALAR","name":"String"}}]}}}""".stripMargin))
  }

  test("extensions: a resolver's FieldError, a plain failure without, request errors' codes") {
    assertEquals(run("""{ missing broken }"""), Json.parse(
      """{"data":{"missing":null,"broken":null},"errors":[
        |{"message":"no such thing","locations":[{"line":1,"column":3}],"path":["missing"],"extensions":{"code":"NOT_FOUND","id":7}},
        |{"message":"boom","locations":[{"line":1,"column":11}],"path":["broken"]}]}""".stripMargin))
    def codes(j: Json) = Json.print(j).split("\"code\":\"").toList.drop(1).map(_.takeWhile(_ != '"'))
    assertEquals(codes(run("{ users ")), List("GRAPHQL_PARSE_FAILED"))
    assertEquals(codes(run("{ nope }")), List("GRAPHQL_VALIDATION_FAILED"))
    assertEquals(codes(run("query($f: Filter!) { find(filter: $f) { id } }", Json.parse("""{"f": 1}"""))), List("BAD_USER_INPUT"))
    assertEquals(codes(Executor.execute(schema, "query A { users { id } } query B { users { id } }")), List("OPERATION_RESOLUTION_FAILURE"))
  }
