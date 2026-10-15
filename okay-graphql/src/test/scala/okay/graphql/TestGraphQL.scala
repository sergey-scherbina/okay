package okay.graphql

import okay.codec.Json
import okay.codec.Json.*
import okay.one.*
import okay.one.Cont.{defer, pure}
import scala.collection.mutable

/** the shop the suites ask: users, their orders, an order's user; every source counts its calls */
object Shop:
  final case class User(id: String, name: String, email: Option[String], tier: Tier)
  final case class Order(id: Int, user: String, total: Double)
  enum Tier:
    case Free, Pro

  val users: Map[String, User] = (1 to 100).map { i =>
    val id = s"u$i"
    id -> User(id, s"user $i", if i % 2 == 0 then Some(s"u$i@shop") else None, if i % 10 == 0 then Tier.Pro else Tier.Free)
  }.toMap
  val orders: Map[String, List[Order]] = users.keys.map { id =>
    val n = id.drop(1).toInt
    id -> (0 until n % 3).map(k => Order(n * 10 + k, id, n + k / 10.0)).toList
  }.toMap

  /** a source recording every call's keys */
  final class Counted[K, V](val name: String, data: K => Option[V]) extends Source[K, V]:
    val calls: mutable.ArrayBuffer[Seq[K]] = mutable.ArrayBuffer.empty
    def fetch(keys: Seq[K]): Map[K, V] =
      calls += keys
      keys.flatMap(k => data(k).map(k -> _)).toMap

  final class Sources:
    val user = Counted[String, User]("users", users.get)
    val ordersOf = Counted[String, List[Order]]("orders", orders.get)
    val log = mutable.ArrayBuffer.empty[String]

  val tier: EnumType[Tier] = EnumType("Tier", List("FREE" -> Tier.Free, "PRO" -> Tier.Pro))
  val tierIn: InEnum[Tier] = InEnum("Tier", List("FREE" -> Tier.Free, "PRO" -> Tier.Pro))

  def schema(s: Sources): Schema =
    lazy val userT: ObjectType[User] = ObjectType[User]("User")(List(
      Field("id", Scalar.id)(_.id),
      Field("name", Scalar.string)(_.name),
      Field("email", Nullable(Scalar.string))(_.email),
      Field("tier", tier)(_.tier),
      Field.fetch("orders", ListOf(orderT))((u, _) => Fetch.get(s.ordersOf, u.id)),
      Field.fetch("broken", Scalar.string)((_, _) => defer(throw IllegalStateException("broken"))),
      Field.fetch("brokenMaybe", Nullable(Scalar.string))((_, _) => defer(throw IllegalStateException("maybe"))),
    ))
    lazy val orderT: ObjectType[Order] = ObjectType[Order]("Order")(List(
      Field("id", Scalar.int)(_.id),
      Field("total", Scalar.float)(_.total),
      Field.fetch("user", userT)((o, _) => Fetch.get(s.user, o.user)),
    ))
    val first = Arg("first", InScalar.int, Some(3))
    val ids = Arg("ids", InList(InScalar.id))
    val only = Arg("tier", Optional(tierIn))
    val query = ObjectType[Unit]("Query")(List(
      Field.fetch("user", Nullable(userT), Arg("id", InScalar.id))((_, a) => pure(users.get(a(Arg("id", InScalar.id))))),
      Field.fetch("users", ListOf(userT), first, only)((_, a) =>
        pure((1 to a(first)).map(i => users(s"u$i")).filter(u => a(only).forall(_ == u.tier)).toList)),
      Field.fetch("byIds", ListOf(Fetched(Nullable(userT))), ids)((_, a) =>
        pure(a(ids).map(id => Fetch.get(s.user, id).map(Some(_))))),
      Field.fetch("allByIds", ListOf(userT), ids)((_, a) => Fetch.traverse(a(ids))(Fetch.get(s.user, _))),
      Field.fetch("pair", ListOf(Scalar.string), Arg("a", InScalar.id), Arg("b", InScalar.id))((_, a) =>
        Fetch.both(Fetch.get(s.user, a(Arg("a", InScalar.id))), Fetch.get(s.user, a(Arg("b", InScalar.id))))
          .map((x, y) => List(x.name, y.name))),
      Field.fetch("chain", Scalar.string, Arg("id", InScalar.id))((_, a) =>
        Fetch.get(s.user, a(Arg("id", InScalar.id))).flatMap(u => Fetch.get(s.ordersOf, u.id)).map(_.size.toString)),
      Field.fetch("many", ListOf(Scalar.int), Arg("n", InScalar.int))((_, a) => pure((1 to a(Arg("n", InScalar.int))).toList)),
    ))
    val mutation = ObjectType[Unit]("Mutation")(List(
      Field.fetch("say", Scalar.string, Arg("word", InScalar.string))((_, a) =>
        defer { s.log += a(Arg("word", InScalar.string)); pure(s.log.mkString(",")) }),
    ))
    Schema(query, Some(mutation))

class TestGraphQL extends okay.testkit.Munit.Diagnosed:
  import Shop.*

  private def run(q: String, vars: Json = JObj(Vector.empty))(using s: Sources): Json =
    val out = Executor.execute(schema(s), q, vars)
    note(s"$q => ${Json.print(out)}")
    out

  private def json(s: String): Json = Json.parse(s)

  /** `data.<field>`, a list */
  private def list(out: Json, field: String): Vector[Json] = out match
    case JObj(Vector(("data", JObj(fs)))) => fs.collectFirst { case (`field`, JArr(xs)) => xs }.getOrElse(fail(s"no list $field"))
    case other => fail(s"not data: ${Json.print(other)}")

  test("scalars, nested objects, lists, nullables") {
    given s: Sources = Sources()
    assertEquals(run("""{ users(first: 2) { id name email tier } }"""), json(
      """{"data":{"users":[{"id":"u1","name":"user 1","email":null,"tier":"FREE"},
        |{"id":"u2","name":"user 2","email":"u2@shop","tier":"FREE"}]}}""".stripMargin))
  }

  test("arguments, variables with defaults, aliases") {
    given s: Sources = Sources()
    val q = """query Q($n: Int = 2, $t: Tier) { few: users(first: $n, tier: $t) { id } all: users(first: 10, tier: PRO) { id } }"""
    assertEquals(run(q), json("""{"data":{"few":[{"id":"u1"},{"id":"u2"}],"all":[{"id":"u10"}]}}"""))
    assertEquals(run(q, json("""{"n": 1}""")), json("""{"data":{"few":[{"id":"u1"}],"all":[{"id":"u10"}]}}"""))
  }

  test("named and inline fragments, @skip / @include, __typename") {
    given s: Sources = Sources()
    val q = """query($x: Boolean!) { users(first: 1) { ...F ... on User { name } email @include(if: $x) id @skip(if: true) } }
              |fragment F on User { __typename id }""".stripMargin
    assertEquals(run(q, json("""{"x": false}""")), json("""{"data":{"users":[{"__typename":"User","id":"u1","name":"user 1"}]}}"""))
    assertEquals(run(q, json("""{"x": true}""")),
      json("""{"data":{"users":[{"__typename":"User","id":"u1","name":"user 1","email":null}]}}"""))
  }

  test("batching: the orders of 100 users and their users are one call per source per round") {
    given s: Sources = Sources()
    val out = run("""{ users(first: 100) { orders { id user { name } } } }""")
    assertEquals(s.ordersOf.calls.size, 1)
    assertEquals(s.ordersOf.calls.head.size, 100)
    assertEquals(s.user.calls.size, 1)
    onFailure(s"user calls: ${s.user.calls}")
    assertEquals(s.user.calls.head.toSet.size, s.user.calls.head.size) // each key once
    val us = list(out, "users")
    assertEquals(us.size, 100)
    assertEquals(us(4), json("""{"orders":[{"id":50,"user":{"name":"user 5"}},{"id":51,"user":{"name":"user 5"}}]}"""))
  }

  test("duplicate keys are fetched once") {
    given s: Sources = Sources()
    assertEquals(run("""{ byIds(ids: ["u1", "u2", "u1"]) { id } }"""),
      json("""{"data":{"byIds":[{"id":"u1"},{"id":"u2"},{"id":"u1"}]}}"""))
    assertEquals(s.user.calls.toList, List(Seq("u1", "u2")))
  }

  test("Fetch.both / Fetch.all: independent fetches inside a resolver in one round; a sequence is rounds") {
    given s: Sources = Sources()
    assertEquals(run("""{ pair(a: "u3", b: "u4") }"""), json("""{"data":{"pair":["user 3","user 4"]}}"""))
    assertEquals(s.user.calls.toList, List(Seq("u3", "u4")))
    assertEquals(run("""{ a: chain(id: "u5") b: chain(id: "u7") }"""), json("""{"data":{"a":"2","b":"1"}}"""))
    assertEquals(s.user.calls.size, 2)
    assertEquals(s.ordersOf.calls.toList, List(Seq("u5", "u7")))
  }

  test("errors with paths; null propagation through non-null fields") {
    given s: Sources = Sources()
    assertEquals(run("""{ users(first: 1) { id brokenMaybe } }"""), json(
      """{"data":{"users":[{"id":"u1","brokenMaybe":null}]},
        |"errors":[{"message":"maybe","path":["users",0,"brokenMaybe"]}]}""".stripMargin))
    // users is non-null all the way up: data is null
    assertEquals(run("""{ users(first: 1) { broken } }"""), json(
      """{"data":null,"errors":[{"message":"broken","path":["users",0,"broken"]}]}"""))
    // items fetched each as its own program: the missing one is null, its error at its index; one call for all
    assertEquals(run("""{ byIds(ids: ["u1", "nobody", "u2"]) { id } }"""), json(
      """{"data":{"byIds":[{"id":"u1"},null,{"id":"u2"}]},
        |"errors":[{"message":"users has no value for nobody","path":["byIds",1]}]}""".stripMargin))
    assertEquals(s.user.calls.toList, List(Seq("u1", "nobody", "u2")))
    // one program traversing them all: its error is the field's — non-null all the way up, data is null
    assertEquals(run("""{ allByIds(ids: ["u1", "nobody"]) { id } }"""), json(
      """{"data":null,"errors":[{"message":"users has no value for nobody","path":["allByIds"]}]}"""))
    // a nullable field takes it
    assertEquals(run("""{ user(id: "u1") { broken } }"""), json(
      """{"data":{"user":null},"errors":[{"message":"broken","path":["user","broken"]}]}"""))
  }

  test("an argument of the wrong type is a field error") {
    given s: Sources = Sources()
    assertEquals(run("""{ users(first: "x") { id } }"""), json(
      """{"data":null,"errors":[{"message":"argument first: Int cannot represent \"x\"","path":["users"]}]}"""))
  }

  test("mutations: top-level fields in order") {
    given s: Sources = Sources()
    assertEquals(run("""mutation { a: say(word: "one") b: say(word: "two") c: say(word: "three") }"""),
      json("""{"data":{"a":"one","b":"one,two","c":"one,two,three"}}"""))
  }

  test("request errors: a parse error, an unknown field, an unknown argument") {
    given s: Sources = Sources()
    assertEquals(run("""{ users { id """), json("""{"errors":[{"message":"Syntax error: expected a name, found '<end>' at 1:14"}]}"""))
    assertEquals(run("""{ users { nope } }"""), json("""{"errors":[{"message":"User has no field nope"}]}"""))
    assertEquals(run("""{ users(last: 1) { id } }"""), json("""{"errors":[{"message":"Query.users has no argument last"}]}"""))
    assertEquals(run("""{ users }"""), json("""{"errors":[{"message":"Query.users of type User needs a selection"}]}"""))
    assertEquals(run("""query($n: Int!) { users(first: $n) { id } }"""),
      json("""{"errors":[{"message":"variable $n of a non-null type is required"}]}"""))
  }

  test("the depth bound: a deep document and a cycle of fragments are errors, not overflows") {
    given s: Sources = Sources()
    val deep = "{ users { orders { user " * 40 + "{ id }" + " } }" * 40 + " }"
    val out = run(deep)
    assert(Json.print(out).contains("nesting deeper than 64"), Json.print(out))
    val cycle = run("""{ users { ...A } } fragment A on User { ...B } fragment B on User { ...A }""")
    assert(Json.print(cycle).contains("fragments nested deeper than 64"), Json.print(cycle))
  }

  test("a list of 100 000 items in constant stack") {
    given s: Sources = Sources()
    val xs = list(run("""{ many(n: 100000) }"""), "many")
    assertEquals(xs.size, 100000)
    assertEquals(xs.last, JNum(100000))
  }

  test("100 000 independent fetches in one round, in constant stack") {
    val src = Counted[Int, Long]("squares", k => Some(k.toLong * k))
    val q = ObjectType[Unit]("Query")(List(
      Field.fetch("sum", Scalar.float)((_, _) => Fetch.traverse((1 to 100000).toList)(Fetch.get(src, _)).map(_.map(_.toDouble).sum))))
    assertEquals(Executor.execute(Schema(q), "{ sum }"), JObj(Vector("data" -> JObj(Vector("sum" -> JNum((1 to 100000).map(k => k.toDouble * k).sum))))))
    assertEquals(src.calls.size, 1)
  }

  test("schema.sdl") {
    val sdl = schema(Sources()).sdl
    note(sdl)
    assert(sdl.contains("type Query {\n  user(id: ID!): User\n  users(first: Int! = 3, tier: Tier): [User!]!"), sdl)
    assert(sdl.contains("type User {\n  id: ID!\n  name: String!\n  email: String\n  tier: Tier!\n  orders: [Order!]!"), sdl)
    assert(sdl.contains("enum Tier {\n  FREE\n  PRO\n}"), sdl)
    assert(sdl.contains("type Mutation {\n  say(word: String!): String!\n}"), sdl)
    assert(sdl.contains("byIds(ids: [ID!]!): [User]!\n  allByIds(ids: [ID!]!): [User!]!"), sdl)
  }
