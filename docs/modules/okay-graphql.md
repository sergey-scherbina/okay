# okay-graphql

GraphQL on the kernel (`okay.graphql`, specs/okay-graphql.md): a schema of Scala values, a resolver a program of
one effect, `Fetch`, and an executor that batches every resolver's data by stepping the programs.

A GraphQL server's hard problem is the fan-out: `users { orders { user } }` asks field by field for data a naive
server fetches one row at a time (N+1). Here a resolver is a program, and the kernel runs a program to its next
operation and gives the rest back as a value (`p.step`). The executor holds all the suspended resolvers at once,
sees all their fetches, calls each source ONCE with all its keys, and resumes them: Haxl's idea, resolvers written
as plain sequential code.

## A schema

Types are values: `ObjectType[A]` with its fields, scalars (`Scalar.int`, `float`, `string`, `boolean`, `id`),
`EnumType`, `ListOf`, `Nullable` (non-null is the default: a Scala value is there), `Fetched` (below). A field
reads its parent, or resolves it by a program of `Fetch`:

```scala
    lazy val userT: ObjectType[User] = ObjectType[User]("User")(List(
      Field("id", Scalar.id)(_.id),
      Field("name", Scalar.string)(_.name),
      Field("email", Nullable(Scalar.string))(_.email),
      Field.fetch("orders", ListOf(orderT))((u, _) => Fetch.get(s.ordersOf, u.id)),
```

A `Source[K, V]` answers many keys in one call (a SQL `IN`, a batch endpoint); a key it leaves out is an error of
the field that asked. Arguments are typed (`Arg`, `InScalar`, `InEnum`, `InList`, `Optional`, `InObject`), with
defaults, and read with `args(arg)`:

```scala
    val first = Arg("first", InScalar.int, Some(3))
      Field.fetch("users", ListOf(userT), first, only)((_, a) =>
        pure((1 to a(first)).map(i => users(s"u$i")).filter(u => a(only).forall(_ == u.tier)).toList)),
```

`schema.sdl` prints the schema in SDL.

## Executing

```scala
    val out = Executor.execute(schema(s), q, vars)
```

The response is JSON (okay-codec's `Json`): `data`, and `errors` with their paths. The transport is the caller's —
an HTTP `POST /graphql` with `{query, variables, operationName}` is a few lines over `execute`.

It runs in ROUNDS. Every runnable task — a resolver's program — runs to its value or its next `Fetch`; a value
completes its field, and an object's fields and a list's items become tasks of the same round; when nothing is
runnable, the round's fetches are grouped by source, each source called once with its keys (each key once), and
every waiting task resumes. Fields of an object and items of a list are independent — the applicative product —
so their fetches meet in a round. The 100 users of the shop in the tests, their orders and the orders' users are
two source calls, not one per field:

```scala
    val out = run("""{ users(first: 100) { orders { id user { name } } } }""")
    assertEquals(s.ordersOf.calls.size, 1)
    assertEquals(s.user.calls.size, 1)
```

Inside one resolver, `Fetch.both(a, b)`, `Fetch.all(ps)` and `Fetch.traverse(as)(f)` ask for the same: independent
programs, one round. A sequence (`flatMap`) is a dependency and stays one round after another, as it must:

```scala
        Fetch.both(Fetch.get(s.user, a(Arg("a", InScalar.id))), Fetch.get(s.user, a(Arg("b", InScalar.id))))
        Fetch.get(s.user, a(Arg("id", InScalar.id))).flatMap(u => Fetch.get(s.ordersOf, u.id)).map(_.size.toString)),
```

An error belongs to what was computed on its own. One program traversing many keys fails as one: a key left out
is the whole field's error. To give each item its own error, make each item its own program: `Fetched(of)` is a
value still to be fetched, run as a task where it stands — the items' fetches still meet in one round (one call),
and a missing one is null at its index, its error at `["byIds", 1]`:

```scala
      Field.fetch("byIds", ListOf(Fetched(Nullable(userT))), ids)((_, a) =>
        pure(a(ids).map(id => Fetch.get(s.user, id).map(Some(_))))),
```

## A cache per execution

A key fetched once is kept for the rest of the execution, as DataLoader keeps it per request: asked again in a later
round, it is answered without calling its source — its error too. Between a mutation's top-level fields the cache is
forgotten, so each field sees what the one before wrote. A source of data that changes while the request runs opts
out:

```scala
    val src = Source[String, Int](name, cached) { keys =>
```

## Errors, nulls, mutations, bounds

A resolver that fails (a raise, a throw, a key its source left out, an argument that does not decode) is an error
at its path, `["users", 0, "broken"]`, and its field null; a null where the type is non-null goes up to the nearest
nullable parent, up to `data: null` (the spec's null propagation). A document that does not parse, names a field or
an argument the schema lacks, or misses a required variable is a request error: `errors` only.

A mutation's top-level fields run in order, each to its end before the next starts.

Nesting — selections, values, types, fragments inside fragments (a cycle is that) — is bounded by `Limits.depth`,
64 by default: past it, an error, not a stack overflow. The executor is a loop over a run queue: a list of 100 000
items and 100 000 fetches in one round run in constant stack.

## Types from case classes

A case class already says its fields and their types; `ObjectType.derive` reads them — a field per parameter, of
the output type its Scala type has (`Option` nullable, `List`/`Seq`/`Vector` a list, another derived type or enum
nested), each read by its own selector, typed, no cast. The fields that fetch are added by hand, and replace a
derived one of the same name. Enums and input objects the same:

```scala
  given EnumType[Tier] = EnumType.derive[Tier]
  given userT: ObjectType[User] = ObjectType.derive[User](
    Field.fetch("orders", ListOf(orderT))((u, _) => Fetch.get(ordersOf, u.id)),
  given InObject[Filter] = InObject.derive[Filter]
```

## Errors with extensions

An error may carry `extensions` for its client. Request errors carry a `code` — `GRAPHQL_PARSE_FAILED`,
`GRAPHQL_VALIDATION_FAILED`, `BAD_USER_INPUT`, `OPERATION_RESOLUTION_FAILURE` — and a resolver fails with its own:

```scala
      FieldError.raise[Option[String]]("no such thing", "code" -> JStr("NOT_FOUND"), "id" -> JNum(7))),
```

## Interfaces and unions

An abstract type holds its MEMBERS: an object type and how a value is told to be of it, typed — no type test of a
class, no cast. Fragments on an interface or a union apply to their members; `__typename` is the member's:

```scala
  lazy val mediumT: InterfaceType[Medium] = InterfaceType[Medium]("Medium")(
    List(Field("id", Scalar.id)(_.ident), Field("title", Scalar.string)(_.name)),
    List(Member[Medium, Book](bookT) { case b: Book => b }, Member[Medium, Film](filmT) { case f: Film => f }))
```

`Schema.check` refuses a member missing a field of its interface. Fields take `describe` and `deprecate`; both reach
the SDL and introspection.

## Introspection, validation, prepared documents

`__schema` and `__type(name:)` answer on every schema — ordinary object types over the schema value, run by the same
executor; GraphiQL's query (`Introspection.query`) answers. Before running, a document is checked as the spec's §5
says — operations, fields, arguments and their literals, fragments (unused, unknown, cycles, impossible spreads),
variables (defined, used, of compatible types), directives, fields that cannot merge — every error with its
`locations`. Variables are coerced to their declared types.

`Executor.prepare` parses and checks once; the `Prepared` executes with any variables; a `QueryCache` keeps them by
text. `Limits.fields` bounds the fields an operation selects, fragments expanded, without expanding past the bound:

```scala
    val p = Executor.prepare(schema, """query($q: String!) { search(q: $q) { ... on Medium { id } } }""").toOption.get
    assertEquals(p.execute(json("""{"q": "Dune"}""")), json("""{"data":{"search":[{"id":"b1"}]}}"""))
```

## Async sources and subscriptions

An `AsyncSource` answers with a program of `Async`; `Executor.executeAsync` is then a program itself, and each
round calls ALL its sources at once, going on when the last answered. A subscription field resolves to a stream
(`StreamCont.Src`); `Executor.subscribe` gives the stream of responses, each value executed as the selection.
`TransportWs.Connection` is the `graphql-transport-ws` protocol's server side, for any socket (a frame in is
`receive`, a frame out `send`); okay-graphql-http puts it on okay-http's server seam.

## The parser, ours or graphql-java's

`GraphQLParser` is a facade: ours the default; `GraphQLJava` (JVM) reads with graphql-java, an optional dependency
refused by name when absent. Both read every document of the tests to the same value, positions included, and each
reads the other's output (`Document.print`):

```scala
    given GraphQLParser = GraphQLJava()
```

Tests: `TestGraphQL` (the shop), `TestGraphQLDerive`, `TestGraphQLCache`, `TestGraphQLNext` (abstract types, introspection, validation, prepared documents,
the cost bound, async sources, subscriptions, the printer), `TestTransportWs`, `TestGraphQLJava` (JVM). Over HTTP
and WebSocket: [okay-graphql-http](okay-graphql-http.md).
