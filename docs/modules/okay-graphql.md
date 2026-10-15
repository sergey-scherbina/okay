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

## Errors, nulls, mutations, bounds

A resolver that fails (a raise, a throw, a key its source left out, an argument that does not decode) is an error
at its path, `["users", 0, "broken"]`, and its field null; a null where the type is non-null goes up to the nearest
nullable parent, up to `data: null` (the spec's null propagation). A document that does not parse, names a field or
an argument the schema lacks, or misses a required variable is a request error: `errors` only.

A mutation's top-level fields run in order, each to its end before the next starts.

Nesting — selections, values, types, fragments inside fragments (a cycle is that) — is bounded by `Limits.depth`,
64 by default: past it, an error, not a stack overflow. The executor is a loop over a run queue: a list of 100 000
items and 100 000 fetches in one round run in constant stack.

Supported: queries and mutations, variables with defaults, aliases, arguments, named and inline fragments,
`@skip`/`@include`, `__typename`. Not yet (backlog `okay-graphql-next`): introspection, interfaces and unions,
subscriptions, an HTTP route, validation beyond fields and arguments, an adapter over graphql-java behind
`GraphQLParser`, sources answering with a program (async).

Tests: `TestGraphQL` (the shop: every item of the spec's behavior list).
