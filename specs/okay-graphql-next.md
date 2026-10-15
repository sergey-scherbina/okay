# okay-graphql-next: the rest of GraphQL

What specs/okay-graphql.md left out, on the same executor (rounds of stepped programs, one call per source per
round). Each piece is the spec's (GraphQL, October 2021) unless a decision below says otherwise.

## Abstract types: interfaces and unions

- `InterfaceType[A](name)(fields)` declares fields (for validation, SDL, introspection); `UnionType[A](name)`
  declares nothing but its members. Both hold MEMBERS: `Member[A, B](tpe: ObjectType[B], pick: A => Option[B])` —
  how a value of the abstract type is told to be of an object type, typed (no type test, no cast). The first member
  that picks the value is its type; none is a field error.
- An object type names the interfaces it implements (`ObjectType(name, implements = List(node))`); `Schema.check`
  refuses an object missing a field its interface declares, or a member that does not implement the interface.
- A fragment (`... on X`, `fragment F on X`) applies to an object `O` when `X` is `O`, an interface `O` implements,
  or a union `O` is a member of. `__typename` is the concrete object's.

## Introspection

`__schema` and `__type(name:)` on the query root, `__typename` everywhere; the types `__Schema`, `__Type`, `__Field`,
`__InputValue`, `__EnumValue`, `__Directive`, `__TypeKind`, `__DirectiveLocation` — built from the schema value as
an ordinary `ObjectType`, so it runs on the same executor. Descriptions and `@deprecated` on fields and enum values
(`Field.describe`, `Field.deprecate`). GraphiQL's introspection query answers.

## Validation (§5)

Before execution, a document is checked whole and every error is said with its locations:
operations (unique names, a lone anonymous one), fields (on their type, leaf vs composite selections, overlapping
fields under one response key the same field with the same arguments), arguments (known, required ones given,
literals of their type, variables of a compatible type), fragments (unique, on a composite type that exists, used,
no cycles, spreads possible), variables (unique, of input types, defined when used, used when defined), directives
(known, in place). Variables are coerced to their declared types before execution.

## Errors with locations, prepared documents, cost

- Every error carries `locations` (`line`, `column`) where the document has them, and `path` where execution had one.
- `Executor.prepare(schema, query)` parses and validates once; the `Prepared` executes with any variables. A
  `QueryCache` keeps prepared documents by text (LRU, a capacity).
- `Limits.fields`: the number of field selections of an operation, fragments expanded — a fragment spread ten times
  in ten levels is 10^10 fields; past the limit the document is refused (validation), counted without expanding
  past it.

## Async sources

A `Source` may answer with a program of an async effect (`AsyncSource`); `Executor.executeAsync` is then a program of
that effect itself: each round calls ALL its sources at once, in parallel, and the next round starts when every one
answered. A plain `Source` in the same query is called as before.

## HTTP (module okay-graphql-http)

GraphQL over HTTP: `POST` with `application/json` `{query, variables, operationName}`, `GET` with the same as query
parameters (a mutation over GET refused with 405), a response `application/json` (`data`, `errors`). A body that is
not JSON or names no query is 400. On okay-http, so it runs on every backend it has.

## Subscriptions

A subscription root field resolves to a STREAM of values; each value is executed as the selection set, giving a
stream of responses. Over a WebSocket, the `graphql-transport-ws` protocol (`connection_init`/`connection_ack`,
`subscribe`, `next`, `error`, `complete`, `ping`/`pong`), its state machine pure and tested without a socket.

## graphql-java (OURS OR THE STANDARD ONE)

`GraphQLJava`, a `GraphQLParser` over graphql-java's parser, behind an import, its jar an OPTIONAL dependency,
refused by name when absent. Each reads the other's output: `Document.print` (ours) parsed by graphql-java equals
ours parsed by ours.

## Behavior

- [x] interfaces and unions: fragments on abstract types, `__typename`, a value no member picks
- [x] `Schema.check`: a missing interface field, a member not implementing
- [x] introspection: `__schema`, `__type`, GraphiQL's query; descriptions and deprecation; SDL with them
- [x] validation: each rule refuses its document, with locations
- [x] variables coerced to their declared types
- [x] errors with locations; `prepare` once, execute many; the cache
- [x] `Limits.fields`: a fragment bomb refused
- [x] async sources: one round's sources called in parallel
- [x] HTTP: POST, GET, a mutation over GET, a bad body
- [x] subscriptions: a stream of responses; graphql-transport-ws in memory
- [x] graphql-java: parses as ours does; refused by name when absent

## Decisions

- Abstract types by MEMBERS with a typed `pick`, not by a `resolveType` naming an object: a name gives back no
  value of the object's type without a cast.
- Async: a round's async sources each run by `AsyncCont.runAsync` and joined by one `await` — no `Scheduler`
  needed, every platform; their answers kept and delivered on the executor's thread after the last, so the run
  queue is never touched from another thread. Measured in TestGraphQLNext: two sources that answer only once both
  were called — sequential calls would never finish.
- Field merging (§5.3.2) by response key with the same name and arguments, recursively through merged selections;
  two fields of one key on different OBJECT types may differ. SameResponseShape's type comparison is not checked.
- okay-http has no Native build: the HTTP and WebSocket half is its own module (okay-graphql-http, JVM and JS);
  `TransportWs.Connection` lives in okay-graphql, socket-free, every platform.
- okay-jetty's server took no subprotocol: a graphql-ws client refuses a server that names none. `Jetty.serve` gains
  `subprotocol: Request => Option[String]`, default none — the old behavior; its suites (19, Live) green.
- graphql-java 22.3 (the one in the local cache, JDK 11+); both parsers give the same `Document` for every test
  document, positions included.

## Results

- okay-graphql: 33 tests on JVM (TestGraphQL 14, TestGraphQLNext 15, TestTransportWs 4) + TestGraphQLJava 5; 33
  on JS and Native. okay-graphql-http: 7 on JVM and JS, TestGraphQLJetty 1 (Live).
