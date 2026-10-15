## okay-graphql - GraphQL on the kernel: resolvers as programs of Fetch, an executor in rounds

- New module `okay-graphql` (JVM, JS, Native; okay-codec's Json, nothing else):
  a document parsed by our own parser behind `GraphQLParser` (queries,
  mutations, variables with defaults, aliases, arguments, named and inline
  fragments, `@skip`/`@include`, `__typename`); a schema of Scala values
  (`ObjectType`, scalars, `EnumType`, `ListOf`, `Nullable`, typed `Arg`s and
  input types, `schema.sdl`); resolvers as programs of `Fetch`
  (`Fetch.get(source, key)`, `Fetch.both`, `Fetch.all`, `Fetch.traverse`).
- `Executor.execute` runs a query in ROUNDS: every resolver stepped by the
  kernel to its next fetch, each source called once per round with all its
  keys — fields and list items the applicative product. 100 users, their
  orders and the orders' users: two source calls. Errors with paths, null
  propagation, mutations in order, `Limits.depth` on every nesting, a list of
  100 000 items in constant stack.
- specs/okay-graphql.md, docs/modules/okay-graphql.md, TestGraphQL (14 tests
  on each platform). Deferred: backlog `okay-graphql-next` (introspection,
  interfaces and unions, subscriptions, an HTTP route, full validation, a
  graphql-java adapter, async sources).
