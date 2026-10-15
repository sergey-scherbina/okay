## okay-graphql-cache - one execution's cache of fetched keys

- The executor keeps every key a source answered for the rest of the execution, as DataLoader keeps it per
  request: a key asked in a later round is answered without calling its source, its error too. Between a
  mutation's top-level fields the cache is forgotten. A source opts out with `cached = false`
  (`Source(name, cached = false)`, `override def cached`).
- specs/okay-graphql-cache.md, TestGraphQLCache (5), docs/modules/okay-graphql.md. TestGraphQLHttp: an import the
  kernel's move made unused, gone.
