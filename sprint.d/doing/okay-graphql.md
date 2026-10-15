- [ ] okay-graphql — specs/okay-graphql.md: GraphQL on the kernel. A module of
      no dependency beyond okay-codec's Json: an executable document parsed
      (queries, mutations, variables, aliases, fragments, @skip/@include), a
      schema of Scala values (objects, lists, nullables, scalars, arguments),
      resolvers as programs of a Fetch effect, and an executor that runs them
      in ROUNDS: every field ready is stepped to its first fetch, the fetches
      of a round grouped by source and sent once (Haxl's applicative batching,
      fields and list items the product; Fetch.both/all inside a resolver),
      errors with paths and null propagation, mutations in order.
