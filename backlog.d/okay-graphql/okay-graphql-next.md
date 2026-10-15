- [ ] okay-graphql-next — what specs/okay-graphql.md left out of its first
      lane, each its own step on the same executor:
      introspection (`__schema`, `__type`: a schema of the schema, the
      tools' need — GraphiQL, codegen); interfaces and unions (an inline
      fragment's condition on an abstract type: `collect` takes the
      object's possible types); subscriptions (a field resolving to an
      okay-stream source, each event executed as a query); an HTTP route
      over okay-http (`POST /graphql` with `{query, variables,
      operationName}`, GET for queries); validation beyond unknown fields,
      arguments and missing selections (variable types against argument
      types, fragment spreads possible, the spec's §5 rules); an adapter
      over graphql-java behind `GraphQLParser` (OURS OR THE STANDARD ONE,
      optional dependency, each reading the other's documents); sources
      answering with a program (async data: `Source.fetch` a plain
      function today, a round blocks on it); per-item errors in a list
      fetched by `Fetch.traverse` (today the first missing key fails the
      whole field).
