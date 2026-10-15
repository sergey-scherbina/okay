## okay-graphql-next - the rest of GraphQL: abstract types, introspection, validation, async, subscriptions, HTTP

- Interfaces and unions by typed members (`Member(objectType)(pick)`), fragments on abstract types,
  `Schema.check`; descriptions and `@deprecated` on fields, in the SDL and introspection.
- Introspection (`__schema`, `__type`) on the same executor; GraphiQL's query answers (`Introspection.query`).
- Validation (§5) before running, every error with `locations`; variables coerced to their declared types;
  `Executor.prepare` / `Prepared` and `QueryCache`; `Limits.fields` refuses a fragment bomb unexpanded.
- `AsyncSource` and `Executor.executeAsync`: a round's sources called at once, in parallel.
- Subscriptions: `SubscriptionType`, `Executor.subscribe` (a `StreamCont.Src` of responses);
  `TransportWs.Connection`, graphql-transport-ws for any socket.
- `GraphQLJava`: graphql-java's parser behind `GraphQLParser`, optional, refused by name; `Document.print`.
- New module okay-graphql-http (JVM, JS): `GraphQLHttp.router` (POST, GET, both media types, 405, 400) and
  `GraphQLWs.route` (sessions on okay-http's server seam). okay-jetty: `Jetty.serve(..., subprotocol = ...)`.
- specs/okay-graphql-next.md, docs/modules/okay-graphql.md, docs/modules/okay-graphql-http.md.
