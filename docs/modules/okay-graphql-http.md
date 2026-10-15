# okay-graphql-http

[okay-graphql](okay-graphql.md) served: GraphQL over HTTP and subscriptions over a WebSocket, on okay-http's seams
(JVM and JS, every backend okay-http has).

## HTTP

`GraphQLHttp.router(schema)` answers `POST /graphql` with an `application/json` body `{query, variables,
operationName}` and `GET /graphql?query=…&variables=…&operationName=…`. The response is JSON, `data` and `errors`:
`application/json` with 200 for anything it could execute or refuse, or `application/graphql-response+json` when the
client accepts it — then a document refused before running is a 400. A mutation over `GET` is a 405 (`Allow:
POST`); a body that is not JSON or names no query, a 400. It runs `executeAsync`, so async sources are called a round
at a time, in parallel. `GraphQLHttp.answer` is the same for a server of another shape.

```scala
  private val router = GraphQLHttp.router(schema)
```

## WebSocket

`GraphQLWs.route(schema, "/graphql")` gives a session per client — `TransportWs.Connection`, the
`graphql-transport-ws` protocol (graphql-ws's) — as okay-http's server seam wants it: the sessions, their pushes,
and the subprotocol to accept, which a graphql-ws client requires the server to name. On Jetty, both on one port:

```scala
    val http = GraphQLHttp.router(schema)
    val ws = GraphQLWs.route(schema, "/graphql")
      Jetty.serve(0)(http.routes)(ws.sessions, ws.pushes, subprotocol = ws.subprotocol).flatMap { server =>
```

(`Jetty.serve`'s `subprotocol` is the one a session accepts among those its client asked for; none by default.)

Tests: `TestGraphQLHttp` (POST, GET, a mutation over GET, bad bodies, the two media types), `TestGraphQLWs` (a
session, no socket), `TestGraphQLJetty` (Live: HTTP and a subscription on a real Jetty, Jetty's own clients).
