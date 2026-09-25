# okay-http — REST and WebSocket, as programs

Small, because the library's vocabulary had already decided most of
it: a response body is a `Source[Chunk[Byte]]`, a WebSocket session is
a `Stage[Frame, Frame, A]` that awaits incoming frames and tells
outgoing ones, and the transport is a TRAIT speaking `Async` rather
than an effect — the house rule mints an effect for domain logic above
the wire, never for the wire itself.

A 4xx is a `Response`. Nothing here throws.

## The pieces

| | |
|---|---|
| `Request` / `Response` / `Method` / `Body` | the wire, as data |
| `trait Http` | the seam: `send(r: Request): Response ! Async` |
| `Http.bytes / text / lines / json / sse` | reading a body — `lines` streams it, `json` is total, `sse` is the same event reader the LLM client uses |
| `Server.serve` | a route is `Request => Response ! Async`; the server is a `Resource`, so the port closes when the program ends |
| `Frame` / `Socket` / `Ws.over` | the WebSocket half: run a `Stage[Frame, Frame, A]` over a socket |

## A server and a client

```scala
import okay.http.*

val route: Request => Response ! Async = {
  case r if Server.path(r) == "/hello" => Server.text(200, "hello")
  case _ => Server.notFound
}

val client = Transports.http()

Resource.run[String, Pure](Server.serve(0)(route).map { s =>
  Async.run[String, Pure](
    client.send(Request.get(s"http://127.0.0.1:${Server.port(s)}/hello"))
      .flatMap(Http.text)).runWith
}).runWith
```

`serve(0)` takes the port the OS gives and `Server.port` reads it
back, which is why a test never hard-codes one.

## Further

| | |
|---|---|
| [`docs/modules/okay-http.md`](../docs/modules/okay-http.md) | the pieces and the reasoning |
| [`specs/http.md`](../specs/http.md) | the design, the backends, and the decisions |
| [`okay-jetty/`](../okay-jetty), [`okay-netty/`](../okay-netty) | other backends behind the same seam |
| [`okay-mcp-http/`](../okay-mcp-http) | MCP over these wires: `McpHttp`, `WsLink`, `NioLink` — a module of its own so this one does not depend on okay-mcp |
