# okay-scala2-ws

WebSockets for **Scala 2.13**. okay-http's `Frame` is used directly
(match on `Frame.Text(t)` and friends). The socket's operations and
server sessions are programs, and this module provides them:

| | |
|---|---|
| `WebSocket.connect(url)` / `WsClient` | a client over okay-http's JDK transport: `send`, `sendText`, `frames`, `texts`, `close` |
| `WsSession.fold(init)(step)` / `echo` / `replay` | a server session as a fold over incoming frames; `replay` runs one without a socket |
| `WsServer.use(port)(routes)(sessions)(body)` | ordinary routes and WebSocket upgrades on one port, over okay-jetty |
| `WebSocket.binary` / `bytes` | binary frames to and from `Array[Byte]` |

The walkthrough is section 8g of
[okay from Scala 2.13](../scala2.md#8g-websockets), and the signatures
are in [okay-scala2](okay-scala2.md#api-reference).
