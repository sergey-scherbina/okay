# okay-foreign-ws — the okay wire over a WebSocket

`ForeignWs.route(path)(session)`: each far side that connects over a WebSocket
— okay-ts in a page, a program behind a proxy that passes HTTP only — is
handed to the session as a `ForeignWorker` and driven as every far side is.
It is served by `Jetty.serve(port)(routes)(route.sessions, route.pushes)`.

**Depends on:** `okay-py`, `okay-jetty`

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-foreign-ws.md`](../docs/modules/okay-foreign-ws.md) | the pieces, and why this is a module |
| [`specs/foreign-ws.md`](../specs/foreign-ws.md) | the design and its decisions |
| [`okay-py/`](../okay-py) | the engine and the wire |
