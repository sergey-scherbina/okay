## foreign-server — a host that ACCEPTS far sides (specs/foreign-server.md)

The okay wire's host could only dial: a child's pipes, a worker's TCP port,
FFM, wasm. Now it can also accept. `ForeignServer.listen(port)` returns the
next far side to connect as an engine with `accept()`. `serve(session)` gives
each connection a thread of its own and closes its engine after the session.
The engine is `ForeignWorker.over(WireLink.accepted(socket))`, so callbacks,
multi-shot, direct style and `Durable` run unchanged. `WireConformance`
passes over an accepted connection, with the TypeScript worker dialling
through a relay (`TestTsServed`), and so does okay-ts, which connects by
itself (`TestOkayTsServed`, LIVE, `OKAY_TS`). A TypeScript program's
transfers are audited by the server (`TestOkayTsAudited`): the trail,
normalization, and a refusal caught in TypeScript where the transfer is.
Plain TCP, like `WireLink.tcp`; WebSocket for browsers is a later module.
