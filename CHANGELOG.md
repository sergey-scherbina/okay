# Changelog

Completed work, newest first. One entry per landed task.

## http-js-acceptance — okay-http's JS transports, proven
Completed: 2026-09-01

They compiled and had never run, which is the failure `js.Dynamic` is
worst at: a mistyped field is `undefined`, not an error, so a transport
can be entirely broken and entirely green. `Acceptance.check` is one
shared-source program — schema, routes, session and expectations — and
both ends run it: the JVM against its own transports as a control, a
linked Scala.js client over `fetch` and the global `WebSocket` as the
acceptance. Verified able to fail. Also `Response.release` and
`Http.discard`, so a body can be let go unread rather than drained.

## http-backends — NIO, Jetty, Netty
Completed: 2026-09-01

Three backends behind one seam, each for a different reason: NIO for
having no dependency and being the byte level, Jetty for serving
WebSocket (which the JDK cannot), Netty for being a runtime someone
already runs. `TestBackends` runs one program across the matrix — three
REST clients, three REST servers, six WebSocket pairs.

## http — REST and WebSocket, as programs
Completed: 2026-09-01

The transport module three specs had deferred to. A response body is a
`Source`, a WebSocket session is a `Stage[Frame, Frame, A]`, and the
transport is a trait speaking `Async` rather than an effect. A socket
IS an `mcp.Link`, so okay-mcp gained a second transport for free.
