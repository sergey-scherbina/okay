# okay-jetty

Jetty behind okay-http's two seams — and the one thing neither the JDK
nor okay-http can do (specs/http-backends.md).

`specs/http.md` put **serving WebSocket** out of scope rather than
half-build it: the JDK has no server-side WebSocket API at all, and
`com.sun.net.httpserver` will not surrender its socket. That gap is why
this module exists, and it is closed with the session type that already
existed — a server session is the same `Stage[Frame, Frame, Unit]` a
client writes, so the echo in `TestJetty` runs on both ends.

## The pieces

| | |
|---|---|
| `Jetty.http(): Http ! Resource` | the REST client, body streamed through `InputStreamResponseListener` |
| `Jetty.sockets(): Sockets ! Resource` | the WebSocket client |
| `Jetty.serve(port)(routes)(ws)` | REST routes and WebSocket sessions on one port |
| `Jetty.of(client)` | a caller's own configured `HttpClient`, behind the seam |

Routes are a `PartialFunction[Request, Response ! Async]` and WebSocket
routes a `PartialFunction[Request, Stage[Frame, Frame, Unit]]` — the
smallest thing that dispatches without inventing a router.

Everything is a `Resource`, because everything owns threads. A module
that hands back a bare client hands back a leak with instructions.

## One interop wrinkle, worth knowing before it bites again

`Listen.java` is ten lines of Java in a Scala module, and its own
comment says why. Jetty decides which callbacks a listener wants by
**reflecting over the methods its class declares**, and refuses one
declaring both `onWebSocketText(String)` and
`onWebSocketPartialText(String, boolean)`. Scala 3 emits mixin
forwarders for every default method of an implemented Java interface,
so a Scala listener declares them all and Jetty rejects it:
`Cannot replace previously assigned [TEXT Handler]`.

The Java class declares exactly the four callbacks wanted and delegates
to a `Sink` interface — all abstract, nothing to forward — which the
Scala side implements. Expect the same with any API that reflects over
declared methods.
