# okay-netty

Netty behind okay-http's two seams (specs/http-backends.md).

This is interop in the sense `okay-fs2` and `okay-zio` are: not a better
implementation than the JDK's, an implementation in a runtime someone is
already running. It is also the honest answer to "NIO with HTTP" —
`okay.http.Nio` is the byte level because hand-rolling HTTP/1.1 is work
without a payoff, and Netty is where that codec is already written,
which is what makes it worth a dependency.

## The pieces

| | |
|---|---|
| `Netty.http(): Http ! Resource` | the REST client — pipeline with **no aggregator**, so the body is a `Source` that arrives chunk by chunk |
| `Netty.sockets(): Sockets ! Resource` | the WebSocket client |
| `Netty.serve(port)(routes)(ws)` | REST and WebSocket on one port |
| `Netty.of(group)` | a caller's own event loops, behind the seam |

## The test that matters

`TestBackends` is the reason the seam exists. It writes **one**
`fetchPerson(http, url)` and **one** `sayOnce(sockets, url)` and runs
them across the matrix: three REST clients, three REST servers, and
every WebSocket client against every WebSocket server — six pairs. One
suite run many times rather than three similar suites, which is the
only form of "the program does not know what is underneath" worth
claiming.

It lives here rather than in okay-http because this is the module that
can see all three: okay-jetty is a test-scope dependency.

## Found by a test

The request conversion built its `Request` from the method, uri and
headers and forgot the content, so every route saw an empty body. The
one test that POSTs caught it. A conversion between two request types
wants a test per **field**, not per happy path.
