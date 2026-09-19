## http-streaming-responses - incremental bodies on the JDK and Netty backends

Only okay-jetty wrote a `text/event-stream` response chunk by chunk;
the JDK backend (`okay.http.Server`, `com.sun.net.httpserver`) and
`okay-netty` both drained the whole body before sending a status line,
which is right for REST and fatal for a server push that has to
outlive the write — MCP's GET stream (specs/mcp.md v6) worked on
Jetty and nowhere else.

`Http.streams` — the one-line answer, "does this response's
content-type say STREAM" — moved out of okay-jetty into `okay-http`
so all three backends share it rather than each naming
`text/event-stream` itself. The JDK backend asks for chunked transfer
encoding (`sendResponseHeaders(status, 0)`) and writes with a flush
per chunk, already on the virtual thread its executor gives every
request. `okay-netty` writes a `DefaultHttpResponse` plus
`DefaultHttpContent` per chunk instead of one `DefaultFullHttpResponse`
— and does it off the event loop, on its own virtual thread, because
a subscription push can hold the body open indefinitely and the event
loop serves every other connection in its group meanwhile.

`TestMcpPushServer` (okay-http) and `TestMcpPush` (okay-netty) port
okay-jetty's own `TestMcpPush` onto the two backends, unchanged in
shape: a subscription's push arrives on the GET stream, and a second
push is not buffered behind a body that never ends.
