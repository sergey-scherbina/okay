## scala2-http - okay-http from Scala 2.13: routes as pattern matching, a server, a client

The second of the five areas (HTTP, SQL, codecs, agents, UI).

- Probed from scalac 2.13.18: okay-http's `Request`, `Method`, `Body`,
  `Router`, `Http`, `Server` and `Transports` are readable, so a
  Scala 2 caller builds requests with okay-http directly. `Response`
  is not readable (its streamed body names the union row in its
  constructor), and neither is `Route` ("Unsupported Scala 3 generic
  tuple type").
- The new module okay-scala2-http provides, in package `okay.scala2`:
  - `Response`: text, html, bytes and JSON bodies, and a streamed one
    (`lines`);
  - routing as Scala 2 pattern matching, `Routes { case GET(Path("users", id)) => ... }`,
    with `Requests` for query, text and JSON;
  - `Server.use` (okay-http's `Server.serve` under `Resource.run`) and
    `Server.start`/`close`;
  - `Client` (`send`/`get`/`post`/`postJson`, and `lines` streamed).
- okay-http gains a public `okay.http.Urls` (`segments`, `params`),
  which delegates to `Route`'s own percent-decoding, so the Scala 2
  `Path` extractor does not duplicate it.
- A trap: the facade's internal value class was named `Body`. A
  Scala 2 file importing both `okay.http.Body` and `okay.scala2._`
  made scalac read that class while resolving the name, and refuse it
  (its constructor names the row). It is now `ProgBody`, and the rule
  is written down: internal classes of the facade must not take
  common names.
- Probe: `TestHttpFromScala2` (4 tests, no socket, in the gate) and
  `TestHttpLiveFromScala2` (3 tests, real socket, Live-tagged: run and
  green here).
- Docs: section 8b of docs/scala2.md (copied from the probe), a module
  page, API reference, typepedia, `Urls` on the okay-http page, and
  spec stage 7.
