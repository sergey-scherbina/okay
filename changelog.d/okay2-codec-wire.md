## okay2-codec-wire - the wire's format, compression and handshake in okay2-codec (JVM)

The third lane of okay2/backlog.d/modules/okay2-codec-tooling (spec stage
59, docs/okay2.md section 33). `Wire` is ported to okay2-codec's JVM
sources: `WireFormat` (JSON, CBOR), `WireCompression` (a preference
order, or deflate, zlib or none as a requirement), `FrameFormat`,
`WireChoice`, `WireAuth` (mutual HMAC-SHA256), `WireSecurity` (TLS trust),
`WireDeadline`, `WireJson.whole`, `WireFrames`, `WireNegotiation` and
`WireCbor`. Each choice is an implicit in its companion, and importing
an explicit one wins. The readers keep their explicit stacks.

New suites: TestWire, which covers the codec half of okay-py's
TestWireGivens and drives `WireNegotiation` directly, and TestWireDepth
on a 256 KB stack, 28 tests in all.

`Staging` was not ported, and the backlog item now says why. The seam
finds a module that compiles codecs at run time with Scala 3's
`scala.quoted.staging`. Scala 2 has nothing like it, and an okay2
twin of that module would be a new design, not a port.
