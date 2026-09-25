## remote-arrow-frames - okay-cluster's Remote sends chunks as Arrow; CBOR's and JSON's List decoding was quadratic

okay-arrow stage 7b (specs/okay-arrow.md). A survey of the codebase found
one transport that sends BATCHES of typed records, okay-cluster's
`Remote` (JSON lines until now); everything else sends one value at a time.

- `Remote` frames: a length, a format tag, a compression tag, the payload.
  The listener reads the tags, so it needs no configuration. There are
  two givens:
  - `RemoteFormat`: `arrow` (the default), `Cbor.given`, `Json.given`;
  - `RemoteCompression`: none, `Lz4.given`, `Zstd.given`.
- Measured on 200 000 records over a loopback socket: Arrow 50–59 ms and
  7 MB, against JSON 136 ms and 13.7 MB, and CBOR 182 ms and 9.8 MB.
  Arrow+ZSTD at 0.95–1.13 MB beats CBOR+ZSTD at 1.46–1.49 MB, and is
  faster.
- FIXED (okay-codec): CBOR's and JSON's `List` decoders appended with
  `:+`, so reading was quadratic. JSON took 13 s and CBOR 7 s for chunks
  of 10 000. `TestListDecodeLinear` was watched red at 463x, then green.
- FOUND: "localhost" can reach a stranger's socket over IPv6 on this box.
  The tests now use the loopback address; three more suites are filed as
  `localhost-connects-to-a-stranger`.
- `TestRemote` is now Live-tagged: it binds a port.
