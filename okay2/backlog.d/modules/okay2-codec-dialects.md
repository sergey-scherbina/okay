- [ ] okay2-codec-dialects: the rest of okay-codec on okay2, once a
      caller needs it. Landed so far: `Schema` and JSON (stage 41), XML
      (stage 43), `JsonSchema` (with okay2-http-routes, stage 44B), and
      `Columns` (okay2-spark-columns). Still unported: `Cbor` (RFC 8949,
      and the BigInt preferred serialization TestBigInt pins),
      `Validate`, `Digest`/`Compat`, `Edn`, `Yaml`, `Markdown`, the
      staged codecs, and the `Codecs` provider registry. (2026-09-25)
