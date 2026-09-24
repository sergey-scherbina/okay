- [ ] wire-givens-r — R's half of stage 5a of specs/polyglot-one-wire.md.
      okay-r's `RSubprocess` is its own engine (a character reader with a
      deadline), not `ForeignWorker`, so the givens (`WireFormat`,
      `WireCompression`) need byte framing there, and shim.R needs a CBOR
      subset (writeBin/readBin, big-endian) plus raw DEFLATE (memCompress
      "gzip" is zlib-wrapped: strip the 2-byte header and the adler32
      trailer, and rebuild both to inflate). Testable through the local
      `r-base:4.4.1` image, the way okay-r's live suite already runs.
      Found by wire-format-givens (2026-09-24).
