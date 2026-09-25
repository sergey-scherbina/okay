- [ ] foreign-one-mux — stage 2 of specs/foreign-one.md: the wire
      multiplexed by `id`, with credit-based far-driven streams. Today
      one exchange is in flight per worker (`ForeignWorker.send` is
      sequential, the callback dialogue nests strictly), so a stream is
      host-driven only (`Streams.viaFrames`, `Speaks.stream` false for
      every language) and a Go/Rust/TS/Haskell function cannot have two
      okay asks open. A reader per link matches answers by `id`; two new
      messages, `{"stream":s,"chunk":…}`/`{"stream":s,"end":true}` from
      the far side and `{"stream":s,"credit":c}` from the host; a far
      side announces `"speaks":{"mux":true,"stream":true}` and one that
      does not is served byte for byte as today (R, Rust on wasip1 say
      `mux: false` by design). `Durable`/`SupervisedWorker` journal by
      `(id|stream, seq)`, not by position. In process: `okay_poll` beside
      `okay_exchange`. Go and Rust first. Gate: two programs interleaved
      on one worker; two asks outstanding under one Reader; a 100 000-row
      far-driven stream whose far side never exceeds its credit (counted
      on the far side); replay by id; the mux reader within noise of the
      sequential one on `WireCodecBench`. The one protocol change of the
      spec: its Decisions are written before the Go reference lands.
