- [ ] foreign-one-mux — stage 5 of specs/foreign-one.md: the wire
      multiplexed by `id`, streams in BOTH directions under credit — the
      one new capability of the model. Today one exchange is in flight
      per worker (`ForeignWorker.send` is sequential, the callback
      dialogue nests strictly), so a stream is host-driven only
      (`Streams.viaFrames`, `Speaks.stream` false everywhere) and a
      Go/Rust/Ts/Hs function cannot have two okay `perform`s open. A
      reader per link matches answers by `id`; `{"stream":s,"chunk":…}`/
      `{"stream":s,"end":true}` from either side and `{"stream":s,
      "credit":c}` from the receiver — a stream ARGUMENT is fed by the
      host under the far side's credit, a stream ANSWER by the far side
      under the host's, both at once is a full-duplex transform (what
      `Stateful`'s open/step/finish spelled from outside). A far side
      announces `"speaks":{"mux":true,"stream":true}`; one that does not
      is served byte for byte as today (R, Rust on wasip1 say `mux:
      false` by design). `Durable`/the pool journal by `(id, seq)`, not
      by position. In process: `okay_poll` beside `okay_exchange`. Go and
      Rust first, then Ts and Hs; Python if a threaded shim is measured
      to beat a pool under the GIL. Gate: two programs interleaved on
      one worker; two `perform`s outstanding under one Reader; a
      100 000-row far-driven stream whose far side never exceeds its
      credit (COUNTED on the far side), a host-driven one the other way,
      a dedup both at once; replay by id; the mux reader within noise of
      the sequential exchange on `WireCodecBench`. Decisions 4–5 of the
      spec are written; the Go reference lands against them.
