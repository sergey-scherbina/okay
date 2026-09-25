- [ ] foreign-one-bulk — stage 6 of specs/foreign-one.md: every thing
      on every side, zero copy in process. SUBSUMES
      foreign-frame-op-rust-hs-go (the table road in the Rust, Haskell and
      Go libraries — that item keeps the per-language shape) and adds
      `Objects[L]` for them (a small table of held values in each
      library) and the in-process road: over FFM a table crosses as the
      Arrow C Data Interface (`ArrowSchema` + `ArrowArray`,
      `okay.arrow.Table` ↔ Rust `arrow::ffi`), the head as a JSON line
      beside it, instead of IPC bytes copied through `okay_exchange`; wasm
      keeps IPC bytes in linear memory (no shared buffers). After
      foreign-one-mux a stream of tables is chunks under credit, not a
      loop of exchanges. Gate: FacadeConformance's table body over Rust,
      Hs, Go; a 1M-row Table through a Rust function over FFM with no
      buffer copy, held by the JVM Arrow allocator's byte count, not by
      time; every claimed cell of the spec's table measured
      (MeasureFacade, load and sha).
