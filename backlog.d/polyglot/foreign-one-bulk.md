- [ ] foreign-one-bulk — stage 3 of specs/foreign-one.md: bulk data on
      every side, zero copy in process. SUBSUMES foreign-frame-op-rust-hs-go
      (the `frame` op in the Rust, Haskell and Go shims — that item keeps
      the per-language shape) and adds the in-process road: over FFM a
      frame crosses as the Arrow C Data Interface (`ArrowSchema` +
      `ArrowArray`, `okay.arrow.Table` ↔ Rust `arrow::ffi`), the head as a
      JSON line beside it, instead of IPC bytes copied through
      `okay_exchange`; wasm keeps IPC bytes in linear memory (no shared
      buffers). After foreign-one-mux tier 3 is a stream of frames with
      credit, not a loop of tier-2 exchanges. Gate: `Frames[RustModule]`/
      `[HsModule]`/`[GoModule]` pass `FacadeConformance.frames`; a 1M-row
      Table through a Rust `frame` over FFM with no buffer copy, held by
      the JVM Arrow allocator's byte count, not by time; every claimed
      cell of the spec's table measured (MeasureFacade, load and sha).
