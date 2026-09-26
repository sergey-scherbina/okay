- [ ] foreign-arrow-ffm — a table over FFM as the Arrow C Data Interface
      (`ArrowSchema` + `ArrowArray`, `okay.arrow.Table` ↔ Rust
      `arrow::ffi`), the head a JSON line beside it, instead of the
      columnar JSON copied through `okay_exchange` (specs/foreign-one.md
      stage 6 as first written; narrowed by Decision 18). Blocked twice on
      2026-09-26: the `arrow` crate is not in the offline cargo registry
      (nor Go's arrow module in GOMODCACHE), and no caller moves tables
      big enough for the copy to show. Trigger: a measurement where a
      table call into in-process Rust is dominated by its JSON codec.
      Gate: a 1M-row Table through a Rust function over FFM with no buffer
      copy, held by the JVM Arrow allocator's byte count, not by time;
      wasm keeps bytes in linear memory.
