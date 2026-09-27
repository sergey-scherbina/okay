- [ ] pyvalue-table — a frame as a VALUE (operator, 2026-09-27: "Делай а"
      to specs/foreign-map-reduce.md "Stage 5 — PROPOSED", road a).
      `PyValue.Table(frame)` in okay.foreign: `Wire.enc/dec` write and read
      it where `t == "frame"` inside any value, `PyCodec` decodes rows from
      it, every `match` over `PyValue` names it; the Python shim tags a
      frame it answers inside a value (`okay.frame(cols)`, a pandas frame),
      Go/Rust/Haskell already do (`Value::Table`), R's `enc` already tags a
      nested data.frame. Then `StatefulValue[M]` in okay-foreign-cluster —
      `open(params) -> state`, `step(frame, state) -> {rows, state'}`,
      `finish(state) -> frame`, three calls, the state a value the JVM
      carries — for every language, the compiled workers included, with
      Decision 23 untouched. Docs, spec stage 5, tests JVM/Python/Rust.
