- [ ] foreign-value-rename — `PyValue`, `PyFrame`, `PyRef`, `PyCodec`,
      `PyModule` are the ONE value model of `okay.foreign` since
      foreign-one (Python, R, Go, Rust, Haskell, TypeScript all speak
      it), and the operator asked (2026-09-27, during pyvalue-table):
      "PyValue — общий enum на весь okay.foreign — а почему он Py если
      общий на весь foreign?" The answer was history: okay-py built the
      wire first, foreign-one-runtime made it shared and kept the names
      (specs/foreign-one.md Decision 26 kept the module's name "for now").
      Rename to `okay.foreign.Value` / `Frame` / `Ref` / `Codec` (the
      module type stays per language: `PyModule`, `RModule`,
      `WorkerModule`), keeping `PyValue`/`PyFrame`/`PyRef` as deprecated
      type aliases plus a `PyValue` companion forwarder so every caller
      and every doc example still compiles, the way `okay.RowLift` →
      `okay.Row` was done (okay-row-rename). Sweep the docs and specs
      to the new names in the same lane; the aliases go a release later.
      Cost: a rename touches every module over okay.foreign — an
      additive lane (aliases) gates by `affected master Test/compile`
      plus its own suites.
