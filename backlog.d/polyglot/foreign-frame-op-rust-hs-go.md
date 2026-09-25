- [ ] foreign-frame-op-rust-hs-go — the `frame` op in the Rust, Haskell
      and Go shims (foreign-map-reduce, 2026-09-25). A cluster stage sends
      a chunk as ONE frame (`ForeignEval.Frame`), and only Python's, R's
      and TypeScript's shims serve that op today: `grep -l '"frame"'` over
      the shims finds exactly those three, so `mapPy`/`mapR` exist and a
      `mapRs`/`mapHs`/`mapGo` cannot yet. Each shim needs: `frame` in its
      dispatch, a frame as a struct of columns (Rust: `Vec<Column>` with
      an enum per kind; Haskell: a record of lists; Go: a
      `map[string][]any`), the columnar wire shape of r-frame-columnar-wire
      both ways, and — where the language has an Arrow library (Rust's
      `arrow` crate, Go's `arrow/go`) — the Arrow road with the header in
      the schema metadata, announced as `frames: ["arrow"]` in the hello.
      Then `TsStage`/`RsStage`/… are each a name in okay-foreign-cluster.
      Gate: `TestForeignStage`'s job with the map in each language.
