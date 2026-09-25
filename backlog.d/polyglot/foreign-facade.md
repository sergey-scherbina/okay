- [ ] foreign-facade — ONE FACADE OVER EVERY FOREIGN LANGUAGE, AND ONE
      DATA MODEL THAT SHOWS HOW (operator, 2026-09-25). The ask, in the
      operator's words: our code must work with all of them the same way
      — R, Python, Haskell, Rust, Clojure, Frege, Go, TypeScript, and the
      list will grow — and at the same time squeeze the most out of each:
      data must cross in bulk and fast, without trouble. A single data
      model that does not LIMIT but shows how to do it right, so that any
      further language can be plugged in the same way.
      WHAT EXISTS, which the facade must be built from and not beside:
      - the WIRE: programs as data on one line protocol (`perform`,
        `continue`, `done`; specs/remote-foreign.md), served by Python, R,
        TypeScript, Haskell, Go and Rust over pipes, TCP, FFM and Wasm
        (specs/polyglot-one-wire.md, its conformance suite: one test body
        over every language and transport). Clojure and Frege are IN the
        JVM instead (okay-clojure; okay-frege's `Prog` walked by okay,
        memory okay-frege-prog) — a third kind of link, no wire at all.
      - the DATA: `PyValue` and `RValue` are two enums that say the same
        thing twice; `Json` is the wire's tree; `okay.arrow.Table` (typed
        columns, nested, Arrow IPC) is the bulk road, and only Python, R
        and TypeScript serve the `frame` op today (backlog
        foreign-frame-op-rust-hs-go). Measured: our share of a 100 000-row
        R round trip is 0.3% and the rest is jsonlite walking the JSON
        (specs/r.md, r-arrow) — the tree road is the ceiling, the columnar
        road is the floor, and today a language gets whichever its shim
        happened to grow.
      - the HIGH LEVEL: `ForeignEval`/`REval` (call, frame, map/reduce
        stages in okay-foreign-cluster; specs/foreign-highlevel.md,
        foreign-map-reduce), each a per-language name (`mapPy`, `mapR`)
        where one name over a language handle would do.
      THE SHAPE TO DESIGN, as a spec first (spec-dev, specs/foreign-facade.md):
      1. `Foreign[L]`: one interface per CAPABILITY, not per language —
         run a program, perform a callback, send/receive a frame, a
         value's codec — with a language as a value that says which
         transports and which roads it speaks (`speaks` in the hello is
         already that, half-written). Adding a language is implementing
         the interface and passing the conformance suite; no new `mapXx`.
      2. ONE data model with THREE tiers, each the fastest road that
         exists for its shape, and the tier chosen by the DATA, not by the
         language: a scalar/record TREE (`Json`, or a `Schema`-typed value
         through the codec) for calls and control; a COLUMNAR frame
         (`Table`, Arrow IPC, zero-copy where the far side has Arrow —
         pyarrow, R arrow, Rust `arrow`, Go `arrow/go`, Java for
         Clojure/Frege) for bulk; and a STREAM of frames for what does
         not fit in memory (okay-stream's `Stage` on both sides, chunked,
         back-pressured through the wire's own `continue`). The model
         must say what every language MUST support (tier 1), SHOULD
         (tier 2, with the columnar JSON shape of r-frame-columnar-wire
         as the fallback when there is no Arrow), and MAY (tier 3) — and
         a language that lacks a tier degrades to the one below by a rule,
         never by a surprise. `PyValue`/`RValue` fold into the tier-1
         model or become views of it; one `Schema` describes a value on
         every side (okay-codec's `JsonSchema` for what a shim generates).
      3. The numbers that make it honest: a table per language × tier ×
         transport, rows/s and bytes/s, measured with the existing
         instruments (MeasurePyArrow, MeasureRFrame, MeasureRemote,
         MeasureForeignMapReduce) so the facade cannot hide a slow road
         behind a uniform API — the operator's "squeeze the most" is a
         number per cell, and a cell that is worse than the language's
         own best road is a defect.
      4. In-JVM languages (Clojure, Frege, Java) get the same facade with
         the link being a function call and the frame being the SAME
         `Table` object — the model has to allow a zero-cost tier, which
         is the test that it does not limit.
      Gate: the one-wire conformance suite extended to every capability,
      run over every language the build has; TestForeignStage's job with
      the map in each language behind ONE name; and the measured table in
      the spec's Results. Not a rewrite of the shims: the facade is the
      Scala-side seam and the model; each shim grows toward it one tier
      at a time (foreign-frame-op-rust-hs-go is the first such step).
