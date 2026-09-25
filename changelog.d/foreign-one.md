## foreign-one — one runtime model behind the foreign facade (spec, 2026-09-25)

specs/foreign-one.md: the operator asked for one way of working with
every foreign language — effects and callbacks both ways, streams,
async, bulk data — fast, and open to the next language. The caller's
side is foreign-facade's typeclasses; this spec names the five gaps
between the facade and the three runtime families (the `ForeignWorker`
wire, R's own engine, the in-JVM `okay.Foreign` walker) and files one
stage per gap in backlog.d/polyglot: `foreign-one-r` (R onto
`ForeignWorker`, a deletion of ~2 300 lines), `foreign-one-mux` (the
wire multiplexed by id with credit-based far-driven streams — the one
protocol change, journal by id), `foreign-one-bulk` (`frame` in
Rust/Haskell/Go, Arrow C Data zero copy over FFM; subsumes
foreign-frame-op-rust-hs-go), `foreign-one-ops` (one effect declaration,
Frege/Clojure stubs generated), `foreign-one-modules` (module types for
every language, a golden wire transcript, the four-step "add a
language"). Eight Decisions, a measurement table to fill, and what is
deliberately not redone. Two records made stale by it are pointed
forward: foreign-facade's Decision 7 and one-language's "R is not behind
the gateway". Docs-only; no code changed.
