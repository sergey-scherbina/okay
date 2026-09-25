## foreign-one-model — the foreign languages as ONE model (spec rewrite, 2026-09-26)

The operator, after the first cut of specs/foreign-one.md listed five
gaps: take everything built before — py, r, the wire, the facade,
map-reduce, streams-holds, Frege/Clojure — and derive one consistent
system with nothing superfluous. specs/foreign-one.md is rewritten as
that model: FOUR things cross a language boundary (a value, a table, an
object, a stream) plus a program, and the tier is per ARGUMENT, so the
wire's eleven operations are five (`call` with `held` and a polymorphic
address, `program`/`continue`/`forget`, `release`) and a message is a
head plus parts; `Runtime[L <: Lang]` with capabilities as compile-time
MARKERS on the language tag (one engine under every wire language, one
walker under every JVM language — so instances with a body per language
are gone); one `Pool` (use, lease, route by ref, perWorker, supervise);
`Language[L]` as the one object a language adds; streams symmetric under
credit; one-shot vs multi-shot a claim, not a second protocol; the
transcript as the protocol's specification. A today→model table says of
every existing piece whether it is kept, folded or deleted (the
stream-shape bridges — transducers, gatherers, core.async — stay
outside). Eleven Decisions. Eight stages in backlog.d/polyglot:
foreign-one-r, -protocol, -pool, -runtime (subsumes -modules, deleted),
-mux, -bulk, -ops, -docs. Pointers added to foreign-facade.md and
foreign-map-reduce.md; the sibling lanes' APIs (Reduce.in, statefulIn,
Model.in) are kept as derived combinators. Docs-only.
