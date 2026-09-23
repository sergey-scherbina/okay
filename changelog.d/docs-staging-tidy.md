## docs-staging-tidy - one account of the staging and generator arcs, numbers checked

Ten lanes landed on 2026-09-22/23 (handler-fusion-flat, direct-staged
and v2, direct-loops v2, direct-stagers, generators, gen-filter-as-walk,
gen-chain-fusion, gen-flatmap-fusion, typeablek-instanceof, the
refuted direct-inline-bind-free), each patching the docs it touched;
the operator asked for the docs to be brought into one order. Done:
docs/benchmarks.md §21 rewritten as one account of `Gen`'s cost —
the wrapper free, the pipeline from three walks to one read in a
table, the refused variants — and a new §22 for staged direct blocks
(the two blocks three ways, the stagers' price, what reached parity
and what the rows refused, the derived test as `instanceof`);
direct-style "What it costs" as one paragraph; the typepedia `Gen`
entry rewritten coherently; the README gains `Direct.staged` under
Effects and `Gen[W]` under Streams; the guide's direct-style paragraph
names the staged and generator doors; specs/generators.md's Design
bullet says the walks are `program` now; theory ch. 7's fusion
sentence names `flatMap`/`++`. Every number on the page was checked
against `src/jmh/history.tsv` before it was written down (six were the
sibling's `gj-*` rows at three decimals). No new snippets: every
example on these pages was already verbatim in a gated test.
