## router-trie - dispatch by index, decision by entry

`Router.routes` asked every entry's `matches` per request, each
splitting and decoding the path again. Now a `Router.Index` — method
→ segment count → a trie over the template's literal segments, a
`{param}` a wildcard branch, leaves the entry indices in declaration
order — names the CANDIDATES for a request, and each candidate is
asked `matches`/`run` exactly as before: first-match, headers,
queries and security (fail-closed without a verifier) are the entry's
as they were, because every entry the index skips would have refused
its own `unapply` on the method, the count or a literal. Built once
per `Router` (a `lazy val`); a template `segmentsOf` cannot read is a
candidate for every request; a literal that looks like `{x}` is a
wildcard — a superset, still right. Laws (`TestRouterIndex`, JVM):
300 generated tables × 50 requests agree with a first-match scan on
definedness and on WHICH entry answers; candidates ⊇ the matching
entries and sorted; `++` keeps the order. `TestRouteHeaders`,
`TestRouterOut` unchanged. Filed by the staging survey as fourth of
four with a size trigger; the operator waived it (2026-09-23).
Measured (compare `RouterBenchmark`, one quiet pair, `rt-*`): a
request hitting the last of 3 / 30 / 300 routes, scan → index: 356 →
342 ns, 3.8 µs → 297 ns, 37.1 µs → 275 ns (2.4 KB / 21 KB / 233 KB →
~2 KB); a miss at 300: 40.7 µs → 79 ns. Parity at three, a constant at
any size — the scan paid ~120 ns and ~780 B per entry tried, the path
split and decoded again for each. docs/benchmarks.md §23; the module
page's table.
