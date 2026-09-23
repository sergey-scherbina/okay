## gen-read-stop-residual - the Stop arm was never the cost; unfold's Option/Tuple is

generators-jmh (2026-09-23) named "the Stop arm in every split" as
the cause of `Gen.unfold.toList` reading +40%/+24 B/elem over
`Writer.foldUntil` on the same program — and the claim survived three
more lanes on the same machinery (gen-filter-as-walk, gen-chain-
fusion, gen-flatmap-fusion) plus typeablek-instanceof, which changed
that exact test's compiled shape, without anyone re-measuring it.
A decisive lane: `Gen.of(prog)` widens the SAME `prog` by `Stop` and
nothing else, read through `Gen.foldUntil`/`Chain`/`Xf.Id` —
1 917 025 B, LESS than the Stop-free `Writer.foldUntil` floor
(2 153 945). Widening by `Stop` costs nothing. A second isolating
lane confirmed the real cause: `Gen.unfold`'s own `S => Option[(W,
S)]` step boxes an `Option`, a `Tuple2` and two `Long`s per element
for a scalar state; the identical shape written directly for `Writer
% Long` — no `Stop` anywhere — pays +71 B/elem over the same floor.
Not fixed, and filed as such: `Option[(W, S)]` is Scala's own
`unfold` convention, and the tax is specific to a scalar-state
benchmark shape production code with a case-class state does not
pay. Rows `grsr-*`; docs/benchmarks.md §21 and specs/generators.md
corrected; backlog.d/refuted-declined-or-answered/gen-read-stop-residual.
