## aggregator-sum-hides-its-specialization - sum[N] answers OfLong/OfInt/OfDouble directly

`sum[N]`'s declared return was `Aggregator[N, N, N]`, hiding the
specialized `OfLong`/`OfInt`/`OfDouble` underneath — so `.zipLong`
was unreachable from the idiomatic spelling, and `Aggregator.sumLong`
had to be named directly, the whole usability cost the
specialization's own doc comment already named.

A match type (`SumOf[N]`) on `sum`'s return reduces structurally for
the three specialized branches with no new cast — the same
`substituteCo` evidence `sum` already carried transports
`SumOf[Long]` to `SumOf[N]` exactly as it did `Aggregator[X, X, X]`.
Only the generic `Numeric` fallback branch needs one: `N` there is
proven to be none of `Long`/`Int`/`Double` by `summonFrom`'s own
exhaustion, which IS `SumOf`'s wildcard case, but a match type can't
reduce for an abstract `N` with no disjointness proof the type
checker can use — isolated to that one line, commented, per
no-casts-without-necessity (checked: the other three branches compile
with none).

`TestAggregate`'s `zipLong` test updated to the idiomatic spelling
(`A.sum[Long]` instead of `A.sumLong`) — checked FAIL first against
the old signature (three compile errors, the direct one: "Found:
`Aggregator[Long, Long, Long]`, Required: `OfLong[Nothing, Any]`").
16/16 green; repo-wide `Test/compile` and a cold full gate (223
modules, 5652 tests) both clean — no other call site relied on
`sum[N]`'s old, wider return type.
