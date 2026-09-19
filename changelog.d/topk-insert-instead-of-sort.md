## topk-insert-instead-of-sort - the accept path stopped re-sorting an already-sorted list

`Aggregator.topK` (docs/benchmarks.md §9h) already refused most elements
with one comparison and no allocation, but an element that DID make the
cut still paid `xs.sorted(using O.reverse).take(k)` on the whole k+1-element
accumulator: a copy out to an `Array`, a sort, and a copy back — and `take`
rebuilds the list a second time, since a `List` cannot share a prefix once
its tail is cut short. The accumulator was already sorted; nothing about
the accept needed a sort at all.

`insert` walks the sorted accumulator once, finds where the new element
belongs, and stops at exactly `k` — one array-free pass instead of a sort
and two list rebuilds. Re-measured rather than trusted from the (stale)
§9h numbers, `compare/runMain okay.TopKProbe 10000 8`, same box, same run:

| 10 000 records, k = 8 | bytes |
|---|---|
| guarded, `sorted`+`take` (before) | 60 568 |
| guarded, `insert` (after) | **10 736** |

−82.3% of what the accept path was still allocating. The boxed-`Double`
case moves less (270 328 → 250 736, −7.3%) — boxing dominates the
accumulator itself there, so the array round trip was cutting away
proportionally less.

No behaviour change: `TopKProbe`'s own `require` confirms the two
implementations select the same elements, and `TestAggregate`'s 16 cases
— including the tie-refusal pin (an element EQUAL to the k-th stays
refused, so among equals the first seen survives) — still pass.

Gate: `scripts/gate.sh "affected master"`, GREEN, 5571 test results.
