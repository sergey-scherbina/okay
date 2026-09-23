## gen-filter-as-walk - Gen.filter is a walk, not a splice: 0.83 of the time, 27 B/elem fewer

generators-jmh found the one `Gen` combinator that was not parity with
the hand road: `filter` was `splice`, an `emit`/`empty` program and a
`flatMap` built per element, +109 B/elem. Now `Gen.filtering` walks
the program as `taking` does — the kept tell is the input's own
`Inject` node re-bound to the rest of the walk, the rejected one a
`Free.delay` skip so a run of 100 000 rejections is flat. Laws
(`TestGen` +1): the laziness counter through `filter.take` (two
rejected, two kept, four steps), `iterator` on a filtered generator,
100 000 rejections in a row, `withFilter.map`. Measured on two quiet
alternated pairs (history `gf-*`): `map.filter.toList` over 10k
341.3 → 283.9 µs (0.83), 381 → 354 B/elem; the unfiltered lanes
byte-identical. Tried and not kept: a budget that recursed straight
through up to 64 rejections before deferring — 330 B/elem and 7%
SLOWER; the runner's trampoline beats a call chain through `split`'s
closure. What is left over the hand road (272 B/elem) is the walk's
`Bind` per kept and `Delay` per rejected element — `gen-chain-fusion`
(backlog), the filter fused into the reader. Docs: direct-style "What
it costs", benchmarks §21, the spec's Results.
