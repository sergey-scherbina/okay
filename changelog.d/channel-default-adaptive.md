## channel-default-adaptive - the default channel stays `growing`; adaptive loses at two producers

The question channel-known-producers left: now that `growing` holds
per-part capacity too, does the bounded default still need to ADOPT a
ring, or can it be `adaptive` — the same buffer without the one-shot
swap, with each producer's order exact? Answered by an A/B through
`Channel.apply`'s own switch, one lane per `Jmh/run`, arms alternating,
the arm printed by the benchmark's `@Setup` so a `-D` that did not reach
the fork could not pass for a result.

Not matched. `adaptive` wins where the spec feared it would lose — one
producer, 0.75-0.90 of `growing` — and loses where adoption is cheap:
two producers, `default_elem` 1.26 and 1.16 of `growing` in two
alternating rounds, past the ~15% band. Four and sixteen producers and
the actor tell/ask rows sat at 0.77-1.03. The controls held. The default
is unchanged; the verdict and the table are in
specs/channel-default-adaptive.md, the rows in src/jmh/history.d.

Two instrument lessons on a shared box, both now in the spec: a quiet
check before and after a lane cannot see a gate that starts in the
middle (rows of +-60% passed it), so a lane is accepted only at error
<= 10%; and `growing`'s own p=2 chunk lane never got there in ten
tries, filed as backlog growing-two-producer-variance. Also added:
`ManyProducersBenchmark.default_elem`/`default_chunk`, the first lanes
that measure what `Channel[A](n)` actually builds.
