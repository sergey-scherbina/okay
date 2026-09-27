## ring-chunk-bimodal-forks — why the ring merge was slow with chunks: two regimes, and the sides' one-shot receives

Answered by per-fork counters on the reverted chunked ring road. Three
explanations REFUTED on the way: JIT inlining (FreqInlineSize 650,
MaxInlineLevel 30 — slow forks remain), thread placement (one carrier:
uniformly 2x slower, no modes), the merge's own parks (a spin before
parking removed them, 0.0 per op, and the slow forks stayed). The cause:
the sides. A slow fork made 84-217 side wake-ups per op against 11-20 —
once the consumer catches up, each side's channel runs empty, its
receive parks, the channel hands the next send over as ONE element, and
the wake work runs on the producer's thread, which keeps the consumer
caught up; which regime a fork lands in is early timing. The old road's
shared channel rarely empties. Design fix filed as
`ring-standing-receiver`. Rows in
`src/jmh/history.d/…-ring-chunk-bimodal-forks.tsv`; specs/source-merge-via-ready.md.
