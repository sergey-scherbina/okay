## merge-cap256-gap — the merge's one named loss closed, and turned into a win

Ten forks per arm at capacity 256 showed the OLD shared-channel merge
had pathological forks (81..725 us in one round, 5-10x slow — the
"±9 noise" of earlier rounds) that the ring merge never showed, and a
real ~16% edge in its good forks. `-prof gc` named the cause: ~65 B more
per element, because every element was told twice — by the side
(`Writer.of(Drain)`'s per-element `Some`, tuple and `Drain` copy) and
again by the merge (a fresh `Say`, `Inject`, `Bind` and lambda). Now
`Channel.drained` is a hand loop over the received batch (an element
costs its tell and one Bind — every `drained` caller gains), and
`ReadyMerge` forwards the source's own `Inject(Say)` with one shared
continuation. After, medians of 5 forks, two rounds: 0.81x of the old
road at cap 64, 0.91-0.93x at 256, 0.77-0.85x at 1024,
`okaySourceMerge` 0.73-0.82x, 12-14% fewer bytes. Rows in
`src/jmh/history.d/…-merge-cap256-gap.tsv`; specs/source-merge-via-ready.md.
