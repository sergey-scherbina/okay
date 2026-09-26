## ready-merge-numbers — the ready-merge measured, the first lanes through bench-window

`MergeBenchmark` 2x500, two rounds alternating, control steady (44.31 /
44.51 us). Matched pair, a fiber per side in both: `mergeReady` over
two buffered sides 98.6 / 99.0 us against `Source.merge` 114.3 / 106.6
— 0.86x / 0.93x, bars separate in both rounds, the tighter lane too.
Ready inputs with no fiber at all: 68-70 us. Rows in
`src/jmh/history.d/2026-09-26T165404Z-ready-merge.tsv`; spec Results,
guide §6 and benchmarks.md carry them. Filed `source-merge-via-ready`
(one merge mechanism) for the operator's call. The run was also
bench-window's first live test: the lane's own compile gate was held
900 s behind two sibling lanes and started on the cap, then eight
lanes ran in eight minutes after an afternoon of 101 failed attempts.
