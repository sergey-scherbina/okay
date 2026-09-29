## merge-flush-on-ring-gap — the ring's flushing gap does not hold; the scheduler flip costs chunked merge ~1.7x

Taken over by the operator's ask from a session silent since its spec.
Re-measured, every lane through `scripts/jmh-lane.sh` with arms
alternating (rows: src/jmh/history.d/2026-09-29T115453Z-merge-flush-on-ring-gap.tsv):
`okayChunkedFlush` ring vs shared reads 1.05x under loom — the
conditions of the 1.12x reading — inside the accepted 1.06x, and 0.88x
on today's default, where the ring is the faster road. Nothing in the
library moves. The control pair found the bigger fact: the default flip
to `adaptive` (c29a5820d) costs the chunked merge 1.5-1.9x on both
roads, and the elementwise merge 1.16x at cap 64; filed as backlog
`adaptive-chunked-merge-cost` (HIGH). The elementwise road on
`Wait.Ladder` shows no regression under loom. specs/ready-merge.md,
the gap stage's Results.
