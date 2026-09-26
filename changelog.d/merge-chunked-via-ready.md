## merge-chunked-via-ready — the chunked merge keeps its shared channel (measured), and stops losing a failing side's tail

Moving `Source.merge(chunked = true)` and `mergeFlushing` onto the ring
(a chunk channel per side joined by `mergeReady`) was built, measured
and REVERTED: on `ChunkFlushBenchmark`, 5 forks per arm, two rounds,
no win anywhere and ~1.2x in half the forks — the ring road's forks
came out bimodal (~200 or ~255 us) where the shared channel's held at
~200-215. The elementwise merge stays on the ring, the chunked roads on
the channel, with the numbers as the reason (specs/source-merge-via-
ready.md; history `…-merge-chunked-via-ready.tsv`). What the attempt
found and KEPT: a chunking feed that failed dropped the partial chunk
it had accumulated, so a chunked merge over a source telling 1, 2, 3
and then throwing delivered the other side and the failure but none of
the three — `Channel.failAfterTail` sends the told tail before the
failure is read (`TestChannelFailure`, a Source-level and a
channel-level law, both watched red on master). The flusher is one
helper now (`flusherFor`).
