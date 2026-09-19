- chunks-fold-vs-foldleft-2x-gap — PROBED TWICE AND CLOSED 2026-09-19:
  the library `Chunks.fold` over `Producer[Chunk[A]]` read 2.53 us where
  every other Producer loop compiled in `compare` read 4.5-4.8 —
  own method (chunks-retype lanes) and, this lane, the same loop with
  `A` kept abstract at the loop (`Probe.foldProducerAbstract`, exactly
  the library arm's shape): 4.76 vs 4.61, no change, allocation
  identical (history.tsv `chunks-fold-2x-gap-abstract-A-probe`). Both
  named hypotheses refuted; the Feed twin sits at 2.60 in every
  placement. Since the `Chunks` retype (76408290) the 2.53 row cannot
  be run through the library at all, so nothing in okay is priced by
  this gap any more. What would settle it is instruction-level
  profiling (`-prof perfasm`), absent on this Mac. The probe rows stay
  in ProducerWriterCarrierBenchmark.scala as the record.
