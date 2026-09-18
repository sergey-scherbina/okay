- [x] dataflow-auto-for-a-real-accumulator — CLOSED by measurement,
      and the prediction it rested on is REFUTED. §20's tuple tree
      (`count zip sum zip max`, six objects per add) crosses in the
      SAME 80 000-to-160 000 band as a Long count, in two runs — the
      accumulator's weight changes the slope past the crossing, not
      the crossing. One constant serves both and 100 000 is it.
      A methodological finding came with it: the first version of the
      table used a millisecond clock over lanes of 1-7 ms and reported
      the two crossovers four-fold apart. That was quantisation, not
      physics; the ratio is computed from nanoseconds now.
      The other half of the old comment — that fewer partitions move
      the bound up — is still unmeasured and is now labelled as such
      rather than asserted.
