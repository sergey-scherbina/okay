- [ ] indexed-effects-measure — the deferred measurement of
      specs/indexed-effects.md, now that its six stages are landed. A/B
      `mine` = master after stage 6 (one machine) against `ref` = master
      before it (the old unstacked machine), on the lanes that exercise
      the machine: DelimBenchmark delimGenerator, delimPushOnly,
      delimDollarResume, writerTellUnderDelim, stateLexDeep; alternating
      rounds, MIN of 3, `jmh-lane.sh -f2 -wi3 -i5 -prof gc`. Then
      stateThreaded after the shared node against its recorded row.
      The operator's word: a regression found here is fixed in this
      lane, not filed. Rows to history.d, verdict to the spec's Results.
