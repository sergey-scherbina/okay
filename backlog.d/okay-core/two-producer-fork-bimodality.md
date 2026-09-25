- [ ] two-producer-fork-bimodality — at producers=2, consumers=1,
      `ManyProducersBenchmark.default_chunk` has a per-FORK slow mode:
      a JVM fork runs 4-6x slower (606-1216 us/op against 144-230) for
      every one of its iterations, under the default `growing` buffer
      and under `adaptive` alike (1/4 + 0/12 forks and 0/4 + 4/12, rows
      in history.d *-two-producer-fork-bimodality.tsv). Decided once per
      process, so not the adoption swap. Two suspects, one measurement
      each: THREAD PLACEMENT (Apple Silicon P- vs E-cores — pin with
      `taskpolicy` or run a fork with only P-cores available and see the
      slow mode vanish) and JIT (`-XX:+PrintCompilation` /
      `-prof perfasm` on a slow fork against a fast one — a different
      compiled shape of the hot loop). Matters beyond this lane: any
      2-producer benchmark read from 2 forks can land on a slow one.
