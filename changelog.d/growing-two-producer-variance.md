## growing-two-producer-variance - the p=2 slow mode is per JVM fork, and not growing's

channel-default-adaptive saw the default `growing` buffer's two-producer
chunk lane never come out tight, and filed it as a possible latency
cliff in the adoption swap. Measured through `scripts/jmh-lane.sh` with
per-iteration data (`-rf json`) against `adaptive` as the control:

The cost is bimodal, but per JVM FORK: a fork is fast (~150-230 us/op)
for all its iterations or 4-6x slower (606-1216) for all of them. And
it is not `growing`'s: 1/4 then 0/12 forks for growing, 0/4 then 4/12
for adaptive. Decided once per process, so the swap inside a run is not
the cause. The hypothesis in the backlog entry is refuted; the channel
verdict stands (its accepted p=2 rows were tight, no slow fork in them)
and specs/channel-default-adaptive.md says so. What remains — thread
placement on P/E cores or a JIT shape — is backlog
two-producer-fork-bimodality, with the one measurement each suspect
needs. Rows: src/jmh/history.d/*-two-producer-fork-bimodality.tsv.
