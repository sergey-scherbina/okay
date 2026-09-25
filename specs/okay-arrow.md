# okay-arrow — the Arrow columnar format, our own

Status: stage 0 (the measurement) open. Asked by the operator
2026-09-25: "можешь сделать модуль okay-arrow с нашей полноценной
реализацией формата arrow? У нас получается быстрее чем у оригинального
apache arrow?"

## Where it starts

py-arrow (specs/py-arrow.md) wrote `okay.codec.ArrowIpc`: Arrow IPC
streams for five columns (int64, float64, utf8, bool, null), read and
written by hand, pyarrow-validated. Nobody has measured it against Arrow
Java, so "faster than Apache Arrow" is a QUESTION until stage 0 answers
it. The design of everything after depends on that answer (a copy into
JVM arrays per column, as today, against Arrow's off-heap buffers read in
place).

## Stage 0 — the measurement, before any module code

JMH, one table of 500 000 rows (float64, int64, utf8 with nulls), Arrow
Java 19.0.0 as the reference, the same IPC bytes on both sides:

- WRITE from JVM arrays to IPC bytes: `ArrowIpc.write` against filling
  Arrow vectors and `ArrowStreamWriter`;
- WRITE from each library's own columns: ours are already arrays; Arrow's
  `VectorSchemaRoot` already filled (its best case);
- READ IPC bytes to JVM arrays: `ArrowIpc.read` against
  `ArrowStreamReader` plus a copy out;
- READ into each library's own columns: ours are arrays; Arrow's
  `VectorSchemaRoot` loaded, no copy out (its best case).

Interop first: each reads the other's bytes and gets the same table,
or no number is recorded. Lane rules (docs/benchmarks.md): each lane
names what it includes, and a lane where one side does work the other
skips is not compared.

- [ ] Stage 0: the four pairs measured, interop checked, verdict here.

## The module (staged after stage 0; order may change on its verdict)

- [ ] Stage 1: `okay-arrow`, the format's TYPE SYSTEM as data: every
      Arrow type (null, bool, ints 8–64 signed and unsigned, floats
      16/32/64, binary/utf8 and their large and view forms, decimal
      128/256, date, time, timestamp with zone, duration, interval,
      fixed-size binary, list/large list/fixed-size list/list view,
      struct, map, sparse and dense union, dictionary encoding, run-end
      encoded), schema and field metadata. `okay.codec.ArrowIpc` becomes
      a client of it (the five columns stay its fast path).
- [ ] Stage 2: IPC STREAM for every type, nested included, and
      dictionary batches (deltas too).
- [ ] Stage 3: IPC FILE format (magic, footer, random access to a batch).
- [ ] Stage 4: body compression: LZ4_FRAME and ZSTD — neither is in the
      JDK; each is its own decision (a pure-Scala codec, or refused by
      name with the reason).
- [ ] Stage 5: the C Data Interface over FFM: a pyarrow table in THIS
      process handed over without a copy (the in-process twin of the
      wire).
- [ ] Every stage: pyarrow as the oracle both ways, a cut stream
      refused at every byte, JMH against Arrow Java on the lanes the
      stage adds.

## Decisions

## Results
