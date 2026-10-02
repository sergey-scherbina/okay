- cont-frames-head-form-run — ANSWERED 2026-10-01 by profile, no
  code change. writerTellUnderDelim (a foreign effect under one
  delimiter: the head form out and back per operation) reads 1.13x the
  single-list machine after two cuts on that road (`Rev.onto` empty
  machine first; a forced `Resume` entering at the registers), with
  FEWER bytes than it (270 vs 286 KB/op). async-profiler, 35k samples
  an arm (branch 29.7 us, single list 26.4): the machine's own
  `loop$1` self time is LOWER on the branch (9.0% vs 13.3%), the outer
  `Freer.resume` about even (8.7 vs 7.7); the difference sits in the
  benchmark's own lambda `go` (36% inclusive vs 23%) and its Integer
  boxing (31% self vs 15%) — the same code, allocating less, slower
  where C2 inlines it into `loop$1`. That is cont-frames-register-
  pressure's cause reaching inlined user code, not the head form: the
  `Run` per operation does not appear in the profile. Nothing to cut
  on the road itself.
