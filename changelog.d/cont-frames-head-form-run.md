## cont-frames-head-form-run - answered by profile: writerTellUnderDelim's 1.13x is the loop's register pressure reaching inlined user code, not the head form

- async-profiler on DelimBenchmark.writerTellUnderDelim, the segmented
  machine against the single list: the machine's own loop takes LESS time
  (9.0% vs 13.3% self), the outer handler about the same; the extra time
  is the benchmark's own continuation lambda and its boxing, inlined into
  the compiled loop (36% vs 23% inclusive) — the same three-registers-
  in-stack-slots cause as cont-frames-register-pressure. The head form's
  `Run` per operation is not in the profile. No code change; the item is
  answered in backlog.d/refuted-declined-or-answered.
