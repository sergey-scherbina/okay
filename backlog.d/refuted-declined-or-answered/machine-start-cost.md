- machine-start-cost — answered 2026-10-04, nothing to do: starting a run
  is cheaper than a step inside a long one. `DelimBenchmark.delimRunEach`
  (N = 1000 runs, each with one `dollar` + `shift0` + resume) takes
  55.6–60.5 µs. `delimDollarResume` (the same N steps in ONE run) takes
  68.5–80.0 µs (0.76–0.81x, two rounds, history.d machine-start-cost).
  A whole run, its `Run`, `Steps`, `Nested` and prompt included, is
  ~56–60 ns. The 4.3x that filed this was the handlers-as-frames probe
  starting a machine per HANDLER, a path that no longer exists.
