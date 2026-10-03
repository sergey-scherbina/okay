## handling-ever-per-machine — the handler-frame flag, measured (2026-10-03)

Commits: 727cae2b4.

- DelimBenchmark gains `stateForeign` and `stateForeignEver`: N State
  operations performed on the machine under one delimiter and answered
  by `State.run` outside, with `Cont0.Handling.ever` as is (off — no
  handler frame is ever built on that path) and forced on.
- Result, arms alternated: 33.73 against 35.60 us/op, 1.055x. The
  process-wide flag saves what it was written to save; deleting it is
  refuted. The per-machine design stays in the backlog, with this number.
