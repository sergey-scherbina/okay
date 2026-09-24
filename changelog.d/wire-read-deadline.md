## wire-read-deadline - deadlines and recovery for foreign workers

The operator: "рилайибилити - если чтото отвалилось там и таймаут - что
тогда происходит? должно както все востанавливаться (по желанию конечно
со стороны обработчика)".

- `given WireDeadline = WireDeadline.after(d)`. On pipes and TCP an answer
  past the deadline closes the wire, and the call answers
  `Left(Condition("timeout"))`. In-process the deadline is refused by
  name: a call there cannot be abandoned.
- `ForeignWorker.supervised(open)` reopens the worker after a death or a
  timeout (a fresh process, a new connection).
  - Programs as data survive, even mid-run: each continuation's path of
    answers is replayed on the fresh worker, and a far side that turns out
    not to be deterministic is caught as `ReplayDrift`.
  - Plain calls and direct-style dialogues caught in the failure answer
    `WorkerDied`/`timeout`, so the caller decides about a retry
    (okay-platform's `retry`).
  - Held objects from before a restart are refused by name.
- Tests, live:
  - a Python process killed (`os._exit`) between two choices, and all four
    branches still arrive;
  - a Go server killed and restarted on its port mid-program;
  - drift, a death mid-ask, a stale ref, and a timeout.
  - Mutant: without replay, all three recovery tests fail.
- Docs: one-language.md "When the far side fails", with the literature:
  log-based rollback recovery (Elnozahy et al. 2002) and Armstrong's
  supervision. The "no read deadline" limit is gone.
- Backlog: `r-supervised-replay`.
