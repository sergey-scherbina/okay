## foreign-callbacks - Python and R call back into okay

Stage 7 of specs/foreign-highlevel.md, taken first at the operator's
request. Python code calls `okay.call("name", ...)` and R code calls
`okay_call("name", ...)` in the middle of a call. Each call runs a
callback that okay offered (`Py.callback`/`R.callback`, then
`fn.calling(callbacks)`). The callback is an okay program in the
caller's row F, so it reads the caller's Reader, updates its State,
sleeps, journals, or calls Python/R again on the same worker.

- The call is a short dialogue: `Start` answers `Done` or `Ask`, and
  `Resume` sends back the callback's answer. The result is a program in
  `F + PyEval` / `F + REval`.
- `Durable` journals the dialogue, and a replay needs no interpreter.
- `PyWorkers` keeps one worker for the whole dialogue.
- A failed callback raises `okay.OkayError` in Python and an
  `okay_error` condition in R.

The shims move to Python 3 and R 4. Tests: 13 live tests (a Python
optimiser and R's `optimize`, both minimising an objective that asks
okay's Reader) and 2 in the default gate. A mutant is caught. Docs:
"Callbacks into okay" in docs/modules/okay-py.md and
docs/modules/okay-r.md, with literature.
