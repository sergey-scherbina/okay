## foreign-streaming - Python and R functions as okay stages over chunks

Stage 6 of specs/foreign-highlevel.md.

- `Py.stage[I, O]("mod:fn", chunk)` and `R.stage(...)` pull a chunk,
  call the function once with it as a list or vector, and tell whatever
  comes back. The stage maps, filters or expands, and composes with
  `through`.
- A slow model back-pressures its source. A test in the default gate
  checks that the source has produced exactly 4, 8 and 10 elements at
  the three calls.
- A stateful stage is a held Python object (`ref.stage(method, finish)`)
  or a held R closure, called through `do.call`.
- A failure ends the stage and names the condition.
- okay-py and okay-r now depend on okay-stream.

Tests: 7 live and 1 in the default gate. A mutant is caught. Docs:
"Streams through Python/R" in docs/modules/okay-py.md and
docs/modules/okay-r.md.
