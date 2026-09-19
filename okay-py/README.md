# okay-py

Python as a handler (specs/py.md; the model is specs/r.md's, verbatim): call-shaped foreign compute. Calls are OPERATIONS — journalable by `Durable`, mockable by handler swap, supervised by dead-process-throws. Named functions only: the enum has no eval-a-string case, structurally, so untrusted input reaches Python only as data.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-py.md`](../docs/modules/okay-py.md) | what it is, and the reasoning |
| [`specs/py.md`](../specs/py.md) | the design and its decisions |
