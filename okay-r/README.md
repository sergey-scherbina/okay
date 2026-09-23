# okay-r

R as a handler (specs/r.md; the twin of [`okay-py`](../docs/modules/okay-py.md), and the model is stated once for both): call-shaped foreign compute. Calls are OPERATIONS — mockable by swapping the handler, supervised by dead-process-throws. Named functions only: the enum has no eval-a-string case, structurally, so untrusted input reaches R only as data.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-r.md`](../docs/modules/okay-r.md) | what it is, and the reasoning |
| [`specs/r.md`](../specs/r.md) | the design and its decisions |
