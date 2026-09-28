# okay-dlm

A deterministic dialogue language model, as a library: the four-layer router, one `Head` per question over a table of exemplars, the pure turn decision with its journal record, the memory fold, the language detector and the safetensors checkpoint — the mechanism a chat service keeps its data OUT of. Lifted from a service that ran four languages through a hand-rolled version of it (specs/dlm.md).

**Depends on:** `okay-intent`, `okay-agent`

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-dlm.md`](../docs/modules/okay-dlm.md) | what it is, and the reasoning |
| [`specs/dlm.md`](../specs/dlm.md) | the design and its decisions |
