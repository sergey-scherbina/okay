# okay-ops

Health, stats and Prometheus over the values that already exist (specs/ops.md): a mapping, like OTLP is for tracing in okay-obs, never an SDK — the whole point is that a scraper, or a Kubernetes probe, needs to know nothing about this stack to read it.

**Depends on:** `okay`, `okay-codec`, `okay-persist`, `okay-http`. Tests:

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-ops.md`](../docs/modules/okay-ops.md) | what it is, and the reasoning |
| [`specs/ops.md`](../specs/ops.md) | the design and its decisions |
