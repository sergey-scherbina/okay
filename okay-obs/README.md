# okay-obs

Tracing without a framework (specs/obs.md): a span is a VALUE with a Schema, appended to a persist topic — retention is a Policy, sampling is which spans you write, shipping is a consumer. W3C `traceparent` is the one propagation vocabulary; programs stay observability-blind.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-obs.md`](../docs/modules/okay-obs.md) | what it is, and the reasoning |
| [`specs/obs.md`](../specs/obs.md) | the design and its decisions |
