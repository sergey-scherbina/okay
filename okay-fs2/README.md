# okay-fs2

Streams cross CHUNK FOR CHUNK — both sides are chunked, nothing is re-buffered, and each side backpressures its own native way.

**Depends on:** `okay` (JVM), fs2-core (and cats-effect underneath).

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-fs2.md`](../docs/modules/okay-fs2.md) | what it is, and the reasoning |
