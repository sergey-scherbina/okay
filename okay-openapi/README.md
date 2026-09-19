# okay-openapi

The OpenAPI document as a rendering of the router that serves it — paths, methods, parameters (path and query, each with the kind its `Param` declared), request bodies and declared responses, every schema being the one the codec was derived from.

**Depends on:** `okay` (JVM), `okay-http`, `okay-codec`.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-openapi.md`](../docs/modules/okay-openapi.md) | what it is, and the reasoning |
| [`specs/route-headers.md`](../specs/route-headers.md) | the design and its decisions |
