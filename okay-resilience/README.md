# okay-resilience

Five handlers around one operation (specs/resilience.md): a circuit breaker, a bulkhead, a keyed token-bucket limiter, hedged requests and a deadline that travels with the request. The program stays blind to them — it says `http.send(r)` — and the edge decides what stands between that and the wire, the way `Tracer.traced` wraps a Handler and `Secure.bearer` wraps a route.

**Depends on:** `okay`, `okay-http` (the `Request` the deadline header

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-resilience.md`](../docs/modules/okay-resilience.md) | what it is, and the reasoning |
| [`specs/resilience.md`](../specs/resilience.md) | the design and its decisions |
