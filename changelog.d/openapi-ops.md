## openapi-ops - okay-ops's four routes stop being "undeclared"

`Ops.router` built all four of its responses by hand, so every
operation rendered `"undeclared — this operation builds its own
Response"` in the OpenAPI document. `/stats` now goes through
`Router.out` (the `Schema[Store.Stats]` it already summons),
`/metrics` through `Router.bytes` (Prometheus's own content-type,
declared once instead of typed loosely in two places).

`/healthz` and `/readyz` keep their own `Response` on purpose: their
status genuinely varies per request (a Kubernetes probe is exactly
the caller who reads that), and every declaring combinator fixes the
status at declaration time. `.answering(200).answering(503)` states
both outcomes instead, so the operation reads as one that can succeed
rather than as one with no answer at all.

New test in `TestOpsSurface`: no `Ops.router()` entry lacks a
declared 2xx answer — checked FAIL first against the unfixed code, on
all four routes. `TestOpsRoutes` (real socket, all four endpoints,
both 503 paths) and the doc-drift check pass unchanged; `Router.markdown`
never rendered answers, so `docs/modules/okay-ops.md` needed no
regeneration.
