# okay-ops

> Health, stats and Prometheus over the values that already exist
> (specs/ops.md): a mapping, like OTLP is for tracing in okay-obs,
> never an SDK — the whole point is that a scraper, or a Kubernetes
> probe, needs to know nothing about this stack to read it.

Depends on: `okay`, `okay-codec`, `okay-persist`, `okay-http`. Tests:
a real socket (`okay-jetty`, JVM test scope).

## The surface is a value, so a deployment can read it

`Ops.routes` used to match with `r.url == "/healthz"`, and a manifest
that had to name that path wrote its own literal. Three copies of one
string — the route, `okay-deploy`'s `Health` defaults, and whatever
each deployment wrote by hand — with nothing able to tell you when one
of them was wrong.

The four endpoints are values now (specs/optics-outside.md, stage 4):

```scala
Ops.healthz   // Route[EmptyTuple], describes as "/healthz"
Ops.readyz
Ops.stats
Ops.metrics
Ops.paths     // Set("/healthz", "/readyz", "/stats", "/metrics")

Ops.router(store, …)   // the four, declared once
Ops.routes(store, …)   // the same, as the PartialFunction every server takes
```

The surface itself, rendered from the router that serves it —
`TestOpsSurface` asserts this block is what `Ops.router(...).markdown`
produces, so an endpoint cannot be served and undocumented, or
documented and unserved:

<!-- generated: Ops.router(store).markdown -->
| verb | path | body |
|---|---|---|
| `GET` | `/healthz` | — |
| `GET` | `/readyz` | — |
| `GET` | `/stats` | — |
| `GET` | `/metrics` | — |
<!-- /generated -->

`paths` exists apart from the router because a deployment has to name a
probe and has no `Store` to build a router with — that is exactly what
a describing interpreter is for. `TestOpsSurface` asserts that `paths`
is precisely what `router(…).describe` dispatches, so the two cannot
drift; okay-script does the same for its own three with `Site.Ops`, and
its `ScriptDeploy` names both probes from there.

One behaviour changed, and it was a disagreement nobody had noticed:
`Ops` compared the whole url while okay-script's `Site` compared only
the path, so `/healthz?probe=1` was served by one and missed by the
other. Both behave like `Site` now.

## Guide

**Wire it in.** `Ops.routes(store)` is a `PartialFunction[Request,
Response ! Async]` — compose it beside any server's own routes, the
same way `Secure.bearer` composes protection:

```scala
case r if Ops.routes(store).isDefinedAt(r) => Ops.routes(store)(r)
```

**The four routes.**

| route | answers |
|---|---|
| `GET /healthz` | `200 live=true` / `503 live=false (reason)` — a Kubernetes `livenessProbe` |
| `GET /readyz` | `200 ready=true` / `503 ready=false (reason)` — a `readinessProbe`; `503 ready=false (draining)` once a `Lifecycle` is draining |
| `GET /stats` | `Store.Stats` as JSON — the Schema already derived, no new codec |
| `GET /metrics` | Prometheus text exposition (`text/plain; version=0.0.4`) |

Health is computed by CALLING `store.stats` — never a cached flag.
Liveness and readiness answer the same question for a `Store` that
opens synchronously (opening IS running recovery), so they agree in
practice; they stay two routes because Kubernetes's own contract
does, and the two CAN diverge for an engine where they do not.

**Consumer lag**, per group, is opt-in: `Ops.routes(store, lagOf =
Vector(("workers", offsets, Vector(topic))))` — a `Store` keeps no
registry of its own consumer groups, so the caller names them.

**Graceful shutdown.** `Lifecycle()` is a value with a draining flag
and an in-flight count. Wrap the application's routes in
`lifecycle.route(...)` — every request is counted, and once draining
a new one is answered 503 `Connection: close` without running. Keep
the ops routes OUTSIDE the wrapper: `Ops.routes(store, lifecycle =
Some(l))` makes `/readyz` answer 503 while draining, and `/healthz`
must keep answering 200 (an un-live pod is restarted, an un-ready one
merely leaves the Service's endpoints). On the JVM,
`Signals.awaitSignal(l, readinessDelayMillis = 2000, graceMillis =
15000)` blocks the main thread until SIGTERM or ctrl-c, flips
readiness, waits the delay for endpoint removal to propagate, drains
in-flight requests up to the grace, and returns — so the region's
own release stops the server, and the shutdown hook holds the JVM
open until that has happened:

```scala
Resource.run[Unit, Pure](
  Jetty.serve(port)(Ops.routes(store, lifecycle = Some(l)).orElse(l.route(app)))().map { s =>
    val drained = Signals.awaitSignal(l)   // SIGTERM → ready=false → delay → drain
  }).runWith                               // → the region stops Jetty
```

Set the pod's `terminationGracePeriodSeconds` above delay + grace.

**RED per route and per client.** `Red("api")` keeps, per label,
requests by status class (`2xx` … `5xx`, `exception`), errors (a 5xx
or a throw) and a duration histogram in fixed buckets (5 ms … 10 s).
`red.route(Red.byMethodAndPath)(routes)` measures a server's routes,
`red.http(_ => "payments")(client)` an outbound `Http`; `Ops.routes
(store, red = Vector(red))` renders
`okay_http_requests_total{name,route,class}`,
`okay_http_errors_total{name,route}` and
`okay_http_request_duration_seconds_{bucket,sum,count}` — a
Prometheus histogram `histogram_quantile(0.99, ...)` reads directly.
The clock is injectable, so a bucket is testable without sleeping.

**Wiring it to Kubernetes and Prometheus** is a manifest, not code:
point `livenessProbe`/`readinessProbe` at `/healthz`/`/readyz`, and
either a `prometheus.io/scrape: "true"` annotation (path `/metrics`)
or a `ServiceMonitor` at the same routes — see deploy-k8s. Terraform
(or any IaC tool) applies that manifest; it never talks to this
process directly.
