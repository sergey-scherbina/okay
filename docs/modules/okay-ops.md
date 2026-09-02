# okay-ops

> Health, stats and Prometheus over the values that already exist
> (specs/ops.md): a mapping, like OTLP is for tracing in okay-obs,
> never an SDK — the whole point is that a scraper, or a Kubernetes
> probe, needs to know nothing about this stack to read it.

Depends on: `okay`, `okay-codec`, `okay-persist`, `okay-http`. Tests:
a real socket (`okay-jetty`, JVM test scope).

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
| `GET /readyz` | `200 ready=true` / `503 ready=false (reason)` — a `readinessProbe` |
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

**Wiring it to Kubernetes and Prometheus** is a manifest, not code:
point `livenessProbe`/`readinessProbe` at `/healthz`/`/readyz`, and
either a `prometheus.io/scrape: "true"` annotation (path `/metrics`)
or a `ServiceMonitor` at the same routes — see deploy-k8s. Terraform
(or any IaC tool) applies that manifest; it never talks to this
process directly.
