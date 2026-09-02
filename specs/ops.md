# Ops: health, stats, and the standard wires (Prometheus, Kubernetes)

## Overview

specs/persist.md already names the doctrine: STATS ARE VALUES
(`Store.stats`, a plain case-class tree with a Schema), OPS EVENTS
ARE TOPICS, and HEALTH IS TWO BOOLEANS WITH REASONS — but nothing
served any of it over HTTP, so nothing OUTSIDE this stack could read
it. specs/obs.md answered the same shape of question for tracing
(spans as values, OTLP export as a pure consumer/mapper, no SDK).
This spec is the operator's ask (2026-09-02): the SAME values,
mapped to the wires operators already run — Prometheus scraping,
Kubernetes liveness/readiness probes — plus what a deployment needs
to be applied by the tools that apply deployments (a container
image, a manifest Terraform's `kubernetes`/`helm` provider can push).
No metrics SDK, no Kubernetes client library, no Terraform provider:
a documented wire is a mapping, exactly as OTLP was.

## The model

- **Health = two booleans with a reason**, already named in
  specs/persist.md: liveness (does the store answer at all —
  `stats` completes without throwing) and readiness (is it PAST
  recovery and serving — the same call, since a `Store` that opened
  is, by construction, past its own recovery scan). One value,
  `Ops.Health(live, ready, reason)`, computed by CALLING the store,
  never cached — a probe that lies because it read a stale flag is
  worse than a probe that costs one method call.
- **Stats stay values; `/metrics` is a MAPPING of them**, not a new
  source of truth: `Prom.render(Store.Stats)` is a pure function to
  Prometheus's text exposition format (HELP/TYPE comments, one gauge
  per (topic, partition) dimension) — testable as a golden string,
  the same move as `Otlp.body`. A Prometheus server, or anything
  that speaks its scrape contract (a Kubernetes `ServiceMonitor`, an
  OTEL Collector's prometheus receiver), reads it without knowing
  this stack exists.
- **The wire is the interop, not a library**: `/metrics` is Prometheus
  TEXT FORMAT 0.0.4 (still every scraper's baseline); `/healthz` and
  `/readyz` are what a Kubernetes `livenessProbe`/`readinessProbe`
  already expects — an HTTP GET, 200 or a non-2xx. A manifest wiring
  those two paths as probes and pointing `prometheus.io/scrape` at
  `/metrics` IS the Kubernetes integration; no operator code runs
  inside the pod on Kubernetes's behalf, which is the same seam
  discipline as `sslmode` adopting Postgres's own vocabulary
  (specs/tls.md) rather than inventing one.
- **Deployment is an artifact, not a protocol**: Terraform (or any
  IaC tool) does not talk to an application directly — it applies a
  resource (a container image reference, a manifest) that something
  ELSE then runs and probes. So this stack's job stops at producing
  a correctly-tagged OCI image and a manifest that names the health
  routes as probes; a Terraform stack that applies that manifest is
  the deploying team's own infrastructure, out of this repo's reach
  and out of scope here (filed: deploy-package, deploy-k8s).

## Behavior

- [ ] `Ops.health(store)` answers live/ready by calling `store.stats`;
      an exception is caught and reported as NOT ready, named
- [ ] `GET /healthz` is 200 while live, else 503 with the reason;
      `GET /readyz` likewise for ready — plain text, human-legible,
      the Kubernetes probe contract
- [ ] `GET /stats` answers `Store.Stats` as JSON (the Schema already
      derived; no new codec)
- [ ] `Prom.render` is PURE and pinned: a `Store.Stats` with two
      topics, several partitions, and (optionally) an `Offsets`
      lag reading becomes stable Prometheus text — HELP/TYPE once
      per metric name, one line per (topic, partition) label pair,
      a trailing newline (the format's own requirement)
- [ ] `GET /metrics` serves `Prom.render` at `text/plain;
      version=0.0.4` (the exposition format's own content-type)
- [ ] the demo (okay-demo) wires `Ops.routes` in as a consumer,
      proving the routes against a real Store over a real socket —
      the same acceptance move `TestChatDemo` already makes for
      every other route

## Out of scope

- a metrics PUSH gateway, a StatsD/OTLP-metrics exporter — this box
  is scrape-shaped (pull), matching Prometheus's own default and
  costing nothing until a push need is named
- a Kubernetes client library, CRDs, an operator — the manifest is
  the interop; nothing here talks to the Kubernetes API
- a Terraform provider — Terraform applies the artifacts this spec's
  sibling boxes produce (an image, a manifest); it does not need to
  know this stack exists
- authenticating `/metrics`/`/healthz` — a scrape endpoint is
  operator-internal traffic; if a deployment exposes it publicly,
  `Secure.bearer` already composes around any route
- per-consumer-group lag in `/metrics` v1 — `Offsets.lag` exists and
  is wired when an `Offsets` is passed to `Prom.render`, but no
  route resolves "which groups exist" automatically; a caller names
  its own groups (a store has no registry of them by design)

## Decisions

- **A mapping, not a dependency** — the same ruling obs.md already
  made for OTLP: Prometheus text and the Kubernetes probe contract
  are DOCUMENTED WIRES, so this module writes the string, never
  imports `io.prometheus.client` or a Kubernetes SDK. Rejected: a
  metrics library (the STATS-ARE-VALUES rule extended, not reopened).
- **Health calls the store live, never a cached flag** — a background
  poller updating a boolean is one more thing to keep correct under
  restart/reconnect races; `stats` is cheap (an in-memory read or a
  handful of file stats) and calling it IS the check.
- **Deployment artifacts are a separate box from the ops routes** —
  producing an image and a manifest is packaging, not observability;
  splitting them lets either evolve (a second deployable, a second
  manifest flavor) without touching `okay-ops`.

## Results
