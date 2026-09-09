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

## Graceful shutdown (service-lifecycle, 2026-09-09)

The microservices audit found no server here that DRAINS: a
`Resource` release calls `stop()` and every request in flight is
cut, and Kubernetes does that at every rollout. The standard
sequence is fixed by how Kubernetes works, not by taste:

1. **Readiness flips first.** `/readyz` answers 503 the moment a
   stop is asked for, while `/healthz` stays 200 — a pod that goes
   un-live is RESTARTED, a pod that goes un-ready is taken out of
   the Service's endpoints, which is what we want.
2. **A readiness delay.** Endpoint removal propagates in seconds;
   during that window new requests still arrive. They are answered
   503 with `Connection: close`, so a client retries elsewhere.
3. **In-flight requests finish**, up to a grace period.
4. **The server stops** — the region's release, unchanged.

`Lifecycle` is the value that carries it: a draining flag, an
in-flight count, `route(routes)` that counts and refuses, `drain
(grace)` that waits, and stats with a Schema. `Ops.routes(store,
lifecycle = Some(l))` makes `/readyz` read it. On the JVM
`Lifecycle.awaitSignal(l, readinessDelay, grace)` blocks the main
thread until SIGTERM/SIGINT (a shutdown hook), runs steps 1–3, and
returns so the region can run step 4 — the demo's
`Thread.sleep(Long.MaxValue)` becomes exactly that call. The
shutdown hook joins the main thread, so the JVM does not exit
before the release has run.

## RED metrics (service-lifecycle, 2026-09-09)

`/metrics` knew the store and, since resilience-http, the guards —
but not the REQUESTS: no rate, no errors, no duration per route,
and nothing for outbound clients. `Red(name)` is a value keeping,
per label, requests by status class, errors (a 5xx or a throw) and
a duration histogram in Prometheus's own cumulative-bucket shape;
`red.route(label)(routes)` wraps a server's routes, `red.http(label)
(inner)` an outbound `Http`, and `Prom.red(rs)` renders
`okay_http_requests_total{name,route,class}`,
`okay_http_errors_total` and
`okay_http_request_duration_seconds_{bucket,sum,count}` — a
histogram a `histogram_quantile` reads directly. The clock is
injected, as everywhere in this stack, so a duration bucket is
testable without sleeping.

## Behavior

- [x] `Ops.health(store)` answers live/ready by calling `store.stats`;
      an exception is caught and reported as NOT ready, named
- [x] `GET /healthz` is 200 while live, else 503 with the reason;
      `GET /readyz` likewise for ready — plain text, human-legible,
      the Kubernetes probe contract
- [x] `GET /stats` answers `Store.Stats` as JSON (the Schema already
      derived; no new codec)
- [x] `Prom.render` is PURE and pinned: a `Store.Stats` with two
      topics, several partitions, and (optionally) an `Offsets`
      lag reading becomes stable Prometheus text — HELP/TYPE once
      per metric name, one line per (topic, partition) label pair,
      a trailing newline (the format's own requirement)
- [x] `GET /metrics` serves `Prom.render` at `text/plain;
      version=0.0.4` (the exposition format's own content-type)
- [x] the demo (okay-demo) wires `Ops.routes` in as a consumer,
      proving the routes against a real Store over a real socket —
      the same acceptance move `TestChatDemo` already makes for
      every other route

Graceful shutdown:

- [x] `Lifecycle.route` counts requests in flight (up on entry, down
      on every exit — an answer, a throw) and, once draining, answers
      new ones 503 with `Connection: close` without running them
- [x] `/readyz` is 503 `ready=false (draining)` once draining while
      `/healthz` stays 200; before draining both are as before
- [x] `drain(grace)` answers true once in-flight reaches zero and
      false when the grace runs out with requests still in flight;
      a request that completes during the drain is the wake-up
- [x] `awaitSignal` (JVM): a SIGTERM delivered to the process runs
      readiness-off → delay → drain and returns to the caller, and
      the JVM waits for the caller's release before exiting (tested
      in-process by firing the hook's own action, not by killing the
      test JVM)
- [x] the demo holds its region open with `awaitSignal` instead of
      an infinite sleep, and `/readyz` reads the lifecycle

RED metrics:

- [x] `red.route(label)` counts a 200 as a request in class `2xx`, a
      404 in `4xx`, a 503 as a request AND an error, a throw as an
      error with class `exception` — and the throw still propagates
- [x] durations land in the right cumulative buckets under an
      injected clock; `sum` and `count` agree with the requests
- [x] `red.http(label)` does the same around an outbound client,
      a dropped wire counted as an error
- [x] `Prom.red` renders the Prometheus histogram shape: `_bucket`
      rows with `le`, cumulative, a `+Inf` row equal to `_count`,
      then `_sum` and `_count`; `Ops.routes(..., red = Vector(r))`
      serves them at `/metrics`

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

- **Draining is a route wrapper, not a server feature** — the three
  servers here (JDK, Jetty, Netty) all take a `PartialFunction
  [Request, Response ! Async]`, so one wrapper serves all three and
  a fourth; a per-server hook would be written three times and
  differ. Rejected: `server.stop(gracePeriod)` (Jetty has one, the
  JDK server has a crude one, Netty none — and none of them flips
  readiness first, which is the step that matters).
- **The drain waits by polling, 20 ms** — a shutdown path runs once
  per process; waking waiters from the in-flight counter is code
  that would exist only for it. Revisit if a drain ever needs to be
  precise to the millisecond.
- **RED, not a metrics library** — the same ruling as the rest of
  this spec: a value with a Schema and a pure rendering. Buckets are
  fixed (5 ms … 10 s, the usual HTTP set) rather than configurable:
  a histogram whose buckets differ per service cannot be aggregated
  across them, which is the one thing a histogram is for.

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

**service-lifecycle (2026-09-09).** `Lifecycle` (shared), `Signals`
(JVM: the shutdown hook that wakes `awaitSignal` and joins the main
thread), `Red` with `Series`/`Count` values and `Prom.red` in the
histogram shape, `Ops.routes(..., lifecycle, red)`. 15 tests in
okay-ops (6 shared, cross-platform; 2 JVM for the stop sequence,
fired in-process). The demo's `main` now drains on SIGTERM instead
of sleeping for ever, its `/readyz` reads the lifecycle and every
route is measured. Not done here: the rendered Kubernetes manifest
does not yet set `terminationGracePeriodSeconds` (BACKLOG
deploy-termination-grace) — the drift test on the committed
rendering makes that its own small lane.


Landed 2026-09-02 (ops-monitoring): a new JVM/JS module `okay-ops`
(the Native leg was not built — its routes need `okay-http`, which
has no Native leg). `Health.of(store)` calls `store.stats` live,
catching a throw as `live=false, ready=false` with the exception's
message as the reason. `Prom.render` is a pure `Store.Stats =>
String` (Prometheus text 0.0.4), pinned by a golden-string test, plus
an opt-in `lagOf` for `Offsets.lag` per named consumer group. `Ops
.routes` composes the four routes into any `PartialFunction[Request,
Response ! Async]`, proven over a real socket (`okay-jetty`, TEST
scope) and wired into the demo (`okay-demo`) as the first consumer —
`chatLog`'s underlying `Store` is now exposed as `chatStore` so both
can see it. One MemoryStore lesson found while pinning the golden
text: its `PartitionStats.segments` field reports RECORD count, not
segment count — an engine-specific reading of a field the Store
trait leaves engines free to interpret, now stated in the test.
