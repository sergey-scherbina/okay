# Deploy: a reusable scaffold, not one app's Dockerfile

## Overview

specs/ops.md gave any Okay server the wires operators already read
(Prometheus, Kubernetes probes) without an SDK. This spec is the
other half the operator asked for (2026-09-02): a way to BUILD and
SHIP that server — and, explicitly, a template any FUTURE Okay
application can adopt, not a one-off recipe for the chat demo.
DemoChat (okay-demo, the one module in this repo with a real `main`
and `okay-ops` already wired in) is the first concrete instance the
scaffold is proven against, not the thing the scaffold is FOR.

## The model

- **Packaging is a plugin, not a script.** `sbt-assembly` is the
  standard, minimal, single-purpose tool for "one fat jar" on the
  JVM — adopted rather than hand-rolling a classpath-copy step,
  the same "adopt the standard tool" reasoning as everywhere else
  in this stack. Each deployable module opts in with two settings
  (`assembly / mainClass`, a stable `assemblyJarName`); nothing
  else in the build changes.
- **The image is a template, parameterized by ONE thing.**
  `deploy/Dockerfile` takes a single build arg, `MODULE` (the sbt
  project id) — multi-stage: an sbt image builds `$MODULE/assembly`,
  a slim JRE image runs the resulting jar. No per-app Dockerfile to
  maintain; a new Okay service adds two build.sbt lines and reuses
  this file unchanged.
- **The manifest is a chart, not a fork.** `deploy/helm/okay-app/`
  is ONE Helm chart with `values.yaml` as the parameterization
  surface (image, tag, port, env, resources) — Terraform's `helm`
  provider (or plain `helm install -f values.yaml`) applies it
  unchanged for any Okay service. Probes point at `/healthz`/
  `/readyz`; a `prometheus.io/scrape` annotation points at
  `/metrics` — okay-ops's OWN contract, so any service that wires
  `Ops.routes` in is deployable by this chart with zero chart
  changes.
- **A convention, stated, not enforced in code**: a service is
  "scaffold-ready" when it (1) has exactly one `object` with `def
  main` in its deployable module, (2) wires `okay.ops.Ops.routes`
  into its own routes, (3) sets `assembly/mainClass` and
  `assemblyJarName` for that module. Nothing CHECKS this — it is
  the same kind of convention `okay-http`'s `Response` shape or
  `okay-persist`'s `Store` trait already are: a shape a new module
  fits into, not a framework it inherits from.

## Behavior

- [x] `sbt "okayDemo/assembly"` produces one runnable jar; `java
      -jar app.jar` serves the same routes the `sbt run` main does,
      `/healthz` included — proven WITHOUT Docker, since the jar is
      the actual hard part (classpath, merge conflicts, one main)
- [x] `deploy/Dockerfile --build-arg MODULE=okayDemo` builds an
      image whose `ENTRYPOINT` runs that jar; the container answers
      `/healthz` on its exposed port (proven when a Docker daemon
      is available; the jar-level proof above is what runs when it
      is not — as it was not for this box's own landing, 2026-09-02,
      operator: "я остановил докер... чтобы память освободить")
- [x] `deploy/scripts/okay-package.sh <module> [tag]` wraps the
      build (and, if a daemon answers, an image build) into one
      command — the utility the operator asked for
- [x] `helm lint deploy/helm/okay-app` passes; `helm template
      deploy/helm/okay-app -f deploy/helm/okay-app/examples/demo-
      chat.values.yaml` renders a Deployment whose probes and
      Prometheus annotations name `/healthz`, `/readyz`, `/metrics`
      exactly as okay-ops answers them
- [x] the chart's `values.yaml` needs no chart edits to point at a
      SECOND Okay service — only its own values file

## Out of scope

- a CI pipeline that runs any of this on a schedule/push — this
  spec ships the artifacts a pipeline would call, not the pipeline
- a Terraform MODULE/provider — Terraform applies what this spec
  produces (an image reference, `helm_release`) through providers
  that already exist; writing Terraform HCL for a SPECIFIC cloud
  account is the deploying team's own infrastructure, not this
  repo's
- a release/deploy AUDIT LOG (an ops-topic record of "who shipped
  what, when") — named by the operator's original ask, genuinely
  useful, and deliberately NOT built here to keep this box
  reviewable; filed as deploy-log
- multi-arch images, a registry push step, image signing — every
  real CI adds these per its own registry/policy; the Dockerfile
  and script stay a local-build starting point

## Decisions

- **One Dockerfile, ARG-selected** — a Dockerfile per app is a
  maintenance N, and the ARG costs one line at build time.
  Rejected: sbt-native-packager's Docker plugin (a much larger
  surface — universal/rpm/deb formats this stack has no use for —
  for what a 20-line Dockerfile already does honestly).
- **One Helm chart, values-selected** — the standard IaC-facing
  shape; Terraform, ArgoCD, plain `helm install` all read the same
  chart. Rejected: raw YAML per app (no parameterization, a fork
  for every service); a custom templating script (Helm already IS
  that tool, adopted rather than reinvented — the sslmode/OTLP
  ruling again).
- **The convention over a build-time check** — a "scaffold-ready"
  service follows three stated rules (Ops.routes wired, one main,
  two build.sbt lines); nothing in this repo enforces them, the
  same trust the `Sql`/`Store`/`Response` shapes already extend to
  every implementer.

## Results

Landed 2026-09-02 (deploy-package). `sbt "okayDemo/assembly"`
produces `okay-demo/target/scala-3.7.4/app.jar` (32MB, one merge
strategy: services concat, other META-INF and module-info discard,
everything else first-wins — no conflicts against this module's
actual dependency set); `java -jar app.jar` served `/`, `/healthz`,
`/readyz`, `/stats`, `/metrics` all 200, proving the jar the
Dockerfile's build stage produces is the real artifact, independent
of whether a daemon exists to containerize it. The Docker image
itself and a live `kubectl apply` were NOT proven live — no Docker
daemon was reachable during this landing (the operator had stopped
it to free memory) and no Kubernetes cluster was reachable at all;
`helm lint` (clean) and `helm template` (rendered, inspected: the
probes and Prometheus annotations name exactly the paths okay-ops
answers) are the honest offline substitute, same shape as every
"skips where the endpoint is absent" live test elsewhere in this
stack. Re-run `deploy/scripts/okay-package.sh okayDemo` once a
daemon is back to close that gap.
