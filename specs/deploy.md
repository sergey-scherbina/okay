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

- **A deployment is a VALUE.** `okay.deploy.Deploy(name, module,
  moduleDir, mainClass, port, image, env, replicas, resources,
  health, metricsPath, javaOpts)` — a case class with a Schema, like
  every other observable thing here. What an application says about
  itself, and nothing else.
- **Rendering is pure.** `Dockerfile.render`, `Helm.values`,
  `Compose.render` are `Deploy => String`, pinned by golden tests
  (the `Prom.render`/`Otlp.body` move). `Deploy.files(d)` is the whole
  deployment as data — inspectable before a byte is written;
  `Deploy.write` puts it under `<moduleDir>/deploy/`; `Deploy.drift`
  names every file there that no longer equals its rendering.
- **The module knows no application.** okay-deploy carries the
  generic Helm chart as resources (every knob a value in
  values.yaml), the renderers, and the build half
  (`okay-deploy/sbt-plugin`, a SOURCE sbt plugin the root
  `project/plugins.sbt` depends on; it brings sbt-assembly, and
  `OkayDeploy.deployable(mainClass)` is one line in a module's build
  entry). Nothing deploy-shaped lives at the repository root — the
  operator's rule: everything in okay-deploy or in the application. An application declares its `Deploy` in its OWN module,
  renders it with a one-line main, commits the result, and keeps one
  drift test — the committed deployment IS the rendered value, or
  the test says which file is not.
- **The wires stay the standard ones.** A Dockerfile (multi-stage,
  sbt image → slim JRE, non-root), a Helm chart (Terraform's `helm`
  provider, ArgoCD, plain `helm install`), a compose file for a
  laptop; probes and the Prometheus annotation point at okay-ops's
  routes by default (`Health()`), so a service that wires
  `Ops.routes` in is probeable with no chart change. Nothing talks to
  Kubernetes or Terraform directly — they apply what this renders.

## Behavior

- [x] `Deploy` renders: the Dockerfile builds ONE module's jar and
      runs it non-root, with no build arg left to decide; values.yaml
      carries every knob as a quoted scalar and the chart templates
      carry no application name at all; compose builds from the
      module's own Dockerfile with the repository as context — all
      pinned (okay-deploy's TestDeploy)
- [x] write-then-drift is empty; a hand edit and a missing file are
      each named by path
- [x] DemoChat is the worked instance: `DemoDeploy.spec` in okay-demo,
      `okay-demo/deploy/` committed as its rendering, `TestDemoDeploy`
      refusing drift; `sbt "okayDemo/assembly"` via
      `OkayDeploy.deployable` produces the jar the Dockerfile copies,
      and `java -jar app.jar` serves `/healthz` (proven without Docker)
- [x] `helm lint okay-demo/deploy/helm` passes and `helm template`
      renders probes and the Prometheus annotation at exactly
      okay-ops's paths, port 8090, the app's own env
- [x] `okay-deploy/bin/okay-package.sh <module> <dir> [image:tag]`
      builds the jar and, where a daemon answers, the image from the
      module's own Dockerfile; a second service needs only its own
      `Deploy` value — no file in okay-deploy changes

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

Reshaped 2026-09-02 (deploy-module, operator: "в самом okay-deploy не
было ничего жестко привязано к конкретному приложению"): the first
landing was a `deploy/` directory at the root whose default
values.yaml knew DemoChat's port variable and image name — an
application leaking into the "generic" template. Now okay-deploy is a
module with a `Deploy` VALUE and pure renderers; the application-
specific facts live only in `okay-demo/src/.../DemoDeploy.scala`, its
rendering is committed under `okay-demo/deploy/` and drift-tested,
and the chart templates are asserted to contain no application name.
One trap found: a forked `run` has the MODULE directory as cwd, so
`Deploy.repoRoot()` walks up to the nearest build.sbt before writing.
Docker/kubectl remain unproven live this session (no daemon, no
cluster); `java -jar`, `helm lint` and `helm template` are the
offline proof.
Second pass the same day (operator: nothing deploy-related at the
root): `docs/deploy.md` folded into the module docs, and the sbt
helper moved out of `project/` into `okay-deploy/sbt-plugin` as a
source plugin the root `plugins.sbt` merely points at — proven by a
clean `okayDemo/assembly` through it. One name clash to know: the
core project is `okay`, so the build entry says
`_root_.okay.deploy.sbt.OkayDeploy`.
