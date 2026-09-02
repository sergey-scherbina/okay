# okay-deploy — deploying an Okay service

> A deployment is a VALUE (specs/deploy.md). An application declares
> `Deploy(...)` in its own module; okay-deploy renders that value into
> the wires operators already run — a Dockerfile, a Helm chart, a
> compose file — and a one-line test keeps the committed files equal
> to the value. okay-deploy itself knows no application.

Depends on: `okay-codec` (a `Deploy` has a Schema). Build half:
`okay-deploy/sbt-plugin`, a source sbt plugin the root
`project/plugins.sbt` points at (it brings sbt-assembly). Nothing
deploy-shaped lives at the repository root — everything is in
okay-deploy or in the application (the operator's rule).

Worked instance: DemoChat — `okay-demo/src/main/scala/okay/demo/DemoDeploy.scala`,
its rendering `okay-demo/deploy/`, its drift test `TestDemoDeploy`.

## Quick start: making a service deployable

Five steps, none of which touch okay-deploy.

**1. The service answers okay-ops's routes.** Probes and the
Prometheus scrape in the chart point at `/healthz`, `/readyz`,
`/metrics` (docs/modules/okay-ops.md) — compose `Ops.routes(store)`
into the service's route table:

```scala
case r if Ops.routes(store).isDefinedAt(r) => Ops.routes(store)(r)
```

**2. One `def main`** in the module — the class the jar runs.

**3. build.sbt — one line each** for the dependency and the jar:

```scala
lazy val okayMySvc = (project in file("okay-my-svc"))
  .dependsOn(/* ... */, okayDeploy)
  .settings(_root_.okay.deploy.sbt.OkayDeploy.deployable("okay.mysvc.Main"))
```

`_root_` is not optional: the core project is itself named `okay`, so
a bare `okay.deploy...` resolves to that project in build.sbt.
`deployable` sets sbt-assembly's main class, a stable jar name
(`app.jar`) and a merge strategy — that is all it does.

**4. Declare the value** in the service's own module, with a `main`
that renders it:

```scala
package okay.mysvc

import okay.deploy.{Deploy, Env, Image, Resources}

object MyDeploy:
  val spec: Deploy = Deploy(
    name      = "my-svc",            // the release/service name — a DNS label
    module    = "okayMySvc",         // the sbt project id
    moduleDir = "okay-my-svc",       // its directory under the repo root
    mainClass = "okay.mysvc.Main",
    port      = 8080,                // what the app LISTENS on
    image     = Image("okay/my-svc", "local"),
    env       = Vector(
      Env("MY_SVC_PORT", "8080"),    // however THIS app learns its port
      Env("MY_SVC_DATA", "/data")),
    resources = Some(Resources("100m", "256Mi", "1", "512Mi")))

  def main(args: Array[String]): Unit =
    Deploy.write(spec, Deploy.repoRoot()).foreach(p => println(s"wrote $p"))
```

**5. Render, commit, guard:**

```sh
sbt "okayMySvc/runMain okay.mysvc.MyDeploy"   # writes okay-my-svc/deploy/
git add okay-my-svc/deploy                      # the deployment, as files
```

```scala
class TestMyDeploy extends munit.FunSuite:
  test("okay-my-svc/deploy does not drift from MyDeploy.spec") {
    assertEquals(Deploy.drift(MyDeploy.spec, Deploy.repoRoot()), Vector.empty)
  }
```

From now on a change is: edit the value, re-run the `main`, commit.
A hand edit of a rendered file, or a value changed without
re-rendering, fails that test naming the file.

## The value

| field | meaning | default |
|---|---|---|
| `name` | release/service name; the Deployment, Service and compose service are named this | — |
| `module` | sbt project id — what `sbt "<module>/assembly"` builds inside the image | — |
| `moduleDir` | the module's directory; the jar is copied from `<moduleDir>/target/scala-*/app.jar`, files render to `<moduleDir>/deploy/` | — |
| `mainClass` | recorded in the value for inspection; the jar's main is set by `deployable(...)` in build.sbt | — |
| `port` | the port the app listens on: `containerPort`, the Service port, compose's published port, the probe port | — |
| `image` | `Image(repository, tag)` — `okay/my-svc:local` | tag `local` |
| `env` | `Vector[Env(name, value)]` — container env in the chart and in compose; this is how the app is told its port, directories, keys | empty |
| `replicas` | Deployment replicas | 1 |
| `resources` | `Some(Resources(cpuRequest, memoryRequest, cpuLimit, memoryLimit))` | none (`resources: {}`) |
| `health` | `Health(livenessPath, readinessPath)` — probe paths | okay-ops's `/healthz`, `/readyz` |
| `metricsPath` | `Some(path)` sets the `prometheus.io/scrape` annotation; `None` turns it off | `Some("/metrics")` |
| `javaOpts` | inserted into the image's `java` line (`-Xmx512m`, `-Dfoo=bar`) | empty |

`d.dir` is `<moduleDir>/deploy`. Every value is quoted when rendered
to YAML, so `":memory:"`, `"8080"` and `"true"` stay the strings they
are.

## What gets rendered

`Deploy.files(d)` is the whole deployment as `(path, content)` pairs
— look at it before writing anything. `Deploy.write(d, root)` puts
them under `root/<moduleDir>/deploy/`:

| file | what it is |
|---|---|
| `Dockerfile` | multi-stage: an sbt image runs `sbt "<module>/assembly"`, a slim JRE image (`eclipse-temurin:21-jre-alpine`) runs `app.jar` as a non-root user; `EXPOSE <port>`; `javaOpts` on the command line. No build args — the value decided everything |
| `compose.yaml` | one service, built from that Dockerfile with the repository as context, `port:port` published, the env inline — a laptop's deployment |
| `helm/values.yaml` | the ONLY application-specific file in the chart: image, tag, replicas, port, probe paths, metrics, env, resources |
| `helm/Chart.yaml`, `helm/templates/deployment.yaml`, `helm/templates/service.yaml` | the generic chart, copied verbatim from okay-deploy's resources — every knob is a value; a test asserts the templates name no application |

The rendered chart is a complete, self-contained Helm chart: a
cluster (or Terraform) needs only that directory, never okay-deploy.

## Building and running

```sh
# the jar — the actual hard part (classpath, merge conflicts, one
# main); needs no Docker at all
sbt "okayMySvc/assembly"
java -jar okay-my-svc/target/scala-3.7.4/app.jar

# jar + image, from the module's OWN rendered Dockerfile; says so and
# stops after the jar when no Docker daemon answers
okay-deploy/bin/okay-package.sh okayMySvc okay-my-svc okay/my-svc:v1

# a laptop
docker compose -f okay-my-svc/deploy/compose.yaml up

# a cluster
helm lint okay-my-svc/deploy/helm
helm template my-svc okay-my-svc/deploy/helm          # inspect what would be applied
helm install my-svc okay-my-svc/deploy/helm
```

**Terraform** applies the same chart through the `helm` provider —
it never talks to the running service:

```hcl
resource "helm_release" "my_svc" {
  name  = "my-svc"
  chart = "${path.module}/../okay-my-svc/deploy/helm"   # the rendered chart, as committed

  set {
    name  = "image.tag"
    value = var.image_tag              # the CI-built tag; everything else is the value's
  }
}
```

Any value in `values.yaml` can be overridden the same way (`set` /
`values = [file(...)]`) without touching the rendered files — the
committed values stay the application's stated defaults, the
environment's overrides stay in the environment's Terraform.

## Monitoring, once deployed

Nothing to add: the chart's probes already hit `/healthz`/`/readyz`,
and the pod carries `prometheus.io/scrape: "true"`,
`prometheus.io/port`, `prometheus.io/path: "/metrics"` — a Prometheus
server with the usual Kubernetes service discovery, or an OTEL
Collector's prometheus receiver, scrapes it as-is. `GET /stats` is the
same numbers as JSON for a human. Tracing is okay-obs's business
(docs/modules/okay-obs.md) and independent of this.

## Changing things

- **A knob the value has** (port, env, replicas, resources, probe
  paths, java opts, image): edit the value, re-run the `main`,
  commit. The drift test is the reminder.
- **Something the chart cannot say yet** (a volume, an Ingress, a
  second container): that is a chart change, and the chart is
  okay-deploy's — extend the template under
  `okay-deploy/src/main/resources/okay/deploy/chart/`, expose the knob
  on `Deploy` and in `Helm.values`, pin it in `TestDeploy`, then every
  service re-renders and gets it. Editing the copy under
  `<moduleDir>/deploy/helm/` instead fails that service's drift test
  by design: the rendered chart is output, not source.
- **Environment-specific overrides** (a production tag, a bigger
  memory limit): Helm `--set`/values files, or Terraform `set` —
  layered over the committed defaults, never written into them.

## Things that bite

- `_root_.okay.deploy.sbt.OkayDeploy` in build.sbt — the core project
  is named `okay`.
- A forked `run` has the MODULE directory as its working directory,
  not the repository root; `Deploy.repoRoot()` walks up to the nearest
  `build.sbt`, so use it (as the example does) rather than
  `Path.of(".")`.
- `okay-package.sh` and the Dockerfile both expect the jar at
  `<moduleDir>/target/scala-*/app.jar` — that name comes from
  `deployable(...)`; a module that sets its own `assemblyJarName`
  breaks the contract.
- A brand-new module's first `sbt "<module>/assembly"` may surface
  duplicate-resource conflicts from a new dependency set; the merge
  strategy in `deployable` (services concat, other `META-INF`
  discard, first-wins otherwise) has covered every module so far —
  if it does not, extend it there, once, for everyone.

## API

| | |
|---|---|
| `Deploy`, `Image`, `Env`, `Resources`, `Health` | the value; `Deploy.dir` = `<moduleDir>/deploy` |
| `Deploy.files(d)` | every rendered `(path, content)` |
| `Deploy.write(d, root)` | render under `root/d.dir`; answers the paths written |
| `Deploy.drift(d, root)` | the rendered files that differ or are missing — empty is the goal |
| `Deploy.repoRoot(from = cwd)` | the nearest ancestor holding a `build.sbt` |
| `Dockerfile.render`, `Helm.values`, `Compose.render` | the pure renderers, `Deploy => String` |
| `Deploy.chartFiles` | the generic chart's file names, served from resources |
| `okay.deploy.sbt.OkayDeploy.deployable(mainClass)` | the build-side settings (sbt plugin) |
| `okay-deploy/bin/okay-package.sh <module> <dir> [image:tag]` | jar, then image if a daemon answers |
