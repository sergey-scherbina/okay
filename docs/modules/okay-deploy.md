# okay-deploy

> A deployment as a VALUE, rendered to the wires operators already run
> (specs/deploy.md): `Deploy(...)` plus pure renderers to a Dockerfile,
> a Helm chart's values and a compose file, with the generic chart
> riding as resources. This module knows no application; an
> application declares its own `Deploy` and OWNS the rendered files.

Depends on: `okay-codec` (a `Deploy` has a Schema — JSON-inspectable
like every other value here). Build half: `project/OkayDeploy.scala`
(`OkayDeploy.deployable(mainClass)` — sbt-assembly with a stable jar
name, one line in a module's build entry).

## Guide

**Declare** — in the application's own module (okay-demo's
`DemoDeploy` is the worked example):

```scala
object MyDeploy:
  val spec = Deploy(name = "my-svc", module = "okayMySvc", moduleDir = "okay-my-svc",
    mainClass = "okay.mysvc.Main", port = 8080, image = Image("okay/my-svc", "local"),
    env = Vector(Env("MY_PORT", "8080")))
  def main(args: Array[String]): Unit = Deploy.write(spec, Deploy.repoRoot())
```

and in build.sbt: `.dependsOn(okayDeploy).settings(OkayDeploy.deployable("okay.mysvc.Main"))`.

**Render** — `sbt "okayMySvc/runMain okay.mysvc.MyDeploy"` writes
`okay-my-svc/deploy/{Dockerfile, compose.yaml, helm/…}`. Commit them:
they are the deployment, readable without running anything.

**Keep them honest** — one test: `Deploy.drift(MyDeploy.spec,
Deploy.repoRoot()) == Vector.empty`. A hand edit, or a value changed
without regenerating, fails by file name.

**Build and run** — `okay-deploy/bin/okay-package.sh okayMySvc
okay-my-svc [image:tag]` (the jar always; the image when a Docker
daemon answers); `docker compose -f okay-my-svc/deploy/compose.yaml up`
for a laptop; `helm install my-svc okay-my-svc/deploy/helm` — or
Terraform's `helm` provider over that same chart — for a cluster.
Probes and the Prometheus annotation point at okay-ops's routes
(`Health` defaults), so a service that wires `Ops.routes` in is
probeable with no chart change.

| | |
|---|---|
| `Deploy`, `Image`, `Env`, `Resources`, `Health` | the value; `d.dir` = `<moduleDir>/deploy` |
| `Deploy.files(d)` | every rendered (path, content) — inspect before writing |
| `Deploy.write(d, root)` / `Deploy.drift(d, root)` | render to disk / name what differs |
| `Dockerfile.render`, `Helm.values`, `Compose.render` | the pure renderers |
| `Deploy.chartFiles` | the generic chart, verbatim from resources |
