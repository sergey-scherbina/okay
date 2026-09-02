# Deploying an Okay service

A reusable scaffold (specs/deploy.md), not one app's Dockerfile:
`sbt-assembly` for the jar, one ARG-parameterized Dockerfile, one
Helm chart. DemoChat (okay-demo) is the first concrete instance —
the scaffold itself works for any Okay service that meets three
conditions.

## Making a service deployable

1. Exactly one `object` with `def main` in the module.
2. Wire `okay.ops.Ops.routes(store)` into the module's own routes
   (specs/ops.md) — this is what makes the module probeable at all.
3. In `build.sbt`, on that module's project:
   ```scala
   .settings(
     assembly / mainClass := Some("your.package.YourMain"),
     assembly / assemblyJarName := "app.jar",
   )
   ```

That's the whole contract. `okay-demo`'s `okayDemo` project already
carries it — read its `build.sbt` entry as the worked example.

## Building

```sh
# the jar alone — proves packaging without needing Docker at all
sbt "okayDemo/assembly"
java -jar okay-demo/target/scala-3.7.4/app.jar

# the jar AND an image, if a Docker daemon answers (skips the image
# step and tells you so if it does not)
deploy/scripts/okay-package.sh okayDemo v1
```

`deploy/Dockerfile` is generic — `--build-arg MODULE=<sbt-project-id>`
selects which module's assembly jar becomes the image.

## Deploying

```sh
helm install demo-chat deploy/helm/okay-app \
  -f deploy/helm/okay-app/examples/demo-chat.values.yaml
```

The chart needs no edits for a second service — only its own
`values.yaml` (image, tag, port, env). Liveness and readiness probes
point at `/healthz`/`/readyz`; a `prometheus.io/scrape` annotation
points at `/metrics` — okay-ops's own routes, read directly by
Kubernetes and by a Prometheus server with zero code running on
either's behalf.

**Terraform** (or ArgoCD, or plain `helm install`) applies this same
chart through its `helm` provider (`helm_release`) — it never talks
to the running service; it only ensures the manifest this chart
renders is what the cluster has. Validate what would be applied
without a cluster at all: `helm lint deploy/helm/okay-app` and
`helm template deploy/helm/okay-app -f <values>`.
