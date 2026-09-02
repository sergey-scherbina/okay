# Deploying an Okay service

The scaffold is a module — `okay-deploy` (docs/modules/okay-deploy.md,
specs/deploy.md) — and the deployment of a service is a VALUE that
service declares and renders into its own directory. Nothing in
okay-deploy names an application; DemoChat is the worked instance:

- `okay-demo/src/main/scala/okay/demo/DemoDeploy.scala` — the value
- `okay-demo/deploy/` — its rendering: `Dockerfile`, `compose.yaml`,
  `helm/` (chart + values); `TestDemoDeploy` refuses drift
- build.sbt: `.dependsOn(okayDeploy).settings(OkayDeploy.deployable("okay.demo.ChatDemo"))`

```sh
sbt "okayDemo/runMain okay.demo.DemoDeploy"        # (re)render okay-demo/deploy
okay-deploy/bin/okay-package.sh okayDemo okay-demo # jar (+ image if a daemon answers)
docker compose -f okay-demo/deploy/compose.yaml up # a laptop
helm install demo-chat okay-demo/deploy/helm       # a cluster; Terraform: helm_release on the same path
```

A second service repeats exactly this with its own value; the chart,
the Dockerfile shape and the script never change.
