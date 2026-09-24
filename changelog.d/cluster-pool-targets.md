## cluster-pool-targets - Need.Peers rendered across every deployment target

Stage 2 of specs/cluster-pool.md, the model half: `Need.Peers` in
specs/deployment.md's closed enum, and a rendering on every existing
target. `cluster` gets a headless Service (ready pods only) beside the
Deployment and `OKAY_POOL_SERVICE`; `laptop` gets `deploy.replicas`
and a bare container port (a fixed host port cannot be bound by more
than one replica); `host` gets N systemd units, each its own env file
with `OKAY_POOL_INSTANCE` and a peer-list template (this model has no
per-instance port field, so a guessed address is refused rather than
invented); `aws` gets a Cloud Map private DNS namespace and a
MULTIVALUE-routed service; `fly` gets `OKAY_POOL_SERVICE` pointed at
its own `<app>.internal`; `gcp`, `azure`, `render` and `railway` refuse
by name, each naming `cluster` as the target that works.

Gated with real tools: `helm lint`/`helm template` accept the headless
Service (`TestClusterHelm`), `terraform validate` against the AWS
provider's own schema accepts the Cloud Map rendering
(`TestCloudsTerraform`). 168 tests in okay-deploy's default suite,
clean compile.

Split off and filed separately: `cluster-pool-kind-harness` (real pods
on `kind`, `kubectl delete pod`/`kubectl scale` — needs an actual
`okay-pool` container image, a distinct piece of work) and
`cluster-pool-other-managers` (stage 3: nomad/yarn/slurm/swarm/batch).
