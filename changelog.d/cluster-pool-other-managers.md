## cluster-pool-other-managers - nomad, yarn, slurm, swarm and batch

Stage 3 of specs/cluster-pool.md: five more deployment targets, all in
`okay.deploy.Managers`. `nomad` and `yarn` render each platform's own
Services API JSON directly (no `nomad`/`yarn` binary needed — neither
format is HCL, and okay-codec's `Json` round-trips it in the default
suite, matching `railway.json`'s own precedent); `slurm` renders one
`sbatch` script for the one peers service a pool actually is; `swarm`
reuses `laptop`'s compose shape in replicated mode; `batch` renders an
AWS multi-node parallel job definition in Terraform.

Scoped narrower than `cluster` on purpose: `Need.Database`/`Need.Cache`
refuse the same way `host` already does, and a service carrying any
`secrets` is refused too — five more secret stories is a lane of its
own.

Gated with real tools (`Live`, `TestManagersLive`): `sh -n` for slurm,
`docker compose config` for swarm, `terraform validate` + `fmt -check`
for batch against the real AWS provider schema, and a word-boundary
grep proving no manager's name lives in okay-cluster's or okay-pool's
main sources (Claim 1). Two real defects found and fixed: AWS Batch's
`node_properties` needed `jsonencode(...)`, not a Terraform block or a
bare object; the grep test's own first run flagged "consult" as
"consul" until it moved to word boundaries. 183 tests in okay-deploy's
default suite, clean compile.

Filed: `cluster-pool-batch-full-mesh` (Batch gives a node its own
address and the total count, never a full peer list).

Adding five targets to `Targets.all` drifted okay-demo's and
okay-script's own committed renderings, which iterate that same roster
— both regenerated (`DemoDeploy`/`ScriptDeploy`'s own `main`) and
committed; `batch` refuses both, by name, for the same reason `aws`/
`gcp`/`azure`/`fly` already do (no `Need.Region`).
