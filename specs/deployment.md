# Deployment: one value, every place it runs

## Overview

specs/deploy.md made a deployment a VALUE and its artifacts a pure
rendering of that value — a Dockerfile, a compose file, a Helm chart,
with a drift test making the committed files equal to the rendering.
That was one application on one shape.

The operator's ask (2026-09-07) is the general case, and it names the
places: a laptop, a rented server, a Kubernetes cluster, a PaaS
(fly.io, Render, Railway) and the three clouds (AWS, Azure, Google).
Same application, same declaration, and — the phrase that sets the
bar — "работает как часы": nothing hand-edited per place, nothing
that drifts, and the failure of a place said in its own words before
anything is applied.

Two more answers came with the ask, both taken as decisions:

- **The model describes what an application NEEDS**, not just the
  process — a database, a volume, a name, a certificate, a neighbour.
  I argued for a closed list of five; the operator chose the full
  dependency graph. It is their call and it is written that way here.
  What that costs is stated in "The line this model does not cross",
  and the escape hatch is stated with it, because the danger of a
  general dependency model is that it slowly becomes a worse Helm.
- **Configuration is a value with defaults, and a file over it** —
  and, at the end of the chain, the environment over the file.
- **Secrets reach cloud managers and SOPS**, not only `env:` and
  `file:`.

## Why this is not "just Helm" (and not just Terraform)

Both exist, both are adopted rather than reinvented — this spec
RENDERS a Helm chart and RENDERS Terraform. What neither gives is the
thing being asked for: one declaration that is simultaneously the
laptop's compose file, the server's unit, the cluster's chart and the
cloud's plan, kept in step by a test. Today an application says its
port in five files and its database URL in four; the point of a value
is that it says each once.

The rule that keeps this honest, inherited from specs/deploy.md and
sharpened by the operator's own framing (2026-09-07): **a declarative
layer over what already works in each place, orchestrating the TOOLS
and never becoming the RUNTIME.**

The distinction is worth its paragraph, because "no orchestration"
would be the wrong promise. We DO orchestrate: render, build, then
apply, in dependency order (a database before the service that needs
it), one command, with the target's prerequisites checked before
anything runs and a failure named in that target's own words. What we
refuse to be is the thing that keeps a workload ALIVE — no agent, no
control plane, no scheduler, no state of our own. `docker compose`,
`systemctl`, `helm`, `flyctl` and `terraform` are each excellent at
exactly that, they are already installed, and an operator already
trusts them.

So: an orchestrator of tools, never of workloads. The practical test
of the line is that everything we produce can be applied BY HAND with
the target's own commands, and everything we do can be tested as a
pure function from a value to files — which is what makes "every
place" affordable at all.

## The module

**`okay-deploy`**, which already exists and already means this. The
new model grows inside it rather than beside it: a second module for
the same domain would be exactly the drift this repository's own rule
warns about — two names for one thing — and "deploy" is the word the
domain has. `specs/deploy.md` stays as the history of the
single-service scaffold this supersedes; `okay.deploy` stays the
package.

Rejected: `okay-ship`, `okay-anywhere`, `okay-manifest` — a new name
for the same subject, which buys nothing and costs everyone the
question of which one to read. Also rejected, for now: splitting the
cloud renderers into `okay-deploy-aws` and friends. They are pure
string builders with no dependency of their own, so nothing is saved
by the split and one import is lost.

## The model

```scala
package okay.deploy

/** everything one deployable system is: its services, and what they
 *  need. One value per application, in the application's own module. */
final case class Deployment(
  name: String,
  services: Vector[Service],
  /** files a target should carry that the model cannot express --
   *  the escape hatch, per target, deliberately ugly to reach for */
  extra: Map[Target.Name, Vector[(String, String)]] = Map.empty,
)

final case class Service(
  name: String,
  run: Run,                       // what starts it
  ports: Vector[Port] = Vector.empty,
  settings: Settings = Settings.empty,   // typed configuration, below
  secrets: Vector[Secret] = Vector.empty,// REFERENCES, never values
  needs: Vector[Need] = Vector.empty,
  health: Health = Health(),
  scale: Scale = Scale(),
  resources: Option[Resources] = None,
)

/** what a service IS, in the two forms every target understands */
enum Run:
  /** built from this repository: a module, its main class, its jar */
  case Module(module: String, moduleDir: String, mainClass: String, javaOpts: String = "")
  /** already an image somewhere */
  case Image(repository: String, tag: String = "latest")

enum Need:
  case Volume(path: String, size: String = "1Gi", name: String = "data")
  case Database(engine: Engine, version: String, database: String, as: String = "db")
  case Cache(engine: Engine, version: String, as: String = "cache")
  case Dns(host: String)                       // the name this answers on
  case Tls(mode: TlsMode)                      // who terminates, and how
  case Neighbour(service: String)              // another Service in this Deployment
  case Port(number: Int, public: Boolean)      // reachable from outside, or not

enum Engine: case Postgres, Redis, Mongo, Kafka
enum TlsMode: case None, SelfSigned, Files, Acme, Proxy
```

Nothing above is a target's vocabulary. `Need.Database(Postgres)` is
a compose service on a laptop, a StatefulSet or a `postgresql` chart
dependency in a cluster, RDS in AWS, Cloud SQL in Google — the value
says WHAT, each target says HOW, and the difference between those two
is the whole reason this file exists.

## Configuration: one declaration, four places

An application's settings are a typed value today only inside its own
process; by the time they reach a compose file they are strings
someone typed twice. `Settings` closes that:

```scala
final case class Setting(name: String, value: String, doc: String = "")
final case class Settings(all: Vector[Setting]):
  def env: Vector[(String, String)]     // OKAY_PAGES=…, the shape a process reads

object Settings:
  /** from a Schema'd config value: field names become environment
   *  names, the value's own fields become the defaults */
  def of[A: Schema](a: A, prefix: String): Settings
```

The chain at runtime, in this order and no other: **the value's
defaults, then a file, then the environment.** Defaults are what the
code says; a file is what this installation says; the environment is
what this run says. Every layer is optional and every layer is
readable — `Conf.load[A]` already reads the file, `Settings.of`
already knows the names, and the same list renders into compose's
`environment`, a Kubernetes ConfigMap, a systemd `EnvironmentFile`
and a Terraform variable block.

The immediate debt this pays: `okay.script.Serve` grew fourteen
`OKAY_*` variables read by hand, none typed, none checked together.
It becomes a `Schema`'d case class, and the names come out of the
schema rather than out of two lists that have to agree.

## Secrets

`Secret` stays what specs/conf.md made it: a REFERENCE that is safe
to commit, print and store. What grows is where a reference can point
and how a target wires it:

| scheme | resolved by | rendered by a target as |
|---|---|---|
| `env:NAME` | the process's environment | compose `environment`, k8s `envFrom`, systemd `EnvironmentFile` |
| `file:/path` | a mounted file | a compose/k8s secret mount, a `0400` file for systemd |
| `sops:path#key` | `sops`/age at the edge | the encrypted file rides in git; the target mounts it and the app decrypts |
| `aws-sm:name` | AWS Secrets Manager | an IAM policy stub and the ARN; no value in any file |
| `gcp-sm:project/name` | Google Secret Manager | the secret's resource name and an accessor binding |
| `azure-kv:vault/name` | Azure Key Vault | the vault URI and an access policy stub |

Three rules hold across all of them, and they are what makes this
safe rather than convenient:

1. **No target ever renders a secret VALUE.** It renders the
   plumbing that lets the process resolve one. A rendered file that
   contained a password would be a rendered file in git.
2. **Resolution stays at the edge** — `Secrets.get` at handler
   construction, exactly as today. Nothing about a cloud manager
   changes where the value lives (in the narrow gap between resolve
   and constructor) or how long (as briefly as possible).
3. **A miss names the reference and what was tried.** `aws-sm:prod/db`
   with no credentials must say that, not "authentication failed".

SOPS is the one that needs its own sentence, because it inverts the
usual rule: the encrypted value IS committed. That is the point of it
for a small team, and it is safe exactly as far as the key
distribution is — which the repository can say nothing about. So
`sops:` is supported and documented with that caveat rather than
recommended.

## Targets

A target is a pure function and a driver, in that order of
importance:

```scala
trait Target:
  def name: Target.Name
  /** the whole deployment as files, relative to <module>/deploy/<name>/ */
  def render(d: Deployment): Vector[(String, String)]
  /** what this target needs on the machine to be applied at all */
  def requires: Vector[Tool]        // docker, helm, kubectl, flyctl, terraform…
  /** the command that applies what was rendered, and the one that
   *  takes it down -- we SHELL OUT, we do not reimplement */
  def up(dir: Path): Command
  def down(dir: Path): Command
```

| target | renders | applied by |
|---|---|---|
| `laptop` | `compose.yaml` (every service and need as a container), `.env.example` | `docker compose up` |
| `host` | a systemd unit per service, `EnvironmentFile`, an install script | `systemctl` |
| `cluster` | a Helm chart: Deployment, Service, ConfigMap, Secret stubs, PVC, Ingress with TLS | `helm upgrade --install` |
| `fly` / `render` / `railway` | the platform's own manifest plus the Dockerfile | `flyctl deploy`, a git push, `railway up` |
| `aws` | Terraform: ECS/Fargate service, ALB, RDS, EFS, Secrets Manager | `terraform apply` |
| `gcp` | Terraform: Cloud Run service, Cloud SQL, Secret Manager | `terraform apply` |
| `azure` | Terraform: Container Apps, Postgres Flexible Server, Key Vault | `terraform apply` |

The clouds render Terraform rather than calling APIs, and that is a
decision with a reason: an HCL file is a value we can test with
`terraform validate` and a golden test on a machine with no account,
while an API client is a thing that can only be tested by having one.
It also puts the deployment where the team's own review lives.

## One command, and one that says what is missing

```
sbt "okayDeploy/runMain okay.deploy.Up laptop"     # render, then apply
sbt "okayDeploy/runMain okay.deploy.Down laptop"
sbt "okayDeploy/runMain okay.deploy.Doctor fly"    # what this machine lacks
```

`Doctor` is not decoration. Half of "it doesn't work" in deployment
is a missing binary, an expired login or a kubectl context pointing
somewhere else, and each of those has an exact question: is `flyctl`
on the PATH, does `flyctl auth whoami` answer, which cluster is
`kubectl config current-context`. A target that lists its `requires`
gets that for free, and the answer is a sentence naming the fix.

## The line this model does not cross

The operator chose a full dependency model over my closed list of
five. It is the right call for the ask and the wrong shape to leave
unbounded, so the boundary is here, written down, and a change to it
is a change to this file:

- **`Need` is a closed enum.** Adding a kind is a spec edit and a
  renderer in every target, deliberately — that cost is the brake.
- **No conditionals, no templating language, no expressions.** A
  `Deployment` is data. If a value must differ per target, the
  application computes it in Scala before building the value.
- **No target-specific fields on the model.** No `k8sAnnotations`,
  no `flyRegions`. Those go in `extra` — a target-keyed map of raw
  files that the renderer merges verbatim. It is deliberately ugly:
  an application reaching for it often is telling us the model is
  wrong, and that is a signal worth keeping visible.
- **We do not provision accounts, DNS zones, or clusters.** We render
  what runs inside one that exists.
- **We do not manage state.** No terraform state backend opinion, no
  drift detection against a live cloud — `Deploy.drift` compares the
  committed rendering to the value, and nothing else.

## Staging

The whole thing at once is not landable, and pretending otherwise is
how it would arrive half-tested. Each stage is a claim of its own,
and each ends with something an operator can actually use:

- **Stage 0 — the model, the settings, and the two places that need
  no account.** `Deployment`/`Service`/`Need`/`Settings`, the
  `laptop` and `host` targets, `Up`/`Down`/`Doctor`, and okay-script's
  fourteen variables re-expressed as a `Schema`'d config. Proven by a
  real `docker compose up` in a Live test and a rendered unit that
  `systemd-analyze verify` accepts.
- **Stage 1 — cluster.** The Helm chart grown to ConfigMap, Secret
  stubs, PVC and Ingress. Proven by `helm template` and `helm lint`
  in the default gate, and optionally by kind in a Live test.
- **Stage 2 — PaaS.** fly/render/railway manifests, golden-tested; a
  real deploy needs an account and stays a documented manual step.
- **Stage 3 — the clouds.** Terraform per cloud, proven by
  `terraform validate` in a container. The AWS one first, because ECS
  plus RDS plus Secrets Manager exercises every part of the model.
- **Stage 4 — the secret schemes.** `sops:`, then the three managers,
  each shape-tested and Live-tested only where a credential exists.

## Behavior (stage 0)

- [ ] one `Deployment` renders `laptop` and `host` from the same
      value, and the port, the image and the database URL appear in
      each rendered file exactly once, from one field.
- [ ] `Settings.of[A]` derives the environment names from the schema;
      the runtime reads defaults, then a file, then the environment,
      and a test pins that order by making all three disagree.
- [ ] a `Need.Database(Postgres)` becomes a compose service with a
      volume on `laptop`, and on `host` a named refusal — a rented
      box's Postgres is not ours to install — which is the model
      being honest rather than the renderer guessing.
- [ ] no rendered file contains a secret value, asserted by a test
      that greps every rendering for the resolved fixtures.
- [ ] `Doctor laptop` names a missing `docker` in a sentence with the
      fix in it; `Up laptop` on the example store answers a page over
      HTTP after `docker compose up`.
- [ ] `Deploy.drift` still holds: the committed rendering equals the
      value, per file.

## Decisions

- **Orchestrate the tools, never the workloads.** The operator's own
  framing, and the reason "every place" is affordable: rendering is a
  pure function testable without an account, applying is somebody
  else's excellent program. A control plane of ours would be a third
  thing to operate and the first thing to page someone at night.
- **One module, and it is `okay-deploy`.** Rejected: a new name for
  the same domain.
- **Terraform for the clouds, not SDK calls.** A file the team
  reviews and `terraform validate` checks, over an API client only an
  account can exercise.
- **The escape hatch is per target and raw.** Rejected: a
  general-purpose overlay/patch language, which is how a deployment
  model becomes a build system.
- **Settings from a Schema, not a naming convention.** Rejected: a
  second list of environment names to keep in step with the config —
  that is the drift okay-script already has.
- **`Need` is closed and its growth is a spec edit.** Rejected: an
  open `Need.Custom(String, Json)`, which is `extra` with a nicer name
  and no renderer.
