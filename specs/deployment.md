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

## The CLI, and the deployment as data

An operator's entry point is a command, not `sbt "okayDeploy/runMain
okay.deploy.Up laptop"` — which is what this module had, and which is
unusable on the machine that matters most: a server with the
artifacts on it and no repository, no sbt and no source.

```
okay deploy render <target>      # the value -> files, and say what changed
okay deploy doctor [target]      # the clean-machine report; --install, --json
okay deploy up <target>          # check, render if stale, apply
okay deploy down <target>
okay deploy diff <target>        # committed files vs what the value renders
okay deploy targets              # what this deployment can be applied to
```

**The decision this forces, and it is the important one: the CLI
reads the deployment as DATA.** `render` writes `deployment.json` —
the `Deployment` value through its own `Schema` — beside the target's
files, and every later verb reads that rather than evaluating Scala.
Three things follow, and each is worth more than the CLI itself:

- the artifacts directory is SELF-CONTAINED: copy `deploy/` to a
  server and `okay deploy up host` works there, with no repository,
  no sbt and no compiler;
- the CLI has no dependency on any application's code, so one binary
  serves every deployment;
- what the CLI knows is exactly what the spec says a deployment is —
  if the JSON cannot express something, the model cannot either, and
  that is a useful pressure to keep on the model.

Rendering stays where it belongs, in the build (`sbt
"<app>/runMain <app>.Deploy"`, the shape okay-script already uses),
because rendering is the one step that genuinely needs the Scala
value. `okay deploy render` is the same rendering re-run from the
JSON when only a target's files are stale — and `diff` is the drift
test an operator can run by hand.

### What it is, and how it gets onto a machine

A fat jar and a small wrapper script, committed as
`okay-deploy/bin/okay`. That makes a JRE the one prerequisite, which
is honest rather than free — and `doctor` names it first, with the
install command for the detected manager, because a tool that cannot
report its own missing runtime is the exact failure this spec set out
to prevent. A GraalVM/Scala Native binary that needs no JRE at all is
the obvious next step and is FILED, not promised: the renderers are
pure string builders and would port, but the value of shipping a
second build toolchain has to be asked for before it is paid.

The name is `okay`, with `deploy` as the first subcommand group.
okay-script's `Serve` and okay-acme's `Revoke` are the same kind of
operator-facing main and can become `okay script serve` and `okay
acme revoke` later; nothing about that is required today, and no
module changes for it.

### What makes a CLI trustworthy, spelled out

None of this is decoration; each line is a thing that goes wrong in
tools like this one:

- **Exit codes mean something**: 0 applied, 1 the operation failed, 2
  the arguments were wrong, 3 a prerequisite is missing (the doctor's
  own code, so a pipeline can tell "install docker" from "the deploy
  failed").
- **`--dry-run` on every verb that changes anything**, printing the
  exact commands it would run.
- **`--json` on every verb that reports**, with the same fields as
  the table.
- **No prompts unless a terminal is attached**, and none at all under
  `--yes`; a CLI that blocks a pipeline on a question is a CLI that
  gets wrapped in `yes |`.
- **`NO_COLOR` honoured**, and colour off when the output is not a
  terminal.
- **Help that fits a screen**, with the three commands an operator
  actually uses first.
- **Every failure carries the wrapped command** — the rule from "No
  silent failure, anywhere" applies to the CLI's own shelling out,
  and the CLI is where an operator will meet it.

`doctor` is not decoration either. Half of "it doesn't work" in
deployment is a missing binary, an expired login or a kubectl context
pointing somewhere else, and each has an exact question: is `flyctl`
on the PATH, does `flyctl auth whoami` answer, what does `kubectl
config current-context` say. A target that lists its `requires` gets
that for free, and the answer is a sentence naming the fix.

## The clean machine

Operator ask (2026-09-07), and the case every tool meets first: a
machine where nothing is installed yet. The requirement was put
exactly: install what can be installed, or at least tell the operator
plainly — never a cryptic error, and never a failure with no message
at all.

That last clause is the one worth reading twice. `docker: command not
found` deep inside a compose call, or an exit code 127 with an empty
stderr, is how most tools spend a new user's first hour. This model
can do better cheaply, because a target already declares what it
`requires`.

### What a check knows

```scala
final case class Tool(
  name: String,                       // "docker"
  probe: Vector[String],              // ["docker", "--version"]
  version: String => Option[String],  // read one out of that output
  atLeast: Option[String],            // the minimum this needs, when it has one
  why: String,                        // "the laptop target builds and runs containers"
  install: Map[Manager, String],      // the exact command, per package manager
  ready: Option[Ready],               // the probe that says it WORKS, not just exists
  note: String,                       // when there is nothing to RUN: "comes with Docker Desktop"
  site: String)                       // where the vendor documents it

/** written 2026-09-07: the sketch above had `usable` as a bare
 *  Vector[String], and the probe turned out to be useless without
 *  the sentence its failure produces -- two parallel Options that
 *  must agree is the shape this repository keeps deleting */
final case class Ready(probe: Vector[String], why: String, fix: String)

enum Manager: case Brew, Apt, Dnf, Apk, Pacman, Winget, Manual

enum Presence:
  case Ok(version: String)
  case Missing
  case TooOld(found: String, needed: String)
  /** installed, and still cannot be used: the state that actually
   *  happens most */
  case NotReady(why: String, fix: String)
```

`NotReady` earns its place by being the common case rather than the
rare one. Docker is installed and its daemon is not running.
`kubectl` is there with no current context. `flyctl` is there and
nobody is logged in. A tool that only asked "is the binary on the
PATH" would pass all three and then fail later, in someone else's
error message — which is exactly the outcome this section exists to
prevent. So a `Tool` may carry a second probe for readiness, and its
failure names the fix: `docker info` failing means "the Docker daemon
is not running — start Docker Desktop, or `sudo systemctl start
docker`".

### What the operator sees

One table, and it is the whole report:

```
okay-deploy: target `laptop` on macOS 15 (arm64), package manager: brew

  tool             state        note
  docker           MISSING      the laptop target builds and runs containers
                                install:  brew install --cask docker
                                docs:     https://docs.docker.com/get-started/
  docker compose   MISSING      comes with Docker Desktop; nothing to install separately
  openssl          ok 3.5.0
  sops             MISSING      only needed because one secret is a sops: reference
                                install:  brew install sops

2 of 4 tools are not ready. Nothing has been applied.
Run with --install to install what brew can, or install by hand and run again.
```

Three properties of that output are requirements, not taste. Every
missing tool says WHY it is needed, and the why names the thing in
the deployment that asked for it — a `sops:` reference, a
`Need.Database`, the target itself — so an operator can decide to
remove the need instead of installing the tool. Every install line is
the command for THIS machine, chosen from the detected package
manager, not a list of five alternatives to read past. And the last
two lines say what happened (nothing) and what to do next, because a
report that ends without a next step is a report that gets ignored.

The same report is available as JSON for a pipeline (`--json`), with
the same fields — a CI that fails should be able to say which tool
and why without parsing a table.

### Installing, and the line under it

Installation is **opt-in, one flag, and never silent**: `Up --install`
or `Doctor --install`. What runs is the platform's OWN package
manager, and the exact command is printed BEFORE it runs, so the
operator reads what is about to happen to their machine rather than
learning afterwards.

Four refusals hold, and each has a reason worth stating:

- **Never `curl … | sh`.** We print the vendor's documented command;
  we do not become a downloader of scripts that run as the user. A
  tool that ships only that way is `Manager.Manual`: the report
  carries the URL and stops.
- **Never silent `sudo`.** If a package manager needs it, that is in
  the printed command and the operator sees it. We do not prompt for
  a password ourselves and we never store one.
- **Never a version pin of ours.** We install what the platform's
  manager gives; `atLeast` only ever REPORTS that what is installed
  is too old, with the upgrade command. Pinning versions of other
  people's software is how a deployment tool becomes a package
  manager.
- **Never during `apply`.** Installation happens in its own step,
  before anything is rendered or applied. Half-applying a deployment
  and then installing a tool is the worst of both.

`--install` answers what it did per tool and re-runs the check, so
the last thing on screen is the same table with the rows now green,
or the ones that still are not and why.

### How the doctor finds the tools to check

A target already names what it `requires`, as strings. The doctor
resolves each name against ONE catalogue — `Tools.all`, keyed by that
name — and a name the catalogue does not know is itself a finding
(`unknown tool`), not a silent pass. That keeps the two halves honest
in opposite directions: a target cannot require something the doctor
would quietly skip, and the catalogue cannot grow entries nothing
asks for.

Three sources feed the list, and each is why-carrying by
construction:

- the target's own `requires`, whose why is the target;
- the deployment's secret REFERENCES, whose scheme names a tool
  (`sops:` needs sops, `aws-sm:` the aws CLI, `gcp-sm:` gcloud,
  `azure-kv:` az) and whose why is the reference itself, so an
  operator can delete the need instead of installing the tool;
- what a service's `Run` implies — a `Run.Module` needs a JRE
  wherever it is not carried in an image.

`Shell.run` is the single door to every subprocess in this module,
and it is what makes "no silent failure" mechanical rather than a
habit: it returns the exit code with the merged output, and
`Shell.must` turns a non-zero one into a message carrying the command
line, the code and the last lines — including the case that motivated
it, an exit code with nothing at all on either stream.

### The remote clean machine

The `host` target's install script is the same logic, rendered: it
runs on a rented box where nothing may be present, checks a JRE and
whatever the services need, and either installs through that
distribution's manager or prints the same report and exits non-zero.
An operator who reads the script before running it — which they
should — finds no surprise in it.

### No silent failure, anywhere

The rule generalises past bootstrap, so it is written here once: every
command this module shells out to is wrapped, and a non-zero exit
becomes a message carrying the command line, the exit code, the last
lines of its output, and what it was trying to do. A tool that exits
1 with nothing on stderr — and several of them do — must still
produce a sentence an operator can act on. "helm upgrade failed"
without the command is not one.

- [x] a target's `requires` is checked BEFORE anything is rendered or
      applied, and `Up` on a machine without docker prints the report
      and exits non-zero, having applied nothing.
- [x] a tool that is installed but not usable is `NotReady` with the
      fix in the message: a stopped docker daemon, a kubectl with no
      context, a flyctl that is not logged in.
- [x] the install command shown is the one for the detected manager,
      and a tool with no manager entry says `Manual` with its URL.
- [x] `--install` prints each command before running it, never uses a
      pipe from the network, and re-checks afterwards.
- [x] every shelled-out failure names its command, exit code and last
      output — asserted on a command that exits non-zero and prints
      nothing.

## Results, stage 0

**deploy-doctor-cli (2026-09-07), stage 0's second half.** The doctor
and the `okay deploy` CLI are in okay-deploy, proven on a real docker
(the Live suite renders a directory, runs `up`, sees the container,
runs `down`) and on okay-script's own committed artifacts copied to a
directory with nothing else in it.

Four things the writing decided or corrected:

- **`docker compose` is asked for BY NAME.** The laptop target used
  to require `docker` alone, and the plugin is exactly what a
  distribution's `docker.io` leaves out — the difference between a
  report and a `'compose' is not a docker command` half way through
  an apply.
- **`Ready` is one value, not two parallel Options.** See the
  correction beside the sketch above.
- **A missing tool with nothing to RUN says so in words.** The note
  ("comes with Docker Desktop; nothing to install separately") is a
  separate field from the install map, because that map's values are
  COMMANDS and English in it is how a tool ends up trying to run a
  sentence.
- **The target can come from the directory.** `okay deploy up` inside
  `deploy/host/` needs no argument: the JSON's own parent names the
  target, which is the shape an operator on a server actually types.
  When the directory is not a target's name, that is a named refusal.

What the doctor does NOT do is guess. A name a target requires that
the catalogue does not know is `Unknown` and fails the check — a
target cannot ask for something the doctor would quietly skip, and
the catalogue cannot grow entries nothing asks for.

- [x] the CLI reads `deployment.json` and never evaluates Scala: the
      Live test's directory holds the rendered files and the JSON,
      nothing else.
- [x] exit codes mean something: 0 applied, 1 failed, 2 arguments, 3 a
      prerequisite is missing, each asserted.
- [x] `--dry-run` prints the exact command and runs nothing.
- [x] `--json` on `doctor`, `diff` and `targets`, same fields as the
      table.
- [x] no prompts at all, so nothing blocks a pipeline; `--yes` is
      accepted and documented as changing nothing.

Not in this landing, and filed: okay-script's fourteen `OKAY_*`
variables re-expressed as a Schema'd config with the runtime order
defaults → file → environment (`script-config`), and the systemd unit
put in front of a real `systemd-analyze verify`, which needs a Linux
box this session does not have.

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

- **Stage 0 — the model, the settings, the clean machine, the CLI,
  and the two places that need no account.** `Deployment`/`Service`/
  `Need`/`Settings`, the `laptop` and `host` targets, the `okay
  deploy` CLI over `deployment.json` with the bootstrap report above,
  and okay-script's fourteen variables re-expressed as a `Schema`'d
  config. Proven by a real `docker compose up` in a Live test, a
  rendered unit that `systemd-analyze verify` accepts, a check run
  with a PATH emptied of docker — which is how the clean machine is
  testable without a clean machine — and the CLI driven from a COPY
  of the artifacts directory with the repository absent, which is the
  claim about self-containment being tested rather than asserted.
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

## Results

**deploy-model (2026-09-07), the first half of stage 0.** The model,
`Settings`, and the two targets that need no account are in
okay-deploy, and okay-script's own deployment is expressed in both the
old value and the new one — side by side on purpose, because a model
proven on a fixture is not proven.

What the port looked like before: written in `Deploy.port`, in an
`Env("OKAY_PORT", "8080")` pair beside it, in the Dockerfile's
`EXPOSE`, in the compose mapping and in the Helm values. After: once,
in `Need.Port(8080)`, with the setting derived. That is the whole
claim of the file, and it is now a test.

Three things the writing decided, which the spec had left open:

- **`Health` and `Resources` are the ones specs/deploy.md already
  had**, extended with `startupSeconds` and defaulted so every
  existing render stays byte-identical. Minting a second pair with
  the same meaning would have been the drift this repository has a
  rule against.
- **A `Need` a target cannot honour is a REFUSAL, not a silence.**
  `host` will not install a Postgres on someone's server, and says
  so with the service and the engine named — a unit that assumed a
  database was there would have failed at 3am instead.
- **A `Need.Volume` names the path the SERVICE sees.** With a
  container that is a mount; with systemd there is no container, so
  the install script creates that directory owned by the service's
  own user. Same value, two honest answers.

## Behavior (stage 0)

- [x] one `Deployment` renders `laptop` and `host` from the same
      value, and the port, the image and the database URL appear in
      each rendered file exactly once, from one field.
- [x] `Settings.of[A]` derives the environment names from the schema
      (camelCase to SNAKE_CASE, one prefix). The runtime's own order —
      defaults, file, environment — arrives with the okay-script
      config port, the second half of this stage.
- [x] a `Need.Database(Postgres)` becomes a compose service with a
      volume on `laptop`, and on `host` a named refusal — a rented
      box's Postgres is not ours to install — which is the model
      being honest rather than the renderer guessing.
- [x] no rendered file contains a secret value, asserted by a test
      that greps every rendering; an `env:` reference becomes a
      compose pass-through and a line in `.env.example`, a `file:`
      one needs no plumbing at all.
- [ ] `Doctor laptop` names a missing `docker` in a sentence with the
      fix in it; `Up laptop` on the example store answers a page over
      HTTP after `docker compose up`.
- [x] drift still holds, now per target: the committed rendering
      equals the value, per file, for `laptop` and `host` as well as
      for the old single-service `Deploy`.

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
- **The CLI reads the deployment as DATA, not as Scala.** Rejected:
  reflecting a compiled application jar (a dependency on every app's
  build), and shelling back into sbt (unavailable exactly where the
  CLI is most needed). The cost is that the model must be
  JSON-expressible, which is a pressure worth having.
- **A jar and a wrapper now, a native binary filed.** Rejected:
  paying for a second build toolchain before anyone asks; the JRE
  prerequisite is named by the doctor rather than hidden.
- **A clean machine is reported before it is fixed, and fixed only
  when asked.** Rejected: installing prerequisites automatically —
  it is someone's machine; a tool that changes it without being asked
  has to be trusted absolutely, and this one does not need that
  trust. Rejected too: `curl | sh` as an install road, at any
  convenience.
