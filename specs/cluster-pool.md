# cluster-pool — the engine on cluster managers: one process, every place

## Overview

The operator's ask (2026-09-23): deploy and drive okay's distributed
processing on clusters the way Spark is deployed and driven — on
Kubernetes, in a cloud, and everywhere Spark and Flink already run
(YARN, Mesos, and the rest) — and, if it can be done, do it with
something of our own that is SIMPLE, SAFE and EFFICIENT rather than a
copy of theirs. Three questions were asked with it and each gets a
one-line answer here, argued below:

- **Is it possible?** Yes, and most of it exists. specs/dataflow.md
  stages 1–13 are the engine: a coordinator, workers asked for jobs
  BY NAME with `Schema`'d parameters, a partition that is a recipe
  (replayed on a survivor, never snapshotted), an epoch loop with a
  journal, a fenced election seam, rescale at an epoch boundary.
  specs/deployment.md is the rendering: one `Deployment` value to nine
  targets. specs/discovery.md turns a service name into addresses.
  What is MISSING is the join between them — how worker processes come
  to exist on a manager, how a coordinator finds them there, and how a
  job is handed to them from outside — and stage 12 of dataflow
  ("the network", BLOCKED on machines) is the part nobody could run.
- **Is it very hard?** Spark's way is. Spark launches executors PER
  APPLICATION by talking to each manager's API — a scheduler backend
  each for standalone, YARN (an ApplicationMaster), Mesos, Kubernetes
  (the driver creates pods), plus an external shuffle service and jar
  shipping. Each is thousands of lines and its own class of bugs. Our
  way is not hard, because of one property the engine already has and
  Spark cannot: **nothing ships a closure** (dataflow Claim 3). A
  worker needs only the artifact, a port and the registrar class
  names, so a worker is a *stateless replicated service* — the ONE
  shape every cluster manager already runs well. We never call a
  manager's API. The manager owns processes; the engine owns
  partitions.
- **Can it be ours, simple, safe, efficient?** That is this spec: the
  **pool** — N copies of one process, discovered by the manager's own
  DNS or a list, any of which can take a submission and coordinate it.
  Per manager the whole cost is a RENDERING (a pure function, the
  okay-deploy shape) and, sometimes, a `Discovery` source. Manager-
  specific code in the engine: zero, asserted by a test.

**Everywhere, and where not.** The pool runs on anything that can (1)
keep N copies of one container or jar alive, each with a port, and (2)
let them address each other, by DNS, by environment, or by a list. The
targets table below says which of the existing nine and which new ones
satisfy both, and names the ones that cannot (a serverless runtime with
no peer addressing is refused by name, not made to limp).

## The model: a pool is a service, a job is a request

```
  manager keeps N alive ──►  pool member ×N  (one binary, one role)
                               │  serves partitions (Cluster.local)
                               │  takes submissions (HTTP)
                               │  can coordinate (Cluster.run / leading)
                               ▼
  submitter ── POST /pool/jobs/<name> ──► any member ──► Cluster.run over peers
                                             peers = Discovery(service) ∪ list
```

- **One process, one role.** Spark has driver/executor/master/worker
  and YARN adds an ApplicationMaster; Flink has JobManager/TaskManager/
  ResourceManager/Dispatcher. A pool has MEMBERS. Every member serves
  partitions through `Cluster.local` and every member can coordinate:
  a batch submission is coordinated by whichever member received it;
  a streaming submission with a journal runs under `Cluster.leading`
  with a `Lease`, so a member's death is a resume at the next epoch,
  which stages 8 and 10 already do.
- **The manager keeps the pool alive; the engine never asks it for
  anything.** No pod creation, no container allocation, no AM. Scale
  is the MANAGER's knob (`kubectl scale`, an HPA on a metric we
  export, a Nomad `count`), and a changed pool is what stage 13
  (rescale at an epoch boundary) already tolerates. This is
  specs/deployment.md's line — "orchestrate the tools, never the
  workloads" — applied to the engine: we stay a program the manager
  runs, not a peer of the manager.
- **A submission is DATA, so anyone can make one.** A job is a
  registered name and its parameters are a `Schema`'d value, so a
  submission is a small JSON or CBOR document: `curl` makes one, an
  Airflow or Argo step makes one, a laptop makes one, and no jar is
  uploaded because the artifact IS the image. The pool refuses a name
  its build does not register and a parameter its Schema does not
  accept, at the door, with the field named — the same door
  `Cluster.guarded`/`schemaChecked` already keep.
- **The job's version is the image tag.** Every member runs the same
  artifact — dataflow's stated bargain — and the pool ENFORCES it: a
  build fingerprint travels in the worker protocol and a member of
  another build is refused, so a rolling update cannot silently mix
  two versions inside one run.

Client mode and cluster mode, in Spark's words, are both there and
neither is special: a process that calls `Cluster.run` against
discovered peers IS a client-mode driver (fine on a flat network — a
laptop with a port-forward, a service in the same namespace); a
submission over HTTP is cluster mode, and the coordinator is a pool
member. There is no third mode and no mode flag.

## Interface

A new JVM module, **`okay-pool`**, because okay-cluster's compile graph
stops at okay-codec on purpose (the engine stays dependency-free) and
a pool needs HTTP, ops routes, discovery and settings:
`okayPool.dependsOn(okayCluster.jvm, okayHttp.jvm, okayOps.jvm,
okayResilience.jvm, okayConf.jvm)`.

```scala
package okay.pool

/** the pool's settings — one Schema'd value, so `Settings.of` renders
 *  them into every target's environment (specs/deployment.md) */
final case class PoolConf(
  port: Int = 7100,                 // the worker protocol AND the HTTP door
  service: String = "",             // a name Discovery resolves to the peers ("" = list only)
  peers: String = "",               // "host:port,host:port" — a static list, joined with `service`
  registrars: String = "",          // class names whose loading registers the jobs (WorkerMain's args)
  tolerance: Int = 3,               // consecutive failures before a peer is buried (dataflow-reconnect)
  build: String = "",               // the artifact fingerprint; "" = read from the jar's manifest
) derives Schema

/** what a member serves, over its one port */
object Pool:
  /** the worker protocol on `port`, and beside it the HTTP door */
  def main(args: Array[String]): Unit           // PoolConf from defaults → file → environment

  /** the peers as the coordinator will see them: discovery ∪ list,
   *  each a `Served.reconnecting`, minus this member (served in-process) */
  def peers(conf: PoolConf, discovery: Discovery): Vector[Cluster.Serve] ! Async

/** the door: a submission is a job by name, parameters as the job's own Schema */
final case class Submission(job: String, params: Json, parts: Int = 0,   // 0 = one per peer
                            stream: Option[Streaming] = None) derives Schema
final case class Streaming(take: Int, journal: String = "") derives Schema   // journal: a Checkpoint name the build binds
final case class Submitted(run: String) derives Schema
enum Status derives Schema:
  case Running(epoch: Int, peers: Int, buried: Int)
  case Done(value: Json, dropped: Long, merged: Long, retried: Int, failed: Int)
  case Failed(why: String)
```

| route | answers |
|---|---|
| `POST /pool/jobs/{name}` | `202 Submitted` — a run id; `400` a parameter its Schema refused (field named); `404` a name this build does not register |
| `GET /pool/runs/{id}` | `Status`; `Done.value` is the run's answer under the job's `Wire` Schema, as JSON |
| `GET /pool/jobs` | the names this build registers, each with its parameter Schema's `Digest` |
| `GET /pool/peers` | what discovery answers now, and which are buried |
| `/healthz`, `/readyz`, `/metrics` | okay-ops's, unchanged; `readyz` is false until the registrars loaded |

```
okay pool submit <url> <job> [--params '{…}'] [--parts N] [--stream take] [--wait]
okay pool runs   <url> [id]
okay pool jobs   <url>
okay pool peers  <url>
```

The CLI joins `okay deploy` in okay-deploy's `okay` binary (its
subcommand groups were planned there); `--wait` polls `runs` and exits
0 on `Done`, 1 on `Failed`, printing the value — so a pipeline step is
one line.

### The deployment side

One new `Need` in specs/deployment.md's closed enum, and the spec edit
is deliberate (that enum's growth is the brake):

```scala
enum Need:
  …
  /** this service must reach its OWN replicas one by one — a pool.
   *  A target answers with per-replica addressing or refuses by name. */
  case Peers
```

| target | `Need.Peers` renders as | discovery source |
|---|---|---|
| `laptop` (compose) | `deploy.replicas: N`; compose's DNS answers every replica for the service name | `dns` |
| `host` (systemd) | N units on N ports, `OKAY_POOL_PEERS` written into the `EnvironmentFile` from the value | list |
| `cluster` (Kubernetes) | a **headless** Service beside the Deployment (`clusterIP: None`, ready pods only); `OKAY_POOL_SERVICE=<name>` | `dns` |
| `aws` (ECS) | Service Connect / Cloud Map namespace; the DNS name | `dns` |
| `fly` | `<app>.internal` answers every machine's private address | `dns` |
| `azure` (Container Apps) | **REFUSED**: replicas are behind one ingress, no per-replica address | — |
| `gcp` (Cloud Run) | **REFUSED**: no peer addressing, scales to zero | — |
| `render`, `railway` | **REFUSED** for the same reason; a pool there is one member | — |
| `nomad` (NEW) | a job with `count = N`, a `service` stanza, Consul or Nomad-native DNS | `dns` |
| `yarn` (NEW) | a YARN Services API `Yarnfile` (Hadoop 3.1+), `number_of_containers: N`, the registry DNS name | `dns` or list from `yarn app -status` |
| `slurm` (NEW) | an `sbatch` script for N tasks; the peer list from `scontrol show hostnames $SLURM_JOB_NODELIST` written before the members start | list |
| `swarm` (NEW) | the compose file with `deploy.replicas`; `tasks.<name>` answers every task | `dns` |
| `batch` (NEW, AWS Batch multi-node) | a node-parallel job; the main node's address and the node count arrive in the environment | list |

Each refusal names the target, says why (no per-replica address), and
names the nearest target that works — the shape `Need.Volume` on gcp
already has. **Mesos is not a row**: Apache Mesos went to the Attic
(2023), Spark 4.0 and Flink 1.17 removed it, and a Marathon JSON
rendering is a trigger ("somebody runs a live Mesos and asks"), not a
stage. The three managed Spark platforms are not rows because they are
rows already: EMR on EKS and Dataproc on GKE are `cluster`, classic EMR
and Dataproc are `yarn`.

## The claims, and what falsifies each

1. **Zero manager-specific code in the engine.** okay-cluster and
   okay-pool contain no Kubernetes, YARN, Nomad, Slurm or cloud
   client, name or API call; every manager is a rendering in
   okay-deploy plus a `Discovery` source. *Falsified by*: a manager
   whose service shape cannot host a pool without a change to the
   engine or the pool. (The test is a grep over the two modules'
   sources, run in the gate.)
2. **One job, every target, one checksum.** The same registered `Job`
   answers the same value on N in-process members, N compose
   containers, and N pods on kind — with no change to the job.
   *Falsified by*: any target needing a job or a `Wire` change.
3. **A warm pool answers in the run's own time.** A small job's
   submission-to-`Done` on kind is under a second at the median, where
   Spark cluster mode on the same kind pays executor pod scheduling per
   application. *Falsified by*: the measurement (stage 6), not by an
   argument; the number is recorded beside Spark's own on the same
   box, with the fixed/marginal split docs/benchmarks.md §20 uses.
4. **A killed pod is a replayed partition, a killed coordinator is a
   resume.** `kubectl delete pod` of a member mid-stream leaves the
   answer equal to the batch answer; of the coordinating member, a
   successor picks the run up from the journal at the next epoch — on
   real pods, which is what dataflow stage 12 asked for and could not
   have. *Falsified by*: a run on kind that loses or duplicates a pane.
5. **A submission cannot execute code.** The pool runs only names its
   build registered; a submission carries no class, no jar, no
   expression, and the parameter is decoded under the job's Schema
   before anything runs. *Falsified by*: any road from a submission
   body to a JVM class not in the artifact. (`shipped-terms` in the
   backlog is the ONE deliberate exception, off by default, and this
   claim is restated there if it lands.)

## Security

- **Authorisation is `Cluster.guarded`, unchanged**: an allow-list of
  jobs and of coordinators. The pool adds the door: a submission is
  authenticated by whatever `okay-security` capability the pool is
  configured to accept, and a member-to-member request by the
  transport (stage 4: mTLS through okay-tls, each member with the
  pool's certificate, the same secret reference on every target).
- **The build fingerprint is checked both ways** — a member refuses a
  coordinator of another build and a coordinator buries a member of
  another build, so a rolling update makes a run FAIL loudly rather
  than answer from two versions.
- **No compiler, no sbt, non-root, one port** in the image — the
  Dockerfile specs/deploy.md already renders. NetworkPolicy on
  `cluster` allows the pool port from the pool's own pods and the
  submitter's namespace only; the other targets get the equivalent
  where one exists and a comment where none does.
- **Blast radius of a bad job** is one member: the manager restarts
  it, the engine buries and replays. A job that kills every member it
  touches is a job that gets buried from the pool's `guarded` list by
  an operator, not a job the pool retries forever — `tolerance` is
  per run and `Run.failed` says what happened.

## Stages

- **0 — this spec.**
- **1 — the pool process** (`okay-pool`, `Pool.main`, the four routes,
  `PoolConf`, peers = discovery ∪ list, the build fingerprint in the
  protocol). Proven on ONE machine: N `Pool` processes on N ports with
  a static list, a submission through `curl`, the value equal to
  `Flows.fan`'s; a member killed mid-run; a member of a different
  fingerprint refused. `TestPool` in the default suite where it spawns
  no process, `Live` where it does (the federation two-process suite
  is the precedent, and its timeout under load is filed).
- **2 — `Need.Peers` and the `cluster` target, on kind.** The headless
  Service, `OKAY_POOL_SERVICE`, `dns` discovery inside a pod, and the
  stage-12 harness at last: N pods, a submission, `kubectl delete pod`
  of a member and of the coordinator, `kubectl scale` between epochs.
  `Live` and docker-dependent (kind). `laptop` and `host` in the same
  stage because they are a line each and the compose one is the
  fastest harness of all.
- **3 — the other managers, as renderings.** `nomad`, `yarn`,
  `slurm`, `swarm`, `batch`: each a pure renderer, each gated by a
  real parser or validator for its format (`nomad job validate` where
  it runs offline, the Yarnfile through okay-codec's `Json`, `sh -n`
  on every script — stage 2 and 3 of specs/deployment.md set the
  bar), and each refusal (`azure`, `gcp`, `render`, `railway`) a named
  one with a test. No account is needed for any of it.
- **4 — the door and the wire, secured.** mTLS between members
  (okay-tls), a capability at the submission route (okay-security),
  the NetworkPolicy on `cluster`, and the unauthenticated pool
  REFUSING to listen on a non-loopback address unless told
  `OKAY_POOL_INSECURE=true` — a pool that is open by default is the
  Spark REST server's CVE.
- **5 — a pool that changes size.** Peers re-resolved at every epoch
  boundary (stage 13's `Job.rescalable` decides whether the run may
  follow); a metric for the manager's autoscaler (`okay_pool_queued`,
  submissions waiting for a coordinator); a `Lease` over the
  manager's own primitive where one exists — Kubernetes
  `coordination.k8s.io/Lease` and Consul sessions are each a few
  dozen lines behind the three-method seam, live in okay-pool behind
  a flag, and are the ONLY place the pool ever speaks to a manager's
  API. Slurm and YARN have none: rank 0 leads with `Lease.solitary`,
  and the spec says so rather than inventing an election.
- **6 — the numbers.** Claim 3 measured: submission latency and the
  fixed/marginal split on kind, beside Spark cluster mode on the same
  kind (okay-spark already carries the dependency), recorded in
  docs/benchmarks.md §20 with the same honesty its distributed section
  has now — one machine, containers, not a datacentre.
- **7 — docs.** `docs/modules/okay-pool.md`, a "Running it on a
  cluster" section in docs/modules/okay-cluster.md, and the deploy
  page's targets table extended. A lane ships its docs before landing
  (operator, 2026-09-22); this stage exists so the ARC's docs are
  written as one story rather than seven footnotes.

## Behavior

Stage 1:
- [ ] `Pool.main` reads `PoolConf` as defaults, then a file, then the
      environment, in that order; loads the registrars; serves the
      worker protocol and the HTTP door on one port; `readyz` is false
      until the registrars have loaded and true after
- [ ] `Pool.peers` is discovery ∪ list minus this member, each a
      `Served.reconnecting`; an empty union plus this member runs the
      job on one partition in-process and says so in `Status`
- [ ] `POST /pool/jobs/{name}` with a body the job's Schema refuses
      answers 400 naming the field; an unregistered name answers 404
      with the registered names; a valid one answers 202 with a run id
- [ ] `GET /pool/runs/{id}` reports `Running` with the epoch and the
      buried count while the run is on, then `Done` with the value,
      the dropped/merged/retried/failed counts of `Run`; `Failed`
      carries the coordinator's own reason
- [ ] N processes on one machine with a static list: the value equals
      `Flows.fan`'s over the same feed (the bar every dataflow stage
      set); one member killed mid-run: equal, `retried` ≥ 1
- [ ] a member whose build fingerprint differs is refused as a
      coordinator and buried as a worker, each with the two
      fingerprints in the message
- [ ] a streaming submission with `journal` names a `Checkpoint` the
      build bound; a second submission of the same job and journal
      after the coordinating member died resumes at the next epoch

Stage 2:
- [ ] `Need.Peers` renders on `cluster` a headless Service beside the
      Deployment, `publishNotReadyAddresses` absent (ready pods only),
      `OKAY_POOL_SERVICE` set to its name; `helm lint` accepts it
- [ ] on `laptop`, `deploy.replicas` and the service name as
      `OKAY_POOL_SERVICE`; on `host`, N units and the list rendered
      into the `EnvironmentFile`
- [ ] `gcp`, `azure`, `render`, `railway` refuse `Need.Peers` by name,
      each naming the nearest target that works
- [ ] on kind (Live): N pods answer a submission with the batch value;
      `kubectl delete pod` of a member mid-stream leaves the answer
      equal; of the coordinator, a re-submission resumes from the
      journal at the next epoch and re-offers only the in-flight epoch
- [ ] `kubectl scale` between epochs is followed by a `rescalable`
      job and refused by name for a windowed one (stage 13's rule,
      now on real pods)

Stage 3:
- [ ] every new target's rendering passes its format's real parser;
      every rendered script passes `sh -n` through the existing walk
      over `Targets.all`
- [ ] the grep test: no manager's name or client in okay-cluster's or
      okay-pool's main sources

Stage 4:
- [ ] a pool told to listen on a non-loopback address without TLS or a
      capability refuses to start, naming the flag that overrides it
- [ ] a submission without the configured capability is 401 before the
      Schema is consulted (the same order `guarded` keeps: a stranger
      learns nothing)
- [ ] two members with the pool certificate speak; one without is
      refused at the handshake, not at the request

Stage 5:
- [ ] a `Lease` over a Kubernetes Lease object: two members, one seat,
      the deposed one's next commit throws `Checkpoint.Deposed`
- [ ] the autoscaler metric is exported through okay-ops's `/metrics`

Stage 6:
- [ ] the submission latency table, ours beside Spark's, on the same
      kind, with the fit

## Out of scope

- **Data locality.** Spark places tasks by HDFS block location; a pool
  partition is a recipe reading a log, a topic or an object store from
  wherever it runs. A partition-to-member affinity hint is a later
  spec, if a measurement ever asks for it.
- **The engine allocating resources per job.** Dynamic allocation is
  the manager's autoscaler over our metric (stage 5), never a call
  from the engine.
- **An external shuffle service.** The exchange (dataflow stage 2)
  writes hash buckets on members; a dead member's buckets are
  recomputed (stage 5 of dataflow). A service to keep them past a
  member's death is a different, later trade.
- **A scheduler of our own** — queues, fair sharing, priorities. One
  pool per tenant; the manager's namespaces and quotas are the
  scheduler, and they are better at it.
- **A web UI.** `GET /pool/runs` and okay-ops's `/metrics` are what a
  dashboard reads; drawing it is not this module's job.
- **Spark or Flink API compatibility**, and Mesos (see the table).
- **Shipping code in a submission** — the `shipped-terms` backlog
  entry is that question, and it is additive to this spec, not part
  of it.

## Decisions

- **A pool, not per-job executor launch.** Rejected: Spark's model, a
  scheduler backend per manager. It is what makes Spark hard to port
  and hard to secure; it exists because Spark must ship the
  application's classes to executors that did not have them, and we
  do not.
- **One role, not driver/executor.** Rejected: a separate coordinator
  Deployment. A second thing to run, elect and page; `Cluster.leading`
  already lets any process take the seat, and a homogeneous pool is
  the thing a manager scales without questions.
- **The engine never speaks to a manager**, with stage 5's `Lease`
  bindings the one exception — behind the three-method seam, behind a
  flag, in okay-pool and never in okay-cluster. Rejected: a Kubernetes
  client in the engine for "just" pod listing; that is the first line
  of a scheduler backend.
- **Discovery is the manager's own DNS or a list**, per
  specs/discovery.md, which already declined a registry of ours.
- **Submission is data over HTTP**, not a jar and not a Scala
  expression. Rejected: a `spark-submit`-shaped upload — that is the
  closure-shipping this engine defines itself against; and a REPL road
  is `shipped-terms`, separately.
- **The artifact version is enforced, not assumed.** Rejected: trusting
  the manager's rolling update to be atomic; it is not, and two
  versions in one run is a silent wrong answer.
- **Mesos is a trigger, not a stage** — retired upstream, dropped by
  both engines this spec measures against.
- **A new module `okay-pool`** rather than growing okay-cluster.
  Rejected: adding okay-http and okay-ops to the engine's compile
  graph; specs/dataflow.md keeps it at okay-codec on purpose and this
  spec has no reason to spend that.
