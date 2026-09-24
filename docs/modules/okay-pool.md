# okay-pool

> The engine on a cluster manager (specs/cluster-pool.md, stage 1): a
> POOL of identical processes, discovered by the manager's own DNS or
> a static list, any of which serves partitions on the worker
> protocol AND takes a submission over HTTP. No manager-specific code
> anywhere in this module or in okay-cluster — a manager's whole job
> is to keep N copies of one process alive; okay-deploy renders the
> per-manager wiring (`Need.Peers`, specs/deployment.md).

Depends on: `okay-cluster.jvm`, `okay-http.jvm`, `okay-ops.jvm`,
`okay-resilience.jvm`, `okay-conf.jvm`, `okay-jetty`. JVM only — the
worker protocol is a raw socket and the HTTP door is a real server,
neither of which a browser runs.

## Guide

**One process, one role.** Every pool member serves partitions
(`Cluster.local`, wrapped so its `Req.Known` answer carries this
build's fingerprint — see below) on `PoolConf.port`, and takes
submissions over HTTP on `PoolConf.httpPort` — a SEPARATE port on
purpose, since the two are different protocols and cannot share a
listener. Whichever member accepts a `POST` becomes that run's
coordinator; there is no second role and no election among peers for
who gets to be one.

**Every submission is journal-backed — there is no bare in-memory run.**
`POST /pool/jobs/{name}` never calls `Cluster.run`. It always calls
`Cluster.leading`, with a journal name that IS the run id
(client-supplied, or minted here). `GET /pool/runs/{id}` reads the
answer from that journal — never from a live process's memory — so
ANY member can answer it, including one that never saw the original
`POST`. If the run is not finished and nobody currently holds the
lease, the member handling the `GET` nudges it forward itself: the
same "`None` means somebody else holds it" shape `Cluster.leading`
already has, just invoked by a reader instead of a fixed retry loop.
This is deliberately not a background sweep — a run makes progress
when something asks about it, and the CLI's own `--wait` is exactly
that something. See specs/cluster-pool.md, "the run id is the journal
name", for the full argument, including the one honest limit: a run
nobody ever polls does not resume itself.

**`PoolConf.store` is one factory, not a Store type.** A build
registers a single `(runId: String) => (Checkpoint, Lease)` — the
same shape `Jobs.register` already has for jobs, one level up. It
MUST be reachable from every member (okay-persist's compacted log and
`Election` are the bound `TestPersisted` already proves in a dozen
lines); the in-memory default works alone and is REFUSED once more
than one peer is configured, because "any member can answer" is
exactly the property a per-process cell does not have.

**A build fingerprint keeps a rolling update honest.** `Req.Known`
doubles as the handshake: `okay.cluster.Resp.Names` gained an opt-in
`build` field (empty by default, so nothing that does not set
`PoolConf.build` changes), and `Pool.fingerprinted` stamps a member's
own answer with it. `Pool.workers` probes every discovered/static
peer this way and EXCLUDES one whose build disagrees, so a run answers
from one artifact only — a mixed-version answer fails loudly by never
including the disagreeing peer, rather than mixing two versions
inside one merge.

**Where a job's answer's Schema comes from.** `Jobs.find` hands back
an existential `Job[?, ?]`, and nothing on it used to expose a
`Schema[R]` for the PRESENTED value — only the partial's Schema
(`Wire#wire`), which is a different shape. `Job.answer` (new,
required on every `Job`) is that Schema; `Job.lead` and `Job.answerOf`
are the two methods (living on `Job` itself, so `this.type` fixes `P`
and `R` — no cast) that let this module coordinate a job and read its
finished answer back as JSON without ever naming its types.

## Running it

One process, discovered by a static list (the smallest useful shape —
`OKAYPOOL_PEERS=127.0.0.1:7101,127.0.0.1:7102`, no `OKAYPOOL_SERVICE`
at all):

```
$ OKAYPOOL_REGISTRARS=my.app.Jobs OKAYPOOL_STORE=my.app.PersistStore \
    java -cp "$CP" okay.pool.Pool
okay-pool: worker protocol on 7100, http on 7101, jobs [my.job]
```

Submit and poll, from anywhere on the network:

```
$ curl -sX POST http://host:7101/pool/jobs/my.job \
    -d '{"params": {"n": 20000}, "journal": "run-1"}'
{"run":"run-1"}
$ curl -s http://host:7101/pool/runs/run-1
{"Running":{"epoch":3,"peers":4}}
$ curl -s http://host:7101/pool/runs/run-1
{"Done":{"value":"20000","dropped":0,"merged":179,"retried":0,"failed":0}}
```

`value` is the job's own `Job.Answer`-rendered JSON, as a STRING —
re-decode it under the job's own answer shape on the caller's side;
this door cannot name that shape itself (see "Where a job's answer's
Schema comes from", above).

On Kubernetes and every other manager: `Need.Peers` in
specs/deployment.md renders the per-manager wiring — a headless
Service on `cluster`, `deploy.replicas` on `laptop`, and so on — and
`okay deploy up <target>` applies it exactly as any other okay
application's. Stage 2 of specs/cluster-pool.md is that rendering and
the real-pod proof; stage 1 (this module) is proven on one machine,
several processes.

## API reference

| member | signature | meaning |
|---|---|---|
| `PoolConf` | `port, httpPort, service, peers, registrars, store, tolerance, build` | one member's whole configuration; `OKAYPOOL_*` env names, `OKAYPOOL_CONF` for a file |
| `PoolConf.load` | `(env, slurp) => Either[String, PoolConf]` | defaults → file → environment (specs/conf.md's layering) |
| `Stores.set` / `.get` | `(String => (Checkpoint, Lease)) => Unit` / `=> Option[...]` | the one store factory a `PoolConf.store` registrar registers |
| `Pool.resolve` | `(PoolConf, Discovery) => Vector[Endpoint] ! Async` | discovery ∪ the static list, deduplicated |
| `Pool.workers` | `(PoolConf, Discovery) => Vector[Cluster.Serve] ! Async` | this member, plus every peer whose build agrees |
| `Pool.fingerprinted` | `(build: String) => Cluster.Serve` | this member's serving side, stamped for a remote coordinator's probe |
| `Pool.submit` | `(name, params, parts, take, journal, conf, discovery, store) => Either[(Int,String), Submitted] ! Async` | the door's own logic behind `POST` |
| `Pool.statusOf` | `(id, conf, discovery, store) => Option[Status] ! Async` | the door's own logic behind `GET`; nudges an unfinished run |
| `Routes.router` | `(conf, discovery, store, ready: () => Boolean) => Router` | `/healthz`, `/readyz`, `/pool/jobs`, `/pool/jobs/{name}`, `/pool/runs/{id}`, `/pool/peers` |
| `Pool.main` / `Pool.run` | `Array[String] => Unit` | the worker protocol and the HTTP door, on their own ports |
| `Job.answer` | `Schema[R]` (new, on every `Job`) | the PRESENTED value's Schema — see "Where a job's answer's Schema comes from" |
| `Job.lead` | `(paramsJson, parts, peers, take, checkpoint, lease) => Either[String, Option[Job.Answer] ! Async]` | coordinate this job, naming neither `P` nor `R` |
| `Job.answerOf` | `(paramsJson, Folded) => Either[String, Job.Answer]` | a finished run's answer, from its journal alone |

## Gotchas

- `port` and `httpPort` are two different listeners on purpose — the
  worker protocol is raw length-prefixed CBOR, the HTTP door is
  HTTP; sharing one port number is not possible and the first draft
  of this spec said otherwise before the code caught it.
- `Attempts` (inside `Pool`) de-duplicates a NUDGE per process, not
  per id globally — it exists only to stop this ONE member from
  re-asking its own lease on every poll of a run it is already
  driving. A different member is free to attempt independently at
  any time, which is the whole point.
- `take <= 0` on a submission means "one epoch, run to completion" —
  internally `Int.MaxValue`, so a bounded job still goes through the
  same epoch loop a genuinely unbounded one does. "Batch" is a
  client-facing word here, not a second code path.
- `Job.answerOf`'s `retried`/`failed` come back `0` when a run's
  answer is read from a bare journal by a member that was never
  inside the attempt that finished it — `okay.cluster.Folded` does
  not carry them (they are `Living`'s own per-attempt counters). The
  computed VALUE is exactly right either way; only these two
  diagnostic counters reset on a resume, the same way a resumed
  `Cluster.stream`'s own `Living` already does.
- A repeat `POST` of an id already on record reuses the STORED
  record and ignores the new body entirely — idempotent without ever
  comparing the two.
