## cluster-pool-process - the pool process, landed

Stage 1 of specs/cluster-pool.md: the new module `okay-pool` runs
okay's distributed engine as a pool of identical processes. Every
member serves partitions on a raw worker-protocol port and takes
submissions on a separate HTTP port; whichever member accepts a
`POST` coordinates it under `Cluster.leading` with a journal (never a
bare `Cluster.run`), so `GET /pool/runs/{id}` — from any member —
reads the answer from the journal and resumes an unfinished run
itself if nobody currently holds the lease. `PoolConf.store` is one
`(runId => (Checkpoint, Lease))` factory a build registers, the same
shape `Jobs.register` already has for jobs; the in-memory default
refuses to start once more than one peer is configured. A build
fingerprint, carried opt-in on `Req.Known`'s answer, lets a
coordinator exclude a peer running a different artifact before
handing it work.

`okay.cluster.Job` gained `def answer: Schema[R]` (required, one line
on fourteen existing implementers) plus `Job.lead`/`Job.answerOf`,
letting the pool coordinate a job and read its finished answer as
JSON without ever naming the job's own types.

Gated: `okayPool/test` clean; the fiber-cancel-and-resume test is
`Live`-tagged (races real timing, like `TestFederation`'s two-process
suite) and passed on three explicit runs; `scripts/gate.sh "affected
master"` green at 7134 tests (the whole family, since `build.sbt`
itself changed) after fixing four `Job` implementers in `compare`
that a renamed import (`Job as Submitted`) had hidden from the first
grep sweep — caught by the compiler. Docs: docs/modules/okay-pool.md,
docs/README.md's index. Filed: `cluster-pool-rolling-drain` (no
automated drain of an in-flight run before a rolling update).

Stages 2-6 (the cluster manager renderings, security, elasticity, the
numbers, the arc's own docs) stay in the backlog, unclaimed.
