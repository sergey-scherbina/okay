## cluster-pool-journal-mandatory - closing a single-point-of-failure in the pool design

Review of specs/cluster-pool.md found a gap: a plain (non-streaming)
submission ran as an in-memory `Cluster.run` on whichever pool member
accepted it, so that member's `Status` lived only in its own process
memory — contradicting the spec's own Claim 4 ("a killed coordinator
is a resume") for exactly the request that needed it most.

Fixed in the spec (no code yet — stage 1 is still queued): every
submission now runs under `Cluster.leading` with a journal, client-
supplied or pool-generated, and the run id IS that journal's name.
`GET /pool/runs/{id}` reads the shared `Checkpoint`, so any member can
answer it, and resumes the run itself if the lease is free — a reader-
triggered resume, not a background sweep (kept out, per the spec's own
"no scheduler of our own" rule). `PoolConf` gains `store`, one
registered `(runId => (Checkpoint, Lease))` factory every run's journal
opens through; the in-memory default is refused once the pool has more
than one peer. `sprint.d/queue/cluster-pool-process.md` updated to
match.
