- [ ] cluster-pool-rolling-drain — a gap named in conversation
      (2026-09-24), not yet in specs/cluster-pool.md: a rolling update
      of a pool mid-run has no automated drain. The build-fingerprint
      check (Security section) refuses an old/new mix, which is
      correct for CORRECTNESS but means a batch run caught mid-rollout
      fails loudly and must be resubmitted, and a long-running
      streaming run must be manually paused or allowed to finish its
      current epoch before the operator rolls the image — nothing
      today tells the rollout to wait for that. What a fix would need:
      either a `PATCH /pool/runs/{id}?pause=true` that lets a run stop
      cleanly at its next epoch boundary and be resumed after the
      rollout (reusing the same `Cluster.leading`/`Checkpoint` seam,
      no new mechanism), or a documented operational recipe (drain
      submissions, wait for `GET /pool/runs` to empty, then roll) with
      nothing enforced in code. Trigger: cluster-pool-process landed
      and an operator asks for a rollout story.
