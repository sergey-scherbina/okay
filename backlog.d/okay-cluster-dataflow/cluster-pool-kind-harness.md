- [ ] cluster-pool-kind-harness — the pod-level half of stage 2 of
      specs/cluster-pool.md, split off from cluster-pool-targets
      (LANDED 2026-09-24, the model-rendering half) because it needs a
      real `okay-pool` container image and a full deploy, a distinct
      piece of work from rendering the manifests that carry it. The
      dataflow spec's own stage 12 ("the network") has waited on
      exactly this: N real pods on `kind` (already installed and
      proven reachable via `kubectl`/`helm` in this session), a real
      submission through the headless Service, `kubectl delete pod`
      of a member mid-run (answer equal to the batch one) and of the
      coordinator (a `GET` from a survivor resumes, no second
      submission), `kubectl scale` between epochs (followed for a
      `rescalable` job, refused by name for a windowed one, per stage
      13's existing rule). `Live`, docker-dependent. Needs, in order:
      a Dockerfile for okay-pool (Deploy's own multi-stage shape), a
      `Deployment` value with a real registered test job, `helm
      install` onto kind, then the kill/scale sequence. cluster-pool-numbers
      (stage 6) depends on this same image and cluster existing.
