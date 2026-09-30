- [ ] windowjoin-spin-fix — okay-stream/BUGS.md `windowjoin-trim-spins`:
      `TestSourceJoinWithin`'s early-stop test under `Schedulers.own`
      never returns; the one pool worker spins in `WindowJoin.trim` from
      the join's `through` loop (dumps in .work/ci/diag/…63346, …73993).
      Reproduce with a timeout, read the dump for which side is starved,
      fix BOTH the cause (the third pair never told) and the quadratic
      per-arrival `trim` of the row's own store, pin with a test that
      fails first. Owner of the join lane. (2026-09-30, operator: "Исправь")
