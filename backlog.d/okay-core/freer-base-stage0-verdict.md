- [x] freer-base-stage0-verdict — DONE. The verdict the entry asked
      for is "land", and stage 0 landed: nothing more than 2.4% slower
      on any core lane, eight lanes faster, allocation at or below
      master everywhere. What closed it after the entry was written:
      `map` reaching the absorbing path, `relay`'s loop getting back
      under the JIT inlining threshold (landed separately, 25102517),
      and `Once` becoming an enum so the runner's call has one target
      (the operator's proposal — worth 0.955-0.968 on the Fib lanes,
      while making the same site merely bimorphic was worth nothing).
      Four refuted theories and every number are in
      specs/freer-base.md Results; rows `freer0*` and `once-*`.
