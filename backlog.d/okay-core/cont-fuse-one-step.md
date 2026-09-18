- [x] cont-fuse-one-step — DONE, absorbed by freer-base stage 0:
      `Cont.Fuse` no longer exists. Absorption is one step, carried by
      the leaf function's own class (`Shift.Once`), and the depth was
      settled by a sweep of 1/4/16/128 rather than by lowering a
      constant — the Fib lanes are flat across it with identical
      allocation, `statePara` reads 0.861 at depth 1 against
      1.19/1.15/1.17 deeper. Rows `fuse0-*`, `fuse1-*`,
      `freer0b-absorb-sweep`.
