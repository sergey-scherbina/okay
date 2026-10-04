- [ ] bayes-smc-move-tuning — found by okay-bayes-resample-move (2026-10-02).
      A move kernel in `smc(..., move = …)` runs with tuning OFF (a kernel
      tuned while it moves particles would not be invariant), so a NUTS
      block is frozen at its first reasonable step: 143 distinct of 500 on
      400 flips, where `Kernel.site("p").times(3)` reached 968 of 1000.
      Adapt between resamplings instead — the particle cloud's spread sets
      the proposal scale and the NUTS metric (Chopin 2002's adaptive
      resample-move; Fearnhead & Taylor 2013) — and measure the distinct
      count and ESS per move against the untuned kernel.
