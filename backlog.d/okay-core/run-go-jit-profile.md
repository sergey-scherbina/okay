- [ ] run-go-jit-profile — PRIORITY: LOW, NOTE (2026-10-04,
      state-foreign-shape). The one loop `Run.go` is sensitive to the JIT's
      type profile at the continuation call `f(a)`: in a benchmark whose
      program has two continuation lambdas C2 inlines them into the loop
      and it slows (stateForeign 41 vs 33 us with dontinline). Real programs
      make the site megamorphic. Read benchmark deltas on the machine with
      this in mind; a `dontinline` control run separates the two.
