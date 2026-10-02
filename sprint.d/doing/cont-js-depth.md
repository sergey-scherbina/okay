- [ ] cont-js-depth — `Cont`'s runner nests the host stack for nested
      OPAQUE bodies (a shift body that calls its continuation before it
      returns), and survives that on the JVM and Native only by moving to
      a fresh stack (StackSwitch, specs/cont-stack.md); Scala.js has no
      fresh stack, and the engine's stack is "the bound" written there.
      Measured 2026-10-02 (eager-carrier-depth): a derivation that ran
      through it overflowed Scala.js between 300 and 1 000 levels and a
      128 KB JVM thread at 1 000. eager-carrier-depth took `tailRecM` and
      `foldMap` off this road; the runner itself still has it. OPERATOR'S
      BAR (2026-10-02): "полный трамплининг — чтобы не было никакого
      переполнения в принципе", on every platform, so a stack switch is
      not the answer. To find: whether a body's synchronous call of `k`
      can be detected and continued by the runner's own loop (the
      callback drive's Got/Moved exchange does this for Await), or
      defunctionalized; the cost on the hot paths (specs/cont-stack.md's
      lanes); acceptance = nested opaque bodies a million deep on a
      128 KB JVM thread, on Scala.js and on Native.
