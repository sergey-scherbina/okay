- [ ] cont-core-remaining-costs — PRIORITY: LOW, the measured price of
      the λ$ core (cont-core-design, 2026-10-01, specs/cont-core.md
      Results; alloc profiles by async-profiler event=alloc). Against
      master 4759dbff7 after the three returns:
      (1) BARE install/pop, no capture (delimPushOnly, delimDollarOnly,
      kontResetOnly) 1.24-1.28x at nearly equal bytes: the segment under
      a delimiter is its own `Run`, popped as its own step, and a plain
      reset calls its identity `ret`. A separate plain node removes only
      the call; the fused delimiter (segment inside it) removes the step
      and is the design the lane took apart — return only with a number
      worth that.
      (2) The FLAGS' price: a clause lambda per `shiftLeaf` (the strict
      flag; statePara ~49 KB/op) and an `Inject` + `Dollar0` per derived
      `shift` (the under flag; delimGenerator ~15 KB/op). contAnswer
      1.21x, delimGenerator 1.23-1.26x.
      (3) `Rev` in the general `cut` (a capture past another delimiter):
      after nearest no measured lane reaches it; tail-recursion-modulo-
      cons construction (Minamide POPL 1998, OCaml's [@tail_mod_cons],
      Scala's ListBuffer) would build the prefix top-down at one object
      a node instead of two, at the price of a write-once field.
