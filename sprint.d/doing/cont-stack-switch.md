- [ ] cont-stack-switch — stage 1c of specs/stack-safety.md: `Cont`
      past the stack, specs/cont-stack.md (the operator's design day,
      2026-09-25; claim `.work/active/cont-stack-switch.claim`, worktree
      `../okay-wt-css`). GOAL: no StackOverflowError from `Cont` at any
      depth on JVM and Native, JS bounded in writing; no exception
      unwinds, nothing runs twice; API unchanged; fib100/fib1000/
      statePara within noise of master. THREE STAGES, LANDED TOGETHER
      (spec "Stages" 1–3), only within noise and with `verifyJdk17`
      green:
      1. Layer 2, the room and the switch — DONE in the lane
         (the lane's first commit): `room` as a parameter and a `Reentry` field, the
         switch at zero; TestContStack green; A/B measured (history.d
         2026-09-25T091202Z): fib100 1.12x, fib1000 1.00, statePara
         9.99x from a first room of 256 that switched ~12 times a run.
      2. Layer 1 A — `shift` as a macro, tail-position bodies return a
         `Jump` and count nothing; 1M shifts on a 128 KB stack with ZERO
         switches (a switch counter in the test's StackSwitch double);
         target statePara back to master's number.
      3. Layer 2 on a 1 GB PLATFORM thread on every JDK (Decision 8:
         virtual threads refuted, 15–20x slower a level on one JDK) and
         Layer 3 on every platform (the spec's matrix): `StackRoom.left()`
         in the core; the JVM `StackSwitch` with a `ThreadStackSize`
         first room and `-Dokay.cont.room`; the FFM reader as the core's
         `jdk22/StackRoom.scala` Multi-Release variant
         (`versioned("okayJdk22", "jdk22", 22, "okayJVM")` +
         `multiRelease`, landed by mrjar-jdk25-ci-gap 565a98465), gated
         on `Module.isNativeAccessEnabled`, macOS arm64 layout first;
         the Native `StackSwitch` reading the runtime's `ThreadInfo` +
         `stackalloc`; `worst` bytes-a-level carried beside the room;
         then shrink the fast path (`Mapped`, `Reentry`) and A/B again.
      Behaviour boxes are in the spec; each new box red first. The lane
      rebases on master for `versioned` before stage 3 starts.
