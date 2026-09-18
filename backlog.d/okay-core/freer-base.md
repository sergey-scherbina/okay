- [ ] freer-base — specs/freer-base.md. STAGE 0 IS LANDED: `Cont` is
      `Freer[Shift, …]`, one enum, one absorption rule, at parity or
      better on every core lane. NEXT IS STAGE 1: `Free` on the same
      base, `A ! F` an alias at a pinned `Unit` index, `object !`
      exporting the cases so the 89 `(x.resume: @unchecked)` sites and
      the 20 files outside the core compile unchanged, and the
      rotation law (already written, `TestFreer`) extended to the Free
      side. It is the bigger half and the one the design is for: the
      rotation still exists FOUR times (`Free.fold`, `runFree`,
      `!.resume`, Async's loop) and stage 1 is what makes it one.
      Three findings from stage 0 apply to it directly and should be
      used rather than rediscovered: a hot loop's BYTECODE SIZE can
      matter more than its data (relay's 305-vs-325 cliff), a `map`
      that misses its carrier's own path costs a node per element, and
      one `apply` body beats two because the JIT counts call targets,
      not receiver types. Stage 2 (the indexes as typestate, Delim
      first) is independent and does not block it.
      SUPERSEDED THE SAME DAY by cont-on-free: `Freer` deleted, `Free`
      kept as the base untouched, `Cont` an opaque facade over
      `Free[Shift, A]` with the indexes on the facade — every core lane
      within 1% of master, allocation identical to the byte, two
      trusted lines under one invariant. Stage 2 (typestate) is now one
      more facade over `Free[F, A]` and cannot leak the way stage 1 did.
      STAGE 2's LANGUAGE QUESTION IS ANSWERED — YES (2026-09-15,
      stage2-probe), asked BEFORE claiming the lane because stage 1
      died on this class of question after its implementation was
      written. A prompt's IDENTITY does reach the type level, so
      `NoPrompt` can become a compile error:
      `scripts/stage2-prompt-identity-probe.scala` runs it — five
      positives compile, three negatives are refused, including a
      prompt that ESCAPES its reset, which is exactly today's throw.
      Four compiler facts were paid for and are in specs/freer-base.md
      so nobody re-buys them: a for-comprehension HEAD has no expected
      type (so the stack must be a given, not an inferred parameter); a
      CURRIED dependent context function is refused outright; a
      non-curried one compiles but CRASHES dotty when it carries the
      stack (`wildApprox failed to remove uninstantiated R`); and a
      `using` clause after the continuation loses to the lambda's own
      typing, so it goes before and the stack is a type MEMBER.
      COST AT THE CALL SITE, measured by writing it: `reset { p => … }`
      becomes `reset { s => import s.given; … }`, one line per reset.
      STILL UNPRICED, and the lane must not assume them: `shift0` and
      `control0` CONSUME the delimiter so their index is unbalanced and
      the probe never exercised it; `abort` still drops a promised
      transition (the spec's own caveat, already a required test); the
      four files outside the core that name Delim (okay-ui `Scope`,
      `Screen`, okay-agent `Stepper`, okay-llm `Cut`) pay the per-reset
      line and none was read; and nothing is measured.
      The refutation below is kept because it is why the facade is the
      shape, not a step toward something else.
      STAGE 1 IS REFUTED AS SPECIFIED (2026-09-15, branch
      `feature/freer-base-stage1`, WIP commit kept and never to be
      merged). `Bind` carries the LEFT side's answer index, so a match
      on a `Free` hands back its continuation at an existential index
      while all 89 sites want `A ! F`. An existential outer index
      fixes elimination and breaks construction; a pinning `unapply`
      is refuted by the compiler, which infers its free parameter as
      `Nothing` instead of skolemizing, so the link between an
      operation's answer type and its continuation is lost. The one
      road that would work is a UNIFORM-INDEX bind case in the base
      (`Seq[G, A, B, R]`), and it costs what stage 1 was for: `Cont`
      still needs the non-uniform `Bind` for `PState`, so the base
      carries both and `resume` rotates both. THAT IS A DECISION, not
      a task — specs/freer-base.md Results has the full reasoning. ONE enum `Freer[G, A, S, R]` — Pure,
      Op, Bind, Defer, one `resume` rotation — under `Cont`
      (`Freer[Shift]`, index = answer type) and `Free` (`Freer[Lift[F]]`
      at a pinned `Unit` index, later typestate). Three stages, each
      its own lane with its own gate and disqualifying numbers in the
      spec: 0 = enum + Cont (absorbs cont-fuse-one-step below: fusion
      becomes the `Shift.Absorbed` class, no budget, 40 B/shift vs 48);
      1 = Free on it, `object !` exports the cases so the 89 `resume:
      @unchecked` sites and 20 outside files compile unchanged, a
      rotation law lets eliminators inline; 2 = the index as typestate,
      Delim first (`NoPrompt` → compile error). Start with stage 0.
