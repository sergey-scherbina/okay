- [ ] okay2-stage2 — the rest of the core, one stage per file. okay2-async
      and okay2-platform LANDED 2026-09-24 (stage 4); Channel/StmChannel/
      Drain/Source/merge/buffer LANDED the same day (stage 5). The fast
      channels are `okay2-fast-channels`. Resource, Once, Delim and
      Provide LANDED 2026-09-24 (stage 6; the `?=>` doors became
      evidence-first values). Choice/Logic, SharedOnce, Delim.Stacked
      and the Once handler-order tests LANDED 2026-09-24 (stage 7; no
      `runSeq` — a collection is not a kind-`*` Row). Gen LANDED 2026-09-24
      (stage 9, chain fusion and zip ported whole). The monad classes,
      `withFilter`, Tag, Instances and Writer.byValue LANDED 2026-09-24
      (stage 13, at the operator's ask). ON DEMAND FROM
      HERE (operator 2026-09-24: okay2 carries less than okay by
      default and grows when somebody needs something specific) —
      LIFTED for okay's MAIN effects later the same day ("carry on
      porting the main effects"): Refs and Prob LANDED (stage 12),
      Sim LANDED (stage 14), TRef/TMap/TDict/TList LANDED (stage 15),
      Validated/Static LANDED (stage 16), okay2-stm LANDED (stage 17), Produce/Producer/Generate LANDED (stage
      18; Pull was already in since stage 1), Eager and the tagless
      Effects[M] LANDED (stage 19). HMap LANDED (stage 20).
      Remaining: the rest of Stream/Fold. Each stage mirrors the
      Scala 3 suite for that file and lands with its docs section in
      docs/okay2.md. Rule from stage 1: a lone `Inject` is a Bind with a
      pure continuation, construct at the signature (widening is
      subtyping since stage 8), a handler takes `Free[F with R, A]` and
      infers the rest — spelled with `Free`, never `!`/`+`, in a
      parameter. Resource first
      among the effects: okay2-zio's `fromZStream` collects the whole
      stream because a pull that survives across a program's operations
      is a scoped resource, and okay2-fs2/-cats want an `Async` to run a
      program under `IO.blocking` as the Scala 3 interop does. (2026-09-24)
