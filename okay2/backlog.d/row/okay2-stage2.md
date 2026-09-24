- [ ] okay2-stage2 — the rest of the core, one stage per file. okay2-async
      and okay2-platform LANDED 2026-09-24 (stage 4); Channel/StmChannel/
      Drain/Source/merge/buffer LANDED the same day (stage 5). The fast
      channels are `okay2-fast-channels`. Resource, Once, Delim and
      Provide LANDED 2026-09-24 (stage 6; the `?=>` doors became
      evidence-first values). Then the rest, in order of use:
      Choice/Logic (`runSeq`, the CanFail witness for `if` guards in a
      `for`; the Once handler-order tests wait for it), SharedOnce
      (okay-async), Gen, Stream/Fold, Prob, Sim,
      Validated/Static, Eager, Refs, HMap, Tag. Each stage mirrors the
      Scala 3 suite for that file and lands with its docs section in
      docs/okay2.md. Rule from stage 1: a lone `Inject` is a Bind with a
      pure continuation, construct at the signature and widen with
      `at`, handlers take the row anywhere via `Remove`. Resource first
      among the effects: okay2-zio's `fromZStream` collects the whole
      stream because a pull that survives across a program's operations
      is a scoped resource, and okay2-fs2/-cats want an `Async` to run a
      program under `IO.blocking` as the Scala 3 interop does. (2026-09-24)
