- [ ] okay2-stage2 — the rest of the core, one stage per file, in order
      of use: Choice/Logic (`runSeq`, the CanFail witness for `if`
      guards in a `for`), Resource, Once, Delim (multi-prompt — the
      `Prompted ?=>` scopes become an explicit scope parameter, since
      Scala 2 has no context functions; specs/okay2.md names the 79
      `?=>` sites of the Scala 3 core), Gen, Stream/Fold, Prob, Sim,
      Validated/Static, Eager, Refs, HMap, Tag. Each stage mirrors the
      Scala 3 suite for that file and lands with its docs section in
      docs/okay2.md. Rule from stage 1: a lone `Inject` is a Bind with a
      pure continuation, construct at the signature and widen with
      `at`, handlers take the row anywhere via `Remove`. (2026-09-24)
