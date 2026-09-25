- [ ] foreign-one-value — stage 2a of specs/foreign-one.md (split from
      foreign-one-protocol, 2026-09-26): R JOINS THE ONE VALUE TREE, EFFECT
      AND HANDLER. R's user API (`R.fn`/`program`/`hold`/`callback`/`stage`)
      is Python's API at another codec, and okay-py already has that seam
      (`Shape`, which `Ts` uses). So: `PyValue` gains R's typed `NA(of)`
      (wire `{"t":"na","of":...}`); shim.R adopts the shared value tags
      (integer plain, integral double `f`, `dict`, `bytes`, `int` digits)
      and the host still READS R's old tags, so old journals replay;
      `RCodec` becomes `Shape.r` over `PyValue`; `RValue`, `REval`,
      `RStep`, `RNode`, `RRef`, `RFrame`, `okay.r.Condition` become aliases
      (R-named extractors, so R code and tests read unchanged); the frame
      codec is R's columnar v2 for everyone (the host writes it, Python's
      shim and the TS worker read it); `RSubprocess` becomes a
      `ForeignWorker` over R's session, supervised when it has a deadline —
      so R's own `Kont` replay is deleted (stage 3's R half, done here) and
      R recovers from a DEATH too. Gate: every okay-r suite green unchanged
      (Live, docker R), okay-py Live green, R a row of `WireConformance` and
      `CrashConformance`.
