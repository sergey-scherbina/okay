- [ ] foreign-one-r — stage 1 of specs/foreign-one.md: R onto the one
      engine. okay-r has its own engine (`RSubprocess`), value enum
      (`RValue`), codec (`RCodec`) and shim — ~2 300 lines saying what
      `ForeignWorker`/`PyValue`/`PyCodec`/shim.py say, sharing only
      `okay.codec.Wire*`; every wire feature is built twice (wire-givens-r
      was one) or not for R (gateway, TCP, `WireAuth`, TLS,
      `CrashConformance`: docs/one-language.md "Limits"). shim.R speaks
      the engine's protocol with additive value escapes
      (`{"t":"na","of":...}` for a typed NA; NULL is `null`; a named list
      is `dict`); `RSubprocess` becomes `ForeignWorker.speaking(Rscript …)`
      with R's timeout as `WireDeadline` + `supervised`; `okay.r.R` a
      facade over `okay.py.Foreign` as `Ts` is; `REval` an alias of
      `ForeignEval`; `RCodec` = `PyCodec` at `Shape.r`. Also deletes the R
      twins the cluster grew (`RStage`, `RReducer`, and feature/
      foreign-streams-holds' `RStreamer`/`RModel` once landed) — they
      differ from the Python ones by `REval`/`RFrame`/`::` only. Gate: all
      okay-r suites green unchanged (NA ≠ NULL at every depth), R a full
      row of the one-language table. A deletion, not a bridge (spec
      Decision 7). FIRST of the stages: everything after has one engine.
