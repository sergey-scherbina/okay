- [ ] handle-values-rest — level 1, the operator's order (2026-10-02),
      after state-get-update and handler-shape, together with
      docs-level1-language. `p.handle(h)` takes `State`, `Reader`, `Writer`,
      `Throws`, `Choose`, `Maybe` and `Reset` off the row. The other ready
      effects still need `X.run(p)`: `Async`, `Resource`, `Once`, `Supply`,
      `Prob`, `Chronicle`, `Gen`. Give each a handler value, so that one
      path holds for every effect. specs/api-levels.md. (2026-10-02)
