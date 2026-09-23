- [ ] foreign-journalled — `Journalled[PyEval]` and `Journalled[REval]`
      instances, so a Python or R call is journalled and replayed by
      `Durable` like a tool. The seam has existed since
      durable-any-operation (okay-agent's `Journalled`, whose only
      production instance is `Tool`); the instances are all that is
      missing, and the answer codec is the Wire each module already has.
      Makes true what specs/r.md, specs/py.md and a LinkedIn draft
      (2026-09-23, caught before posting) promised. FIRST of the
      Python/R items: small, and the others build on replay.
