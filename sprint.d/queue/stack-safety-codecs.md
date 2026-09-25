- [ ] stack-safety-codecs — stage 4 of specs/stack-safety.md, the data
      codecs over VALUES: `PyCodec.enc`/`dec` (okay-py) and
      `RCodec.enc`/`dec` (okay-r) recurse per level of a recursive
      value, and the stage's other modules (okay-sql, okay-pg,
      okay-jdbc, okay-r2dbc) are audited against the inventory
      (specs/stack-safety-okay.tsv). Red first on a small stack with a
      deep value, then an explicit stack or a written bound. Operator
      asked for it with the catch-ups (2026-09-25).
