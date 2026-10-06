- [ ] **semantic-expressions** — execute previously refused Ossie analytics expressions across data sources.
  User: remove constant/division, scalar/conditional/aggregate/window and cross-dataset limitations.
  Design: specs/semantic-expressions.md; extend core constants, add portable compiled expression plan with typed bindings, checked relationship enrichment and dialect/function facades. Keep source SQL inert.
  Done when: regression cases for every previously named class, empty/null/precision/fanout/window ordering and limits, JVM/JS/Native parity, affected staged gate and docs green. Main contains sibling changes; commit only explicit paths.
