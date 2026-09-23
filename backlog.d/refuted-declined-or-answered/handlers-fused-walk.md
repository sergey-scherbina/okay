- handlers-fused-walk — DECLARED POINTLESS 2026-09-23 by specs/continuations-roadmap.md
  Results ("where staging is pointless: road 1, `!.runAll` over a vector of
  handlers, capped 1.1–1.3x by handler-fusion's own data"); `Handler.flat`
  (handler-fusion-flat, 2026-09-22) is the row handler that landed instead,
  1.08–1.24x, arc closed. Moved here by backlog-audit-0923. Was: road 1 of
  specs/continuations-roadmap.md:
      one walker over a whole row with a `Handlers[R]` vector, `O(ops)`
      nodes where N nested handle/relay layers re-emit `O(N · ops)`.
      LANE FIRST: a four-effect lane in HandlerBenchmark run as nested
      handle, nested relay, and the fused walk. Ceiling known before
      starting: handler-fusion measured nesting at 1.1–1.3x, so this is
      allocation plus that, not a third; under 1.1x it is a refutation.
