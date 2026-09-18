- [ ] handlers-fused-walk — road 1 of specs/continuations-roadmap.md:
      one walker over a whole row with a `Handlers[R]` vector, `O(ops)`
      nodes where N nested handle/relay layers re-emit `O(N · ops)`.
      LANE FIRST: a four-effect lane in HandlerBenchmark run as nested
      handle, nested relay, and the fused walk. Ceiling known before
      starting: handler-fusion measured nesting at 1.1–1.3x, so this is
      allocation plus that, not a third; under 1.1x it is a refutation.
