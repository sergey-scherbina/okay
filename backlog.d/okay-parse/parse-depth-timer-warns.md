- [ ] parse-depth-timer-warns — `TestParseDepth.timeMs(body: => Unit)`
      makes its four callers discard a `Parsed`, which is four E175s
      on every platform and therefore a warning on master that every
      landing inherits. It is NOT a mechanical fix: making the helper
      generic was tried (47dbc639) and reverted, because the test
      then read 27.4x where it asserts under 8 and the honest reading
      is that a TIMING test is the one place AGENTS.md forbids
      rewriting to please a linter. Whoever owns this test should
      change it and re-establish its measurement in the same lane —
      the `: Unit` ascription at the call sites is the candidate that
      generates the same code the implicit discard did.
