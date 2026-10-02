- [ ] state-get-update — the operator, 2026-10-02: "оставь у State только
      Update и Get". `Set(s)` is `Update(_ => (s, s))` and `Modify(f)` is
      `Update(s => { val n = f(s); (n, n) })`: they existed for speed, a value
      instead of a closure and a pair. `State.set` / `State.modify` keep
      their signatures and build an `Update`. Every handler matching `Set`
      or `Modify` moves to `Update`. The cost of a write is measured before
      and after, one lane at a time, and recorded. After handler-apply.
      (2026-10-02)
