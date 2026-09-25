- [ ] lexical-tail-guard-abort — BUG (hypothesis from reading the code,
      2026-09-25, operator approved the plan). `Closing.guarded`
      (Lexical.scala) guards a `tail` instance against multi-shot from
      OUTSIDE with a `dollar` whose `ret` sets `returned = true`. `ret`
      runs only on a NORMAL return through the `Segs.Ret` frame. A second
      resumption that leaves the body by `abort` or a `shift0` to an
      outer prompt drops its `k`, so `ret` never runs and nothing throws,
      while the cell was already mutated by the first resumption. Shape:
      outer `reset` p0 with `shift(p0)(k => k(()) ... k(()))`, inside
      `Lexical.State(0)`, body `set(get + 1)` then `abort(p0)(get)`:
      deep gives (1, 1), tail is expected to give (1, 2) with no
      `MultiShotAcrossTail`. STAGES: (1) the test in TestLexical, RED on
      master — if it is green the hypothesis is wrong and the lane closes
      with a Results entry; (2) count ENTRIES, not exits: the machine
      calls `(v: a) => reify(r.close, …)` (Delim.scala, the AtRet resume
      closure) once per resumption, so either `Segs.Ret` carries an
      optional `enter` hook that `dollar` takes, or `tail` marks a
      generation in `perform` — pick by price, delimGenerator and
      stateLexTail bytes as on master; (3) specs/lexical-instances.md
      Decisions/Results, box "the guard counts resumptions, not returns".
      Gate: `scripts/gate.sh "affected master staged"`. DONE WHEN: the
      test is red on master and green on the lane, delimGenerator's
      B/op unchanged (910 330).
