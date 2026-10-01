- [ ] cont-strict-k — PRIORITY: HIGH (operator: Cont runs on the frame
      machine, optimize from here). The strict `k` — an opaque shift body
      (`k` passed as a value, inside a lambda, a function answer) gets `k`
      as a function that runs the rest NOW, a nested run of the machine —
      is where Cont on the machine is slow: statePara (PState,
      `s => k(s)(s2)`) 1.85-1.90x the old runner, fib100 (a generator over
      Cont: `take` hands `k` out, `put` hides it in a lambda) 2.62x;
      contAnswer (a CPS-transformed body, lazy `k`) is at 1.09x. Per call
      of a strict `k`: a `Resume`, a `Return`, the delimiter node over
      the `Kept`, the machine's entry, the room's save/restore; per
      capture: `Inject`/`Shift0`, the clause closure, a `Kept`, a `Next`.
      Leads, in order: (1) more bodies onto the lazy road — the macro's
      transform (cont-stack-layer1-c's list), above all `PState`'s
      function answer and Generate's `put`; (2) a strict `k` that stays in
      the machine when it is called in tail position of the clause;
      (3) the per-capture objects. Rows: history.d
      2026-10-01T…-cont-on-frames-probe.tsv; profiles in the changelog.
