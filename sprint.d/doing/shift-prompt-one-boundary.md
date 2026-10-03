- [ ] shift-prompt-one-boundary — PRIORITY: MEDIUM, MEASURED 2026-10-03
      (cont-atm, history.d cont-atm-machine). On `Delimited` a Shift prompt
      in force is TWO value boundaries (its opening and its closing) with
      a `ret` frame between, so a capture can skip `ret` (λ$'s `S0 k.e`).
      `shift` = `shift0` + `push` installs one per capture, so a generator
      pays it per yield: DelimBenchmark.delimGenerator 1.30x master's λ$
      machine (70.7 vs 54.4 us), 815 KB vs 518 KB a run (~+300 B a yield).
      Per capture: `Piece.Nil` + 2 `Snoc`, `Found`, `Cut`, the `_ eq p`
      lambda, `Resumption`; per resumption: `Return`, `Resume`, `Inject`,
      `Nested`, `Delay`, a `Delim` per boundary relinked, `Next`s. Leads:
      (1) a `reset` (`ret` = pure) as ONE boundary — a capture then has no
      `ret` to skip; the catch is telling it from a `dollar`'s opening
      whose closing lies under it (a reset of the same prompt run inside
      that dollar's `ret` sits right above its closing): a mark of its own
      per installation kind, not a per-prompt `close`; (2) the prompt's
      predicate once per prompt, not a lambda per capture; (3) `Resume`
      as its own `Pending` (no `Nested` beside it). Everything else on the
      machine is faster than master: statePara 0.73x, fib100 0.74x,
      contAnswer 0.89x, stateForeign 0.91x; the folds at parity.
