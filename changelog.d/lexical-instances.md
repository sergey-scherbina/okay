## lexical-instances - handler instances as prompts (POPL 2020), every strategy named (stage 0)

specs/lexical-instances.md. Operator rule for the arc: every way of
running a handler is an explicit primitive or combinator, a default is
built from them later, and the manual choice stays.

- `okay.Lexical`: an installation is a fresh prompt, and the body gets
  its `Inst` as a lambda parameter. `Lexical.deep` (shift0 under a
  `dollar`, the return clause as its return function) and
  `Lexical.shallow` (control0, where the clause re-installs) run
  user-written `Clauses`. `Lexical.State` is the worked instance.
- The effect never enters the row, so there is no `TypeableK`, no
  `Distinct`, and no misrouting. Two `State[Int]` are two names, and
  an operation addressed to an outer instance passes an inner one of
  the same effect untouched (no accidental handling).
- TestLexical (7): two instances in both strategies, the row refusal
  (checked against its real message), no accidental handling, `Bisim`
  equivalence with `State.handle`, a multi-shot user effect, and
  10 000 operations in constant stack.
- docs/many-instances.md: the "fresh prompt" route has a real API now,
  with examples and the price. The backlog item
  effect-instances-tunnelling notes the progress.
