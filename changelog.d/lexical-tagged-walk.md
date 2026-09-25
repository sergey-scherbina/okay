## lexical-tagged-walk - `Lexical.walk`, an optional fourth strategy: 1.29x the row, correct or loud

Operator: add it optionally, not as the default.

- `Lexical.walk` / `Lexical.State.walk`. Instance operations are inert
  `Inject(Local.Op(owner, e))` nodes of ONE shared signature, `Local`.
  The installation walks its body like a row handler, with the state
  threaded purely: no cell, no guard, no `Delay` per operation.
  `Lexical.runLocal` at the top turns an escaped operation into
  `LocalEscaped`. There is one cast, justified by identity.
- Bytes per 1000 get/set: row 222 040, walk 286 192 (1.29x), tail 366 225
  (1.65x).
- The backlog's own caveat was refuted by the tests: a walk never answers
  silently differently from deep. Across the installation it gives deep's
  per-branch answer. Inside it, the operation inside a delimiter's body
  escapes loudly (machine outside), or the walk threads the state as
  deep does (machine inside).
- TestLexicalWalk (8), all predicted by hand, and a mutant watched failing.
  docs/many-instances.md describes it as a by-name strategy with the
  spine rule.
