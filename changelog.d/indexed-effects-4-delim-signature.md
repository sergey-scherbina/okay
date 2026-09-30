## indexed-effects-4-delim-signature — Delim.Stacked is a typed machine over the indexed tree: the operations with their types, no facade, the two casts gone for them

Stage 4 of specs/indexed-effects.md, the operator's "Да ок".
`Delim.Stacked.Op[F, S, R, +X]` carries `Push`/`Dollar`/`Capture` with
exact payloads (the row's other half `F` is a parameter, fixed at
`run`), every operation on the diagonal, `Under[F, A, S]` the tree
itself; the machine is the unstacked one's port over `Op | Delim | F`
— typed operations at their own types, embedded unstacked ones at the
two claims they always made, foreign ones suspended into the residual.
Found: a positional witness (ProbeDelimTyped) is refuted by `shift`'s
re-installation, so the cut stays the identity search and the
segments are typed by answer types; `rebase` is the one claim (presence
is monotone); `at`/`under` and `erase` are the two embeddings Lexical's
unstacked clauses need. `Lexical.Stacked` and `Layered.Stacked` moved
(`Prog.diag` → `at`, `.free` → `erase`); the unstacked `Delim` and its
machine untouched; routing them through this machine is the named
follow-up. Gate: eight suites 48/48, `affected master staged`.
