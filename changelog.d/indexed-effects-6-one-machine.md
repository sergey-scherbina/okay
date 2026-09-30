## indexed-effects-6-one-machine — one Delim machine: the unstacked doors enter the typed one, the old machine deleted

Stage 6 of specs/indexed-effects.md, the operator's "Продолжай".
`Delim.run`/`runNested` route their program through `Stacked.at` onto
the typed machine; the unstacked machine (326 lines of Delim.scala:
Segs, Frames, Cut, split/copy, reify, loop, step over `Delim` alone)
is gone. Two reds on the way, both structural: the embeddings must be
IDENTITIES (lazy node-per-op rewrites wrapped each resumption's
continuation one layer deeper and Lexical's depth test ran out of
heap), so `at`/`erase` are casts with their argument in `Stacked` and
`Indexed.lift` stays for `Tx.Data.async`; and a dollar's `ret` must be
re-based as a function value, not wrapped (10 000 nested closures
overflowed the stack at the end of the same test). `runNested` keeps
the one forwarding cast for embedded captures. Gate: the Delim family
79/79, no warnings; the full `affected master staged`.
