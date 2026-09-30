## indexed-effects-9-lexical-typed — Lexical's clauses over the program type

`Lexical.Ops/Clauses/ShallowClauses[F, …, P[_]]` answer in `P[R]`:
`Lexical.Unstacked[G]` (`X ! G`) for the unstacked instances, unchanged
in behaviour; `Lexical.Stacked.Below[G, St]` (`Under[G, X, St]`) for the
stacked deep instance, whose `perform` now proves the stack below its
prompt (`Has.Aux[st.S, p.type, S]` with `S` fixed) and hands the clause
the stacked `k` as it is — `at`/`erase` are off that road. Callers
change one type argument (`Unstacked[Delim + G]` for `Delim + G`);
stacked clauses are written `def deep[St <: Tuple]` and the installation
instantiates them. TestLexicalStacked +1 (clauses at another stack
refused). specs/indexed-effects.md stage 9 Results.
Commits: see `git log --grep indexed-effects-9`.
