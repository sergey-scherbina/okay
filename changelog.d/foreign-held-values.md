## foreign-held-values — Go, Rust and Haskell workers hold values (2026-09-26)

The compiled workers' libraries keep a table of held values: a call made
`held` keeps its answer and answers a ref, a ref among a call's or a
program's arguments is that value again, `release` drops it, and a ref the
worker does not hold is refused by name. WireConformance gained the held
case, answered by every row that has calls (Python, TypeScript, R, Go,
Rust, Haskell). Through the facade a compiled worker now has `Holds` and
`Models`; no `Methods` (a value has none by name) and no `Stateful` (a
held value is not changed in place). specs/foreign-one.md Decision 23.
