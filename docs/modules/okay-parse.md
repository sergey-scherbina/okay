# okay-parse

Total, error-tolerant parsing: any token stream yields a tree.

- `Instr` (Open/Emit/Close/Bad) — the ONE instruction language both
  surfaces emit: the hand-written driver (`JsonParse.driver`) and the
  combinators (`JsonParse.combinators`) produce the same CST, damaged
  inputs included.
- `Parse.build` — the TOTAL builder: a close with nothing open is an
  error leaf; unclosed nodes are closed by `present` with markers —
  a truncated stream (the LLM case) is a tree with holes, never a
  fault. `Cst.lexemes` is the lossless law; `Cst.errors` collects the
  diagnostics FROM the tree.
- The pipeline is stage composition:
  `through(through(chars)(Scan.stage(...)))(driver)` |> `Parse.toCst`.

Incremental reparse (node-boundary snapshots over lex reconvergence)
is the stated follow-up.
