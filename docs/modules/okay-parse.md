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
- Incremental reparse: `Parse.full(scan, instrs)(input)` keeps
  `(tokenIndex, Building)` snapshots at root-level node boundaries —
  the builder is persistent, a snapshot is a pointer — and
  `Parse.reparse` relexes (okay-lex reconvergence), resumes from the
  nearest boundary before the damage and SPLICES once the token
  stream is the old one again at a matching boundary:

```scala
val session = Parse.full(JsonLex.scan, JsonParse.instrs)(doc)
val re = Parse.reparse(JsonLex.scan, JsonParse.instrs)(
  session, doc, edited, editStart, editEndOld, editEndNew)
// a length-preserving edit reuses untouched subtrees BY REFERENCE
// (eq); a length-changing one rebases their spans. O(damage) driving.
```

  The contract that makes token-level reconvergence sound: the driver
  is a PER-TOKEN function (`JsonParse.instrs: Token => Vector[Instr]`,
  no cross-token state) — all parser state lives in the builder, and
  the builder is the thing snapshotted.
