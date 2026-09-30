## indexed-effects-5-docs — docs/typestate.md: the two readings of the indexes, a typestate as data, the indexed row, the transaction with the connection typed

Stage 5 of specs/indexed-effects.md. The user page for indexed
effects: what the tree's two indexes mean under each reading (answer
types, or a state the handler consumes), `PState.Threaded` as the
type-changing state written as data and run by a tail-recursive
handler, the indexed row (`+~`, `Unary`, `State.handleIndexed`),
okay-sql's `Tx.Data` with `Conn[S]` moved only by the driver's
transitions, which road when, and the literature (Atkey 2009, McBride
2011, Danvy–Filinski 1989). Every example line pinned:
`TestDocExamplesTypestate` holds the core ones verbatim, `TestTxData`
the transaction's. Linked from docs/README.md. Gate: the new suite,
TestDocSnippets, TestDocsIndex.
