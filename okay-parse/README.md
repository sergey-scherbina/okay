# okay-parse

Total, error-tolerant, lossless parsing: any token stream yields a tree; what did not parse is IN the tree as error nodes; a truncated stream is a tree with holes. Incremental reparse reuses unchanged subtrees by reference.

**Depends on:** `okay-lex`. Pure Scala — cross-built for JVM and JS.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-parse.md`](../docs/modules/okay-parse.md) | what it is, and the reasoning |
