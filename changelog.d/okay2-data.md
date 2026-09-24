## okay2-data - okay-data in okay2: Sketch, Hlc, Uid

The first lane of porting the types okay2 still lacked (operator:
"портируй"). A new subproject `okay2-data`, as okay-data is its own
module in okay: the approximate aggregators `Sketch.hyperLogLog`,
`countMin` and `tDigest` (Aggregators, so they merge and zip with the
exact ones), the hybrid logical clock `Hlc`, and the sortable id `Uid`
(UUIDv7 and ULID) over it. `Hlc.Stamp` is a value class where the core
has an opaque type (specs/okay2.md stage 23). 24 tests.

Docs: docs/okay2.md section 27.
