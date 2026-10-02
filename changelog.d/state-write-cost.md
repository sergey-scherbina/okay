## state-write-cost - the 16 B a State write costs is the price of two operations: both candidates refuted, measured

The operator's "optimise later" for State as Get + Update (1.19x, +16 B
a write against the old four-operation loop). Both candidates were
measured alone (history.d `state-write-cost`).

- **A `Put` arm in State's clause:** refuted. The bytes are identical and
  the time is within noise, alternating against master. The JIT already
  removes `Put.apply`'s pair, so the 16 B is the `Put` object.
- **One node per write** (`Put` and `Modified` extending `Update`):
  refuted by the compiler.
  - An extractor that refines the answer type is refutable, so every
    match on State's operations becomes non-exhaustive: six E029 in the
    library, and in every user's own State handler.
  - An irrefutable extractor loses the answer type.

The item is answered and archived in BACKLOG-ARCHIVE.md, with the third
road that would re-open it. No code changed.
