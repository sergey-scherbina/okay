## chunks-foldwriter-docs-condense - the two chunk-fold docs say what they are

`Chunks.foldLeftWriter` and `Chunks.foldWriter` carried ~130 lines of
the investigation that built them — three shapes tried, the profiler
findings, two corrections of earlier paragraphs — in a 624-line file.
Each doc now says what the combinator IS (a G-effectful chunk fold for
`Source[Chunk[A]]`; `foldWriter` JVM/Native only, since its eager walk
needs `CanBlock`), why its shape is the fast one in one paragraph, and
points at specs/producer-to-writer-carrier.md's Results for the
numbers and the history. Chunks.scala: 624 -> 490 lines, no code
changed.

Two backlog entries retired with it: `foldwriter-js-incompatible`
(moot since the `Chunks` retype — every `Chunks.fold` call site is
pure; the spec's Results records the JS question for a G-effectful
caller that has not appeared) and `chunks-foldwriter-docs-condense`
itself.

Files: okay-stream/src/main/scala/Chunks.scala, backlog.d/okay-core/
foldwriter-js-incompatible.md and chunks-foldwriter-docs-condense.md
(deleted).
