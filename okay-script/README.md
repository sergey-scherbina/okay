# okay-script

Markdown files as Scala source (specs/okay-script.md; the road from an empty directory is [the guide](../docs/okay-script-guide.md)). A `.md` file with fenced ` ```scala ` blocks is a literate program: the blocks are extracted, concatenated in document order, and compiled by the REAL Scala 3 compiler in-process (`dotty.tools.dotc`), then run. No new language and no interpreter — markup metadata extraction, minimal preprocessing, meta-compilation.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-script.md`](../docs/modules/okay-script.md) | what it is, and the reasoning |
| [`specs/okay-script.md`](../specs/okay-script.md) | the design and its decisions |
