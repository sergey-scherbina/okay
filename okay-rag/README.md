# okay-rag — retrieval built from the library's own primitives

Documents are split over the LOSSLESS concrete syntax tree, so a
citation is a byte range that cannot drift. Code is indexed by parsing
it, so re-indexing costs the edit rather than the file. And symbol
search needs no embeddings at all.

## The pieces

| | |
|---|---|
| `Split.structural` | packs sibling subtrees greedily while they fit the budget and ENTERS one that does not — boundaries land on a heading's section, a JSON member, a whole definition, never mid-word |
| `Ingest` | a source into segments: parse, then split |
| `Keyword` | BM25 over an index that is a MONOID — halves of a corpus combine, so indexing forks and merges |
| `Symbols` | search by what the code declares, with no vectors involved |
| `Store` / `Retrieve` | the vector side and the combination, behind a seam a database can fill |

## Indexing and searching, with no model in sight

```scala
import okay.rag.*

val files = Seq(Source("Greeter.scala", "class Greeter(name: String) { … }"))
val segs = files.flatMap(f => Ingest.segment(f, 400)(_.length))

val index = Keyword.index(segs)
val hits = Keyword.search(index, "network requests", 3)
hits.map(h => (h.segment.source, h.score))
```

The index being a monoid is not a flourish: index two halves on two
fibers, combine them, and the search answers exactly what indexing the
whole would have.

## Further

| | |
|---|---|
| [`docs/modules/okay-rag.md`](../docs/modules/okay-rag.md) | the guide: splitting the tree rather than the string |
| [`specs/rag.md`](../specs/rag.md) | the design and its decisions |
| [`okay-codec/`](../okay-codec) | the lossless CST the citations come from |
