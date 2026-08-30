# okay-rag

> Retrieval built from our own primitives: documents split over the
> lossless CST, so a citation is a byte range that cannot drift; code
> indexed by parsing it, so re-indexing costs the edit and not the
> file; and symbol search that needs no embeddings at all.

Depends on: `okay-codec` (and through it the whole total text stack).
Pure Scala — cross-built for JVM and JS.

## Guide

**Split the tree, not the string.** `Split.structural` packs sibling
subtrees greedily while they fit the budget and ENTERS a subtree that
does not, so boundaries land on structure — a heading's section, a
JSON member, a whole definition — and never mid-word. Each segment
carries the structural path that located it, which is the best free
metadata a retriever can have.

**Provenance is a law, not an intention.** Every token has an exact
span and the CST is lossless, so a segment records the byte range it
came from and `Segment.quotes(source)` is checkable: the segment's
text IS `source.substring(span)`. Tests assert it at several budgets
across Markdown, JSON, YAML and code. Splitters that regex over
normalized text cannot make this promise.

**Code is the proving corpus.** `Code` is a definition-boundary
grammar — braces, comments, strings and a handful of keywords — which
is enough to cut a file into whole definitions and name them. It is
only sane to build such a partial grammar because the parser is
TOTAL: what it does not understand becomes ordinary leaves or error
nodes, never a failure, so precision can be sharpened later without a
rewrite. Doc comments are held by the driver and adopted by the
definition that follows, so they land inside the node.

**Re-indexing costs the damage.** The corpus changes constantly —
because the agent is editing it. `Code.reparse` takes the edit range
(exactly what an agent's edit tool already knows), relexes by
reconvergence, resumes the driver from the nearest snapshot and
splices: measured, a one-literal edit in a 40-definition file
re-drives under half its tokens, and the untouched definitions keep
their exact text, so their embeddings stay valid.

**Symbols need no vectors.** `Symbols.Index` collects definitions and
mentions by a `Fold`, and is a `Monoid` — the index of a project is
the merge of the indexes of its files, which makes distribution and
incremental update the same operation. "The definition of X" and
"what mentions X" are exact answers; semantic search covers the other
half and the two fuse later.

## Tutorial

```scala
import okay.rag.*

val src = Source("Greeter.scala", scala.io.Source.fromFile(f).mkString)

// whole definitions, each quoting the file exactly
val segs = Split.structural(src, Code.parse(src.text).tree, budget = 400)(_.length)
segs.foreach(s => assert(s.quotes(src)))

// structural retrieval, no embeddings involved
val idx = Symbols.project(files)
idx.definition("hello").map(Symbols.segment(_, src))   // the code, exactly
idx.mentions("Greeter")                                // where it is used

// the agent edits a file; re-index the damage, not the file
val fresh = Code.reparse(session, oldText, newText, editStart, editEndOld, editEndNew)
```

Prose and data split the same way, with the dialect's own parser:

```scala
Split.structural(doc, Markdown.parse(doc.text), 800)(countTokens)
Split.structural(cfg, Yaml.cst(cfg.text), 200)(_.length)
Split.windows(doc, bpe, budget = 512, overlap = 64)   // the classic shape, exact
```

## API reference

| member | signature | meaning |
|---|---|---|
| `Source` | `(id, text)` | the original document, kept whole |
| `Segment` | `(source, span, text, path)` | a retrievable piece with exact provenance |
| `Segment.quotes` | `Source => Boolean` | the provenance law, as a method |
| `Split.structural` | `(src, cst, budget)(size) => Seq[Segment]` | pack siblings, enter what does not fit |
| `Split.windows` | `(src, scan, budget, overlap) => Seq[Segment]` | token windows, exact under any Scan |
| `Split.covers` | `(src, segs) => Boolean` | every character accounted for |
| `Code.scan` | `Scan[Code.K, Code.S]` | brace-family scanner (doc comments, strings, braces) |
| `Code.step` / `initD` / `finish` | `Parse.Step[K, D]` | the definition-boundary driver |
| `Code.parse` / `Code.reparse` | full and incremental | a session, and the damage-priced update |
| `Symbol` | `(name, kind, source, span, path)` | one definition |
| `Index` | `defs`, `refs`, `merge`, `definition`, `mentions`, `names` | the symbol index (a Monoid) |
| `Symbols.of` / `project` / `fold` | one file, many, or streaming | build the index |
| `Symbols.segment` | `(Symbol, Source) => Segment` | the code a symbol names |

## Gotchas

- `Split.structural` may exceed the budget when a document's own
  atoms do (a single enormous string literal) — reported honestly
  rather than cut mid-token.
- The code grammar is deliberately partial: it finds definition
  boundaries and names, not types or scopes. Unknown syntax becomes
  ordinary leaves, which is why it degrades instead of failing.
- Overlapping window splits intentionally break `covers`-style exact
  reassembly; use `overlap = 0` when you want the concatenation law.

## Not yet (specs/rag.md)

Embeddings (`Embed` as an effect) and the `VectorStore` interface
with a reference in-memory implementation (P10b), retrieval pipelines
with fair interleaving and RRF fusion (P10c), resilient ingestion
(P10d), keyword/hybrid (P10e), and the retrieval-augmented `recall`
that puts code in the agent's context without a tool call.
