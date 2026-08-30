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

## The retrieval layer

**Embedding is an effect, the store is an interface.** `Embed` has
one operation (a batch in, a batch out), so nothing here pulls a
model runtime and the tests run offline against a deterministic
hashing handler. `MemoryStore` is the reference implementation:
brute force with top-k done by the aggregator we already had, keyed
by source+span so re-indexing a definition replaces rather than
doubles, and persisted through our own codec (CBOR to ship, JSON to
read). Honest scope — linear to ~10^5 segments; ANN and real
databases are adapters behind the same interface.

**The keyword side has the same shape as everything else.** An
inverted index is a `Fold` and a `Monoid`, so it distributes and
merges; BM25 reads its counts and nothing more. Fusion is reciprocal
rank — an `Aggregator`, which makes fusing two machines' results the
same operation as fusing two local retrievers, and needs no
comparable scores.

**Pipelines are combinators, not classes.** `Retrieve.vector`,
`.keyword`, `.symbols` (exact, no embeddings), `.hybrid` (fused),
`.multiQuery` (rewrites explored as `Choose`, then fused), `.fair`
(`Logic.interleave`, so a prolific retriever cannot starve a precise
one), `.rerank` (any scorer).

**Ingestion is a program**, so retries and fibers wrap it rather than
live inside it; `Ingest.reindex` closes the loop the code corpus
opened — reparse by damage, keep the vectors of segments whose text
did not change, delete the stale spans.

```scala
given Handler[Embed] = Vectors.hashingHandler()      // or a provider
val store = MemoryStore()
Ingest.run(store, files)(_.length).runWith           // split, embed, upsert
Retrieve.vector(store).retrieve("multiply numbers", 3)
```

## Grounding an agent

`okay.agent.Grounded.context` makes retrieval part of `recall`: the
conversation is compacted by its policy, the last question drives a
retrieval, and both go through ONE budget with `share` naming how
much retrieval may take. The agent never asks for code — it has it —
and the explicit search tool remains for when it wants to steer.

## Not yet (specs/rag.md)

Passages kept as LINEAGE: a `Segment` already carries the exact span,
so re-observing more of a document is a substring of its `Source`;
wiring that in as a follow-up capability (the model sees a
projection, a follow-up widens it without a new retrieval) is what
remains.
