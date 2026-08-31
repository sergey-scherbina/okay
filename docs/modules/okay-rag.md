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
grammar — comments, strings and a handful of keywords — which is
enough to cut a file into whole definitions and name them. It is only
sane to build such a partial grammar because the parser is TOTAL:
what it does not understand becomes ordinary leaves or error nodes,
never a failure, so precision can be sharpened later without a
rewrite. Doc comments are held by the driver and adopted by the
definition that follows, so they land inside the node.

**A language is data, not a parser.** `Language` is a nine-field case
class — how comments are written, how strings are written, which words
introduce a definition, and whether structure is delimited by braces
or by indentation — and the scanner and driver are functions of it.
That is the whole reason a new language costs five lines instead of a
grammar: the total parser turns an imperfect description into ordinary
leaves rather than an exception, so a rough `Language` works on day
one and sharpens later without a rewrite. A parser generator offers no
such gradient — it either accepts the file or does not.

Shipped: **Scala, Java, JavaScript, TypeScript, Rust, Go, C/C++**
(brace layout) and **Python** (indent layout). `Language.of(path)`
dispatches by extension, and `Code.source(src)` parses a `Source` as
the language its own id names — which is what `Symbols.project` and
`Ingest.segment` call, so a mixed-language repository is indexed
correctly with no ceremony at the call site.

The indent driver is the YAML indent stack one level up, which is the
same distinction okay-codec already met between JSON and YAML: the
first token of a line at column *c* closes every definition opened at
a column ≥ *c*. So `def hello` nested in `class Greeter` is nested in
the tree, and the next top-level `def` is not.

**Prose is not code.** A file no language claims gets `Language.text`
— no comments, no strings, no definers — so it becomes a flat run of
leaves and splits by size. That matters more than it sounds: under
Scala's rules a README saying "the type of a given value" would open
two definitions, and indexing a documentation tree as identifiers buys
two thousand mentions of "the" and nothing else. Prose still lands in
BM25, which is what prose is for.

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

// whole definitions, each quoting the file exactly. `Code.source`
// picks the language from the id — ".scala" here, ".py" would get
// the indent driver instead
val segs = Split.structural(src, Code.source(src).tree, budget = 400)(_.length)
segs.foreach(s => assert(s.quotes(src)))

// structural retrieval, no embeddings involved
val idx = Symbols.project(files)      // per-file language, mixed repo is fine
idx.definition("hello").map(Symbols.segment(_, src))   // the code, exactly
idx.mentions("Greeter")                                // where it is used

// the agent edits a file; re-index the damage, not the file
val fresh = Code.reparse(session, oldText, newText, editStart, editEndOld, editEndNew)
```

Naming a language explicitly, and adding one:

```scala
Code.parse(text, snapshotEvery = 64, Language.python)   // this grammar
Code.parseFile("a/b/c.rs", text)                        // by extension
Language.of("script.ts").map(_.name)                    // Some("typescript")

// a new language is data
val kotlin = Language("kotlin", Set("kt", "kts"),
  lineComment = "//", blockComment = Some(("/*", "*/")), docPrefix = Some("/**"),
  quotes = Set('"'), triple = true,
  definers = Set("fun", "class", "object", "val", "var", "interface"),
  layout = Layout.Braces)
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
| `Layout` | `Braces \| Indent` | how the language delimits structure |
| `Language` | `(name, extensions, lineComment, blockComment, docPrefix, quotes, triple, definers, layout)` | everything the grammar needs to know |
| `Language.of` | `String => Option[Language]` | dispatch by file extension |
| `Language.text` | `Language` | the prose fallback: no comments, no definers |
| `Language.all` | `Seq[Language]` | scala, java, javascript, typescript, rust, go, c, python |
| `Code.scanner` | `Language => Scan[Code.K, Code.S]` | the scanner for a language |
| `Code.driver` | `Language => Parse.Step[K, D]` | braces or indentation |
| `Code.scan` / `Code.step` | the Scala defaults | what the bare API uses |
| `Code.parse` | `(text, snapshotEvery, lang)` | parse as a named language |
| `Code.parseFile` / `Code.source` | by path, or by a `Source`'s own id | language dispatch |
| `Code.reparse` | full signature plus `lang` | the damage-priced update |
| `Symbol` | `(name, kind, source, span, path)` | one definition |
| `Index` | `defs`, `refs`, `merge`, `definition`, `mentions`, `names` | the symbol index (a Monoid) |
| `Symbols.of` | `(source, tree, identifiers = true)` | one parsed file; `identifiers = false` for prose |
| `Symbols.source` / `project` / `fold` | one `Source`, many, or streaming | build the index, language per file |
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
- A triple-quoted string ends at the first three quotes, backslash or
  not. That is exactly Scala's rule and only approximates Python's —
  chosen deliberately, because the two ways of being wrong are not
  symmetric: closing one string early costs a few mis-shaped leaves,
  while honouring `\"""` in a language that does not have that escape
  would swallow the rest of the file.
- A definition owns its BLOCK, not its parameter list. This was a real
  bug: `class Greeter(name: String) { … }` ended at the `)`, which
  threw away the body, and for a Go method `func (g G) Hello()` threw
  away the name too. `TestLanguages` locks both.
- `Symbols.of` defaults to `identifiers = true`; call it directly on a
  prose tree and every word becomes a reference. `Symbols.source`
  decides correctly from the path — prefer it.

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

It takes a `Retriever[Pure]`, and that is a real constraint rather
than an oversight: the `Handler[Context]` it builds is COMONADIC, so
nothing inside it may suspend. `Retrieve.handled` discharges a
retriever's own row against a pure handler, which is how the vector
side joins symbols and BM25 there when the embedder is in-process:

```scala
given Handler[Embed] = Vectors.hashingHandler()      // or any pure one
Retrieve.hybrid[Pure](Seq(
  Retrieve.symbols(index, sources),                  // exact
  Retrieve.keyword(postings),                        // BM25
  Retrieve.handled(Retrieve.vector(store))))         // semantic
```

An embedder that must do I/O cannot go there, and should not — that
retrieval belongs in the agent's own row, reached through the search
tool, where it can park. `okay-demo`'s `RepoAgent` runs all three
sides with the deterministic hashing embedder, so it needs no
embedding service to demonstrate the whole shape.

## Passages as lineage

A `Segment` carries the exact span, so re-observing more of a document
is a substring of its `Source` — no second retrieval, no second
embedding. `Corpus.current(seg)` re-reads the passage as the file
stands now (it returns `None` if the file is gone), `Corpus.widen(seg,
by)` grows it by `by` characters on each side without ever moving off
the passage it started from, and `Corpus.whole(seg)` returns the file.
That is what makes "show me more of that" a projection rather than a
new query, and it is the retrieval-side twin of `Large.projecting`.

## Where the numbers are

Indexing throughput, the incremental-reindex ratio, structural
chunking against sliding windows, and per-query retrieval latency are
all measured — see the retrieval section of
[benchmarks](../benchmarks.md). `okay.demo.IndexReport` prints the
same shape for any repository you point it at:

```
sbt "okayDemo/runMain okay.demo.IndexReport /path/to/repo"
```
