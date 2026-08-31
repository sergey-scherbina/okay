# P10 — okay-rag: retrieval, built out of what we already have

## Overview
Retrieval-augmented generation, designed from our own primitives
rather than ported from LangChain's. The question this spec had to
answer first was "have we anything to say here, or would we be
re-typing someone else's class list?" — and the answer is a short
list of things that follow MECHANICALLY from the lex/parse/codec
stack, the Aggregator contract and the laziness contract, none of
which the existing frameworks can express. Everything else in RAG is
assembly, and assembly is cheap here.

The five that are genuinely ours:

1. **Provenance by construction.** Every token carries an exact Span
   and the CST is lossless (concatenated lexemes == the input, by
   law). So a retrieved segment is a BYTE RANGE into the original,
   re-assembly is exact, and a citation cannot drift. Splitters that
   regex over normalized text lose that mapping the moment they
   normalize.
2. **Incremental re-indexing at O(damage).** Documents change. The
   usual answer is re-split and re-embed the file (content-hashing
   per chunk helps only until an insertion shifts every later
   boundary). We have lex/parse RECONVERGENCE: after an edit we know
   which spans changed and which subtrees are reference-identical, so
   only genuinely changed segments are re-embedded and the rest keep
   their vectors with rebased spans. Nobody else in this stack has
   the machinery to know that.
3. **Retrieval and memory are ONE budget and ONE fold.** In
   LangChain/langchain4j a RetrievalAugmentor injects passages and a
   ChatMemory evicts history — two subsystems spending the same
   context window with no knowledge of each other. Here a retrieved
   passage IS a `Turn`, and assembling the prompt is the same
   `Aggregator[Turn, S, Seq[Turn]]` with the same budget: "more
   history or more passages" becomes an explicit, testable policy
   (`zip` two, or score both kinds in one).
4. **Lossy in the view, lossless in the lineage.** A passage is a
   handle into a stream that recomputes (P2). The prompt carries a
   projection; a follow-up re-observes more of that document without
   a second retrieval round trip and without a bigger prompt.
5. **The index is an Aggregator, so it merges.** Building an index is
   a fold with `merge` — which makes distribution (`Cluster.distribute`,
   Spark) and incremental update THE SAME operation: the index of a
   corpus is the merge of the indexes of its parts.

And one the substrate uniquely enables, marked speculative:

6. **Retrieve WHILE generating.** Stages are demand-driven
   coroutines and the parser is total, so a retriever can sit
   downstream of the token stream: as the answer streams, the
   partial structured output names an entity, retrieval for it starts
   immediately, and the result is ready for the next step — the
   FLARE/IRCoT shape falling out of the architecture instead of
   needing an orchestrator.

## Module policy
Small modules, named by function (the standing doctrine):
`okay-llm` = models, transport, tokenization; `okay-agent` = agents,
tools, context, search; `okay-rag` = documents, splitting,
embeddings, retrieval. No umbrella module and no name borrowed from
another product.

## P10a — Documents and splitting (pure, cross-platform)

```scala
final case class Source(id: String, text: String)         // the original bytes
final case class Segment(source: String, span: Span,      // EXACT provenance
                         text: String, path: Seq[String]) // structural path
```

- `Split.structural(dialect)` — split the CST, not the string: a
  Markdown heading tree, JSON members, YAML blocks. Segments inherit
  a structural PATH (`["Chapter 2", "Installation"]`) that is also
  the best free metadata a retriever can have.
- `Split.tokens(budget, overlap)` — a token-window splitter over our
  own `Bpe` Scan: exact counts, no provider call, and the overlap
  window is a `Group` (subtract-on-age), the same primitive chat
  compaction uses.
- Both are Stages, so they compose with `through` and stream.
- Laws: every character of the source lands in at least one segment
  (coverage); concatenating a non-overlapping split reproduces the
  source exactly (the lossless law, inherited); spans are exact under
  every dialect; a DAMAGED document still splits (totality) with the
  damage visible as data.

## P10b — Embeddings and stores (interface first)

```scala
enum Embed[+A]:
  case Of(texts: Seq[String]) extends Embed[Seq[Vector[Float]]]

trait VectorStore[F[+_]]:
  def upsert(items: Seq[(Segment, Vector[Float])]): Unit ! F
  def search(query: Vector[Float], k: Int): Seq[(Segment, Float)] ! F
  def delete(source: String, spans: Seq[Span]): Unit ! F
```

- `Embed` is an effect: a provider handler, a local model handler, a
  deterministic mock for tests — the agent-layer discipline again.
- Batching is `Chunks` + `parMap`; retries are `retryChunks`.
- v1 ships the INTERFACE plus a reference in-memory implementation
  built from parts we already have (`Aggregator.topK` for kNN, CBOR
  for persistence). Honest scope: brute force is honest to ~10^5
  vectors; ANN (HNSW/IVF) and real stores are adapters, later.

## P10c — Retrieval pipelines

- `Query` transforms: multi-query is `Choose` over rewrites +
  `runChoice`; HyDE is a `Model` call; both are programs, not classes.
- Multiple retrievers: `Logic.interleave` gives FAIR interleaving —
  a prolific retriever cannot starve a slow-but-precise one, which no
  round-robin router expresses.
- Fusion: reciprocal-rank fusion is an `Aggregator` (mergeable ⇒
  distributable); dedup is an exact set or HLL.
- Rerank: a `Model` effect over candidate pairs.
- Assembly: passages become `Turn`s and go through the SAME compactor
  budget as the conversation (point 3 above).

## P10d — Ingestion, and keeping the index fresh

- Ingestion is a chunked stream job: load → split → embed (batched,
  `parMap`) → upsert, with `retryChunks` per-chunk recompute,
  resumability, progress as an Aggregator, and `Cluster.distribute`
  for scale — all existing parts.
- **Re-index by damage**: given the previous `Parsed` session and an
  edit, `Scan.relex` + `Parse.reparse` say exactly which segments
  changed; unchanged ones keep their vectors (spans rebased). The
  measurable claim: re-embedding cost is proportional to the edit,
  not the document.

## P10e — Keyword and hybrid

- An inverted index is a `Fold`; term statistics are sketches (CMS);
  BM25 is a scoring function over them; hybrid fusion is P10c's
  Aggregator. Mergeable, therefore distributable, therefore
  incrementally updatable — the same property three times.

## P10f — Code as the proving corpus (the user's proposal)

Index the source of the project the agent is working on, and let
retrieval REPLACE the read/grep tool loop. This is where every
advantage above lands hardest, so code is to RAG what JSON was to the
parser: the dialect that proves the design.

- **Chunks are definitions, not windows.** Splitting the tree gives a
  function, class or method WITH its doc comment, whole — the unit a
  reader actually wants. Window splitters cut mid-body and retrieve
  halves of two functions.
- **Provenance matters more here than anywhere.** An agent quoting
  code must quote it exactly, and `file:line-range` must point at the
  bytes it claims. That is a law here, not a best effort.
- **Incremental re-index is not a nicety, it is the feature.** The
  corpus changes constantly BECAUSE THE AGENT IS EDITING IT. And the
  edit deltas the incremental parser wants are exactly what the
  agent's own edit tool already knows: an Edit handler can hand
  (file, editStart, editEndOld, editEndNew) straight to `Parse.reparse`
  and re-embed only the definitions that actually changed. The loop
  closes: the agent edits → O(damage) re-index → the next turn
  retrieves fresh code.
- **A symbol index needs no embeddings at all.** Definitions and
  references collected by a `Fold` over the parse give exact
  structural retrieval — "the definition of X", "what calls X" —
  cheap, precise, and for code often better than similarity. Semantic
  search then covers the "where is the thing that does…" half, and
  fusion (P10c) combines them. Zero-embedding retrieval is a real v1.

**Instead of a tool call, sharpen it to RETRIEVAL-AUGMENTED RECALL.**
The agent does not ask for code; `recall` already contains it. That
follows from point 3 of this spec — retrieved segments ARE turns, so
the context handler can assemble "conversation + relevant code" under
ONE budget, and the trade-off is a policy, not an accident. A
`Selective` decision keeps it honest (is the request code-shaped? is
there budget?), and the explicit search tool stays available for when
the agent wants to steer — the point is that the common case costs no
round trip and no tokens spent asking.

**The honest cost: a grammar per language.** Two things make it
tractable. Our parser is TOTAL, so a partial grammar degrades into
error nodes instead of failing — a "definition boundary" grammar
(braces or indentation plus a handful of keywords) is already enough
for chunking and symbols, and can be sharpened later without a
rewrite; a parser generator gives no such gradient. And the machinery
is shared: a brace-family scanner covers Java/JS/TS/C-like, an
indent-family one reuses the YAML indent stack for Python-shaped
languages, Scala is where we live, Markdown is already done.

Rejected alternative: tree-sitter via JNI — it would forfeit
cross-platform (the JS agent could not index), add a native
dependency to a dependency-free core, and hand incremental parsing to
a black box we already do better inside.

## Behavior
- [x] every character of a source is covered by the split; a
      non-overlapping split concatenates back to the source exactly
- [x] a segment's span highlights the right bytes in the original,
      for every dialect (Markdown, JSON, YAML) — `Segment.quotes` is
      the law as a method, asserted at four budgets per dialect
- [x] structural splitting respects boundaries: siblings pack
      greedily while they fit, a subtree too big is ENTERED rather
      than chopped, and no cut lands mid-word
- [x] token-window splitting with overlap is exact under the BPE
      counter, and consecutive windows overlap in the source
- [x] a damaged/truncated document still yields segments (totality)
- [x] Embed batches (one operation per batch, the caller's lever);
      retries and fibers per batch are the existing combinators
      wrapped around the program, since ingestion IS a program
      (resume-from-interruption rides the same store keying: a
      segment is identified by source+span, so re-running upserts
      rather than duplicates — asserted)
- [x] after an edit, re-indexing embeds only the changed segments —
      Ingest.reindex reparses by damage, keeps the vectors of
      segments whose TEXT is unchanged, deletes the stale spans;
      asserted that reuse happens and that fewer than all segments
      are embedded. (The first version of the test used a budget big
      enough to hold the whole file, so there was one segment and
      "only what changed" was trivially everything — the useful kind
      of failure, and the test now separates definitions.)
- [x] multi-query retrieval with fusion widens the result on a
      fixture corpus; fusion is order-independent and needs no
      comparable scores (RRF over ranked lists, tested with two
      deliberately different score scales)
- [x] fair interleaving is available as Retrieve.fair over
      Logic.interleave (the Logic property itself is tested in the
      core suite; here it is wiring)
- [x] retrieved passages and chat history share ONE budget: adding
      passages evicts history under the same policy — Grounded.context
      in okay-agent, with `share` naming how much retrieval may take;
      tested that neither side is starved and the total never
      overflows
- [x] a passage kept as lineage re-observes more of its document
      without a new retrieval call — `Corpus.widen(seg, by)` grows a
      passage from its source (clipped at the edges, snapped outward
      to line boundaries, span still exact so it still QUOTES), and
      `Corpus.whole` hands back the document. `Corpus.current` is the
      companion check: a segment whose text no longer matches its
      span means the index has drifted from the file.
- [x] (P10f) a code file splits into whole definitions with their doc
      comments, each quoting its file exactly — the doc comment is
      HELD by the driver and adopted by the definition that follows,
      so it lands inside the node where a reader expects it
- [x] (P10f) an agent edit re-indexes only the changed definitions —
      measured: a one-literal edit in a 40-definition file re-drives
      under half the tokens, the tree equals a fresh parse, and the
      untouched definitions keep their exact text
- [x] (P10f) symbol retrieval finds a definition with no embeddings
      in play (Index is a Monoid: a project is the merge of its
      files); fusing with semantic hits waits for P10b/c
- [x] (P10f) recall assembles conversation and code under ONE budget:
      the agent asks a question, the code is already in `recall`, no
      tool call is made (asserted on the recorded tool log), and the
      passage keeps its provenance header on the way in
- [x] (P10f) the grammar is a FUNCTION OF A LANGUAGE DESCRIPTION, not
      a hardcoded dialect: `Language` names comments, strings,
      definers and layout, and `Code.scanner`/`Code.driver` are
      functions of it — so adding a language is data, not code
- [x] (P10f) both layout families exist, as this spec predicted:
      braces for Scala/Java/JS/TS/Rust/Go/C, and an indent driver for
      Python that IS the YAML indent stack one level up — a definition
      owns everything indented under it, asserted by nesting and not
      only by names being found
- [x] (P10f) language dispatch by extension, and a mixed-language
      project indexes each file by its own grammar (`Symbols.project`
      and `Ingest.segment` both go through `Code.source`)
- [x] (P10f) every language holds the same three laws under random
      input: the tree is lossless, an incremental reparse equals a full
      one, and every definition quotes its file exactly — fifteen
      properties, five languages
- [x] (P10f) prose is not read as code: a file no language claims gets
      `Language.text` (no definers), so a documentation tree
      contributes segments to BM25 and no phantom identifiers to the
      symbol index
- [x] (P10f) a definition owns its BLOCK and not its parameter list —
      found by the Go case, where `func (g G) Hello()` lost its name
      to its receiver, and true of Scala's `class C(x: Int) { … }` all
      along

## Decisions
- **No Runnable/LCEL layer.** LCEL exists because Python has no
  composition; `flatMap`, `Stage`/`through` and `Chunks` already are
  invoke/stream/batch, typed, with no parallel API surface to keep in
  sync. This is a whole subsystem we delete by construction.
- **Interface first for stores** (the user's call): define
  `VectorStore`, ship a reference in-memory implementation, leave
  pgvector/qdrant/Lucene adapters to their own small modules.
- **Embeddings are an effect, not a dependency.** Nothing in the
  module pulls a model runtime.
- **Segments carry structure, not just text** — the path is free
  metadata, and it is what makes citations readable.

## Out of scope
- binary document formats (PDF, docx) — a separate module if ever;
- ANN indexes in v1 (brute force is honest to ~10^5);
- provider and store breadth — adapters, one small module each;
- evaluation harness (its own phase; Aggregators already fit).
