# Backlog

Open work only, grouped by the module that owns it. Everything closed —
landed, refuted, declined or answered — moved VERBATIM to
`BACKLOG-ARCHIVE.md` on 2026-09-10 (backlog-cleanup): 314 closed entries
against 51 live ones had made the live list unreadable, which is the
flat-root-file failure the `scrumban` skill names. Nothing was deleted.

Refutations are the expensive part of this repository's memory, so the
verdicts stay HERE as one-liners (below) and the reasoning stays in the
archive. Read them before re-taking anything.

New work goes under its module's heading; a cross-module or unscoped item
goes under "not yet scoped". When a heading grows past a screen, the
skill's next step is that module's own `<module>/BACKLOG.md`.

## bench-native-lanes — a competitor's row should measure THEIR api, not ours

- [x] **The five in-process lanes: DONE** (2026-09-10). The plain JVM,
      `java.util.stream`, fs2, zio-streams and kyo all fold
      `Native.Fold` — panes as keys in a map, nothing evicted, a
      mutable cell, a sort for the top-5 — with no okay type in the
      lane, each carried by the library's own combinators and each at
      1/2/4/8 cores. §20 has the numbers and the reversal they show.
- [x] **Flink and Spark: DONE** (2026-09-10,
      bench-engine-native-arithmetic). Flink accumulates through its
      own `AggregateFunction` over POJO accumulators (`StatsAcc`,
      `TopAcc`), Spark's RDD lane through its own `aggregateByKey`
      over a flat `(Long, Long, Int)` and a sort per window for the
      top-5. Both still answer the same eleven checksums. The finding
      the rewrite produced is worth more than the fairness: our
      accumulator was a HANDICAP — `((Long, Long), Option[Int])` is
      unreadable to Flink's type extractor, so its window state went
      through Kryo, and Spark wrote it across every shuffle. `toFlink`
      and `SparkInterop` stay in their own suites, where "one value
      answers on every engine" is a claim about the interop rather
      than a benchmark row.

## okay core
- [x] aggregator-zip-allocates — DONE (2026-09-10):
      `Aggregator.summary` is the flat count/sum/min/max accumulator,
      beside `Mean` and `Variance`, and it takes the Wrocław job's
      windowed lane from 810 ms to 580 (1.40x) — level with a
      hand-written MUTABLE cell, while keeping the value semantics
      `merge` needs. 83 B per `add` becomes 37.
- [x] topk-stops-sorting-the-corpus — DONE (2026-09-10):
      `Aggregator.topK` consed and re-sorted for EVERY element, and
      `MemoryStore.search` folds a whole corpus through it. Guarded on
      the k-th kept element: 5 358 168 B to select 8 of 10 000 records
      became 30 328 (docs/benchmarks.md §9h), and the store's search
      lane 351 713 B/op and 2542 us became 49 943 and 1036.
- [ ] topk-insert-instead-of-sort — what §9h left on the table. An
      element that DOES make the cut still sorts k+1 through
      `List.sorted` (array copy out, sort, list back), and about
      `k · ln(n/k)` elements make the cut: 57 of 10 000 at k = 8,
      which is the whole 30 KB residual. Inserting into the
      already-sorted list would allocate the prefix and nothing else.
      Small, and only worth doing with the probe in front of you —
      `compare/runMain okay.TopKProbe 10000 8` prints the exact bytes.
- [x] aggregator-zip-flat-general — DONE (2026-09-10):
      `OfLong.zipLong` keeps the specialization through the pair — a
      flat `Longs2` accumulator, both sides stepped by `addLong` — and
      `AggregatorZipBenchmark` prices it in BYTES, which a loaded box
      cannot blur: 90.5 B/element for `count zip sumLong` against 50.8
      for `count zipLong sumLong`, error ±0.4 B/op. A new name rather
      than an overload of `zip`, so no inferred accumulator type
      changes under a caller who did not ask.
- [x] parse-depth-test-asserts-wall-clock — DONE the same day
      (parse-depth-tests-out-of-the-gate, 63db4156): the suite is
      `Live` and runs in `sbt integrationTest`. The minimum-of-three
      repair was tried and MEASURED to fail — 19.6x and 11.5x with
      minima on both sides, and the 50 000-level test hit munit's 30 s
      timeout at three runs, because the two sides of the ratio differ
      20x in duration and a contended scheduler perturbs the long one
      far more often. The gate that proved the fix ran green at load
      average 65, the condition that had produced five reds.
- [ ] aggregator-sum-hides-its-specialization — `sum[N]`'s declared
      return type is `Aggregator[N, N, N]`, so the `OfLong` underneath
      is invisible and `zipLong` cannot be reached from the idiomatic
      spelling (it needs `Aggregator.sumLong`). A match type on the
      return, or an `OfLong`-returning overload for the `Long` case,
      would close it. The same shape as `zip` hiding the
      specialization, one level down.
- [x] windows-one-lookup-per-pane — REFUTED BY MEASUREMENT
      (2026-09-10, windows-int-key-panes). The idea: `Windows` holds a
      one-field `Cell` per pane, so folding an element is ONE hash walk
      plus a field store where it was `getOrElse` then `update`, twice
      per pane per element. Written, gated, and then priced on a quiet
      box by `WindowsBenchmark` (which this lane also lands), 16
      iterations per side, minutes apart:
        tumbling 426.8 +-23.0 us with it, 431.4 +-18.6 without
        sliding  911.5 +-124.2 us with it, 874.5 +-69.8 without
      1% better on one lane, 4% WORSE on the other, both inside the
      bars: the change buys nothing. `LongMap`'s get and update are
      cheap next to the rest of `add`, and the Cell's indirection and
      its allocation per pane pay back whatever the second walk cost.
      The code is reverted; the instrument stays.
- [ ] windows-int-key-panes — the OTHER third of the same gap
      (docs/benchmarks.md §20, "Why one core loses"): `okay.Windows`
      keys panes by `HashMap[K, LongMap[Acc]]`, which boxes an `Int`
      key and hashes twice where the packed benchmark form does one
      `LongMap` lookup — 8.5-11% measured. A `PaneKey[K]` seam with a
      packed store for `Int` keys and today's store for everything
      else would take it, IF the window index and the key both fit in
      a `Long` (they do for any `Int` key and a slide over ~2 ms, and
      the fallback must be chosen at construction rather than
      mid-run). Measure before believing: the same 2x2 is in
      `OkayLane` and prices it on every run.
- [x] wroclaw-remeasure-quiet — DONE (2026-09-10): §20's whole table
      is one run of `scripts/wroclaw-bench.sh 8 3 1` on a box under
      load 5, so the daggers are gone and the rows can be read against
      each other. It also PRICED the arithmetic rewrite: Flink +12%
      and a fifth less allocation, Spark's RDD lane +30%.
- [ ] handler-fusion-flat — GATED OFF by stage 0 (the ceiling for pass
      fusion measured 1.13–1.29x); reopen only with a new number. Was:
      `Handler.flat`: Handler.union assembled
      inline so the nested <|> chain unrolls to one match; measured
      on the four-effect agent row, fourth position is the number.
      (was filed under "handler-fusion" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] handler-fusion-step — GATED OFF by stage 0, same reason. Was:
      `Step[F, Acc]` (tail-resumptive by type)
      and `Fused.run` over `F + G` with the row-shaped product state;
      instances for State, Writer (Fold-generic), Reader incl. local;
      abort/choose fall back to a shift with the state captured
      immutably; laws: agrees with nested for both orders, stack-safe
      at 1M, multi-shot and abort survive.
      (was filed under "handler-fusion" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] growing-channel-order-under-load — SEEN ONCE, NOT REPRODUCED,
      and recorded because the alternative is forgetting it. A full
      matrix on 2026-09-10 failed `okay.TestGrowing`'s "each
      producer's own order survives the swap"
      (src/test/scala-jvm/TestGrowing.scala:186) at round 34 of 200:
      producer 1's subsequence came back 49, **57**, 51, 55, 59 — its
      own FIFO order broken across a part swap, on the SHIPPED
      `Channel(4)` the test deliberately uses. The box was at load 45
      (an operator VM holding ~11 of 14 cores) and two full matrices
      on the same tree an hour earlier were green, as were 13
      subsequent runs of the suite alone — 2 600 rounds at load 15–29,
      no failure. So it is one of: a real race in the adoption/swap
      path that needs contention to show, or a promise the growing
      channel does not actually make under it. NOT tagged and NOT
      retried: both would hide a real defect, and the suite is the
      only place this guarantee is stated. Reproduce with load, not
      with repetition — that is what distinguished the two runs. The
      gate log is okay-gate.ySVIJbPIg1 (the diff is in it).

- [ ] ctx-reader-bridge — `(A ?=> B) <-> B ! Reader % A`, one
      Conversion each way; GATED: no consumer named
      (specs/context-functions.md)

- [ ] logic-named-cut — GATED on a search consumer
      (specs/backtracking.md)

## okay-lex
- [ ] scan-into-the-other-scanners — `Yaml`, `Markdown`, `Xml` and
      okay-rag's `Code` scanner still answer the pair, so they still
      pay the `Tuple2` per character and a `Vector` per token; the
      default `stepInto` keeps them correct, not fast. Json's move
      says the shape of the win (~29% of the allocation on that
      scanner's road), but none of the four has a measured lane, and
      `Code`/`Yaml` recurse into their own `step` — the conversion is
      real work, not a rename. Wants a lane that measures one of them
      first: okay-rag's code chunker over a real file is the honest
      workload.

## okay-codec
- [ ] native-runner-error, RECURRENCE LEDGER (the entry itself is
      closed in BACKLOG-ARCHIVE.md — the cause is settled: the test
      binary's connection ends and it exits 0 while sbt still has a
      call in flight, so the module reports no tests and sbt reports a
      lost process). Recorded here only so the rate stays visible, as
      `scripts/gate.sh` asks on every occurrence:
      2026-09-10, okayCodecNative, one lost process in a full gate,
      GREEN on the rerun of that module alone (bench-native-lanes).

- [ ] json-strict-is-now-the-slow-door — `Json.readStrict` reads 1104
      ns against `Json.read`'s 1004. The strict door was built to
      avoid the lossless road's cost, and 131cedc2 + b4172242 removed
      that cost. Either make the strict walk cheaper than the CST road
      it was meant to replace, or leave it and keep it for its
      REFUSAL — docs/benchmarks.md §10 already says the latter.
      DISQUALIFYING: if the strict walk's extra 100 ns is the field
      map and `make` (the breakdown says it is ~3.3x the bare parse),
      there is no cheap win and this closes as wontfix.
      (was filed under "bench-known-prices" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] elements-door-cursor — the `.elements` door reads 23.8 against
      the chunk transformers' 10.78 (§5), 2.2x for the per-element
      cursor. Known mechanism, stated in the doc.
      (was filed under "bench-known-prices" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] codec-two-roads-audit — the json-parse-fast-road shape as a
      QUESTION rather than a fix: a module had two roads to the same
      value, 37x apart, and the DEFAULT was the slow one for long
      enough that a separate feature (py-arrow) got filed to work
      around the symptom. Where else does this repository have a fast
      path that nothing takes by default? Named suspects: the CBOR
      pair beside `Json`/`JsonValue`, and the staging seam's
      interpreter-vs-installed choice (`Codecs.current`), which is a
      runtime switch rather than a road but has the same failure mode
      — measured once, then assumed. Cheap to check, and the last
      check of this kind was worth 37x.

## okay-agent / okay-intent — intent, autonomy, dialogue
- [ ] spans-static-tokens — `Static` already holds a vector per token
      unit, which would give `okay.intent.Spans` a JS road with no
      runtime and no model file. The measurement says context is worth
      0.2–0.3 of cosine (specs/intent-spans.md), so this is a
      compromise to MEASURE against the contextual encoder on the same
      317 turns, not a default. Trigger: a consumer that needs the slot
      layer where `okay-onnx` cannot follow.

- [ ] tod-schema-guided-retrieval — Labruna, Bonetta, Magnini (RANLP
      2025, "Task-Oriented Dialogue Systems through Function
      Calling", MultiWOZ 2.3): let the model call a schema-guided
      query that fetches only the needed KB entries, instead of
      putting the whole KB in the prompt; accuracy up, tokens and
      time down, the gap widening as the KB grows. Their BASELINE is
      not ours — we never put a KB in a prompt, and the demo's
      central claim is that nothing reaches the projection except
      through a tool. What IS new for us: deriving the RETRIEVAL tool
      from the store's own `Schema` instead of hand-writing one tool
      per query shape (okay-sql's typed layer, okay-match's
      registry), so a new domain field becomes a queryable slot with
      no new tool code. Pairs with a KB-size sweep — tokens per turn
      and latency, full-KB against schema-guided — in
      docs/benchmarks.md, because at the demo's current KB size the
      effect is invisible by construction.
      (was filed under "Task-oriented dialogue: the literature the operator " — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] tod-single-sequence — SimpleTOD (Hosseini-Asl, McCann, Wu,
      Yavuz, Socher, 2020, arxiv 2005.00796): belief state, actions
      and response as ONE delimited sequence rather than three
      models. Filed LAST, and the reason is the useful part. Its
      result is a fine-tuning result (GPT-2 on MultiWOZ) that we
      cannot reproduce without training; the 2025 paper above argues
      the opposite architecture on ground that suits us better; and a
      single delimited sequence WEAKENS the invariant the demo is
      built on, since a belief cut out of raw text has not gone
      through a tool (recoverable only by decoding it through
      `Schema` before it touches the store — intent-classify's own
      rule). What stays attractive is the SHAPE: SimpleTOD's
      inference suspends after the belief state, queries the KB,
      appends the result and resumes the SAME generation — a
      coroutine that yields exactly once, which is `Stage` over
      `Cont` and something okay expresses better than a framework
      would. Worth building only if the items above leave a reason to.
      (was filed under "Task-oriented dialogue: the literature the operator " — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] tod-multiwoz-harness — OPTIONAL and honestly expensive: a
      loader for MultiWOZ 2.3/2.4 plus the inform/success/joint-goal
      metrics, so our dialogue lane has numbers comparable with the
      outside world instead of only with itself. Keep separate from
      the items above; it is a benchmark harness, not a feature.
      (was filed under "Task-oriented dialogue: the literature the operator " — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-fasttext-subword — subword embeddings TRAINED on the
      corpus plus a linear head, i.e. fastText's actual algorithm in
      plain Scala. Bridges chargrams (language-agnostic, no network,
      60%) and the probe (86.7%, needs a server): a trained
      representation that still ships as an array. Only worth it if
      `intent-embedding-choice` says the server is the problem.
      GATED 2026-09-07, the gate measured: "only worth it if the
      server is the problem" — intent-4b-with-more-data found both
      embedders flat from 32 examples with the same slope, and every
      no-network tier since (chargrams 65, TF-IDF 61.7, the static
      table at 68.3 with triples and PCA) meets the same ceiling from
      a different road: the limit is register and context, not the
      server. A trained subword head would be a fourth road to it.
      Opens if a fixture at least twice this size shows a late slope.

- [ ] intent-grammar-parse — intent by GRAMMAR over `okay-lex` and
      `okay-parse`, the way `Temporal` does dates: deterministic,
      explainable, and refusing rather than guessing. Expensive in
      rules, and the honest reason to want it is a domain where a wrong
      answer is worse than no answer.
      GATED 2026-09-07: no consumer has named a domain where a wrong
      answer is worse than no answer; the model tier at 0.909 macro
      F1 with zero undecodable replies, the slot parsers (when,
      duration, people, amount) already refusing rather than guessing
      where a wrong value would act. Opens with that consumer.

- [ ] intent-crf-slots — sequence labelling for the frame's SLOTS
      (who, when, where) rather than its class. `Temporal` fills one
      slot with a parser; the general case is a tagger, and a CRF is
      the classical one. Only after the class problem is settled.
      GATED 2026-09-07: "only after the class problem is settled" —
      it is settled for the model tier (0.909, deterministic, the
      decoder reading every reply) and the four parsed slots cover
      what the meeting frame asks; the slots still open (who, places)
      are named entities, which is what a tagger is for, and which no
      frame in the fixture yet asks a question about. Opens with a
      frame that does.

- [ ] intent-ensemble-weights — `NoModel` blends the probe with the
      pattern tier using ONE fitted weight from a six-point grid,
      because sixty rows cannot support a fitted second-level model.
      When the corpus grows (see distillation), replace the grid with a
      real stacking model and measure whether it beats the blend.
      GATED, and the gate is now measured (2026-09-07): the corpus did
      not grow honestly — intent-distil-dose found the distilled rows'
      gain to be one split's, intent-distil-static found them worth
      nothing to the static table, intent-distil-diversity found them
      a third as diverse as the fixture — so a second-level model
      trained on them would learn the generator's register. Opens when
      the human fixture passes ~200 rows (the review queue is the
      source); the six-point grid stays until then.

- [ ] intent-language-fixture-growth — SHARPER NOW (2026-09-05): the
      fixture is eight languages wide (uk and pl added) and every
      non-English language has at least one class at F1 0.00 when
      fitted on fifteen rows of it. The construction is
      language-agnostic; the DATA is what is missing. Original entry:
      the per-language arms train on
      FIFTEEN examples each, where the learning curve put the probe's
      stabilisation at about thirty-two, and the numbers swing from
      46.7% to 86.7% accordingly. No per-language claim about
      embedders or classifiers is defensible until the parallel set has
      at least 30 messages per language, which means growing it from 30
      meanings to 120. That is a translation job, and the
      author-written-translation limitation grows with it.
      (was filed under "the original entry" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-annotate-log — the model reads the LOGS and proposes
      labels for REAL messages (operator's direction; specs/intent-classify.md,
      "The harvest programme"). Not the distillation that failed: the
      model writes nothing, the messages are real traffic, only the
      label is proposed — the practice the literature supports
      (arxiv 2406.17633, 2503.17336). A row is kept only if the
      reading grounds in the message, conf >= Medium, k samples agree,
      and no deterministic tier contradicts at high margin; provenance
      per row (model, prompt fingerprint, date, filters passed).
      Criterion: 100+ kept rows, and a refit on them moves the
      autonomy rate without breaking the per-class law.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-coannotate-queue — CoAnnotating (arxiv 2310.15638):
      route by UNCERTAINTY, so a person arbitrates only what the
      filters could not settle, ranked by tier disagreement. Our
      active-learning lane already measured the shape (28 labels
      against 36 random for the same gain). Criterion: human effort
      per point of autonomy, not accuracy alone.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-discover-classes — Other is several real classes nobody
      named; cluster what lands there and name the clusters with the
      model (Dial-In LLM, ACL 2025.emnlp-main.300, >95% agreement with
      human judgement on 100k real calls; NILC, arxiv 2511.05913,
      WSDM 2026), then a person accepts or rejects each proposal.
      Criterion: Other's recall after the split, and how many
      proposals survive review.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-noise-aware-refit — if harvested labels prove noisy
      enough to bind, the noise-aware refinement the literature
      reports (arxiv 2505.19675, ~7% recovered). GATED on a
      measurement showing noise is the limit.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-label-queue — which rows to ask a person about: small
      margin plus disagreement between tiers (the shape the
      active-learning lane measured at 28 labels against 36 for the
      same gain). Ships as a queue the admin flow can drain.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-induce-on-harvest — re-run cue induction whenever the
      corpus grows and ship the induced cues beside the hand-written
      ones. Cues need nothing at run time, so their coverage is pure
      autonomy: 85.7% precision at 11.7% coverage on 60 rows today,
      against hand-written 90.6% at 53.3%.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-per-language-models — one artifact per language once
      rows exist; the shipped one is English-only and scores chance
      (23-30%) elsewhere. GATED on intent-language-fixture-growth.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-other-more-rows — SHARPENED 2026-09-07 by
      intent-offline-other, which turned this from "more data would
      presumably help" into a measured blocker: an out-of-domain
      detector over the same rows already RANKS at AUC 0.843, and
      every decision rule built on it is starved by 15 training rows
      (by argmax it cannot fire at all; balanced it destroys the
      tier). 40-60 real out-of-domain English rows — the operator's,
      or harvested from the service's own traffic into
      okay-chat/corpus/harvested.json — and TestOfflineGate re-runs
      unchanged to settle it. Still needs human rows: the distillation
      lanes measured generated ones to be worth nothing (they carry
      the generator's register).
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-second-author — PARTLY ANSWERED 2026-09-04 by measuring
      the gap instead of the corpus: 66.7% on the least-familiar half
      against 86.7% on the most, 65-67% under mechanical register
      shifts, and every shipped quote corrected to 65-70% for a
      message somebody else wrote. What remains is the part no
      measurement replaces — a corpus this repository did not write.
      Original entry follows.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-second-author (original) — the provenance problem the review could
      not fix: the rows are still one hand's Russian, rewritten by the
      same hand that wrote them. A gap measured against my own language
      is a joint measurement of the model and me. The consumer offered
      REVIEW, which is what was available and is now spent; what is
      missing is a second AUTHOR, for Russian and for whatever
      languages the fixture keeps.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-extract-more-slots — people DONE 2026-09-07
      (intent-extract-people): `People.parse`/`find` — a count beside a
      people-word (`four people`, `six of us`, `a team of 5`, `vier
      Personen`, `dla czterech osób`, `4人用`), the Slavic collective
      numerals counting by themselves (`на четверых`, `на чотирьох`),
      1..1000, `None` otherwise; `Slots.people`; the fixture's
      book-room row counts four in all eight languages; number words
      shared with `Duration` through `Numbers`. Left here: named
      entities (who) and places — neither is a parser. Durations DONE
      2026-09-07
      (intent-extract-duration): `Duration.parse`/`find` (minutes; a
      number and a unit, `1h30`, `90m`, the spoken fractions, number
      words, `and a half`; total and deterministic like `Temporal`)
      and `Slots.duration`, asked in `when`'s six languages, showing
      `1h30`/`2h`/`45min` back. English phrases; the other languages'
      number-and-unit words are filed as
      intent-duration-multilingual. Amounts DONE 2026-09-07
      (intent-extract-amount): `Amount.parse`/`find` — a number
      beside a symbol, a code or an unambiguous currency name in
      the eight languages, separators told apart by the digits that
      follow, composed number words, the nearest number wins;
      `Amount(value, currency)` with an ISO code; `Slots.amount`.
      Still open here: named entities (who) and places — neither a
      parser. Original: only
      `when` and whole-message text have extractors. Named entities
      (who), durations, places and amounts are the obvious next ones,
      and each is a `Slot.extract` rather than a design.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## okay-rag
- [ ] vector-search-dominates — `searchVectors` 379 us dominates §11's
      per-query table, where everything else is under 20. Not a
      defect (240 segments x 1536 dims is real work), filed because it
      is where retrieval's time actually goes.
      (was filed under "bench-known-prices" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] rag-langchain4j — their EmbeddingStore as a Retrieve handler
      (the other half of the interop sentence; when a consumer
      names a store)
      PARTIAL 2026-09-02: the EmbeddingModel half landed as
      okay-langchain4j-embed (a local ONNX embedder, String =>
      Embedding + Handler[Embed] — MemoryMatch's exact `embed` seam,
      no okay-rag pipeline needed). Deliberately OUT of okay-demo's
      build and the root aggregate (a real ~90MB model download).
      The EmbeddingStore/VectorStore half named in the title is
      still open — this box stays unchecked for that.

## okay-r
- [ ] r-rserve — stage 1: the served engine (Java client behind a
      trait; own QAP1 over Async later if named); two-engine
      acceptance

- [ ] r-arrow — frames as Arrow files/streams once the JSON-frame
      road hurts. MEASURED 2026-09-09 (r-measure-harden,
      `MeasureRFrame`, medians of five against the dockerized R 4.4.1,
      `identity` on a 3-column frame):

      | rows | payload | our encode | round trip | our decode | typed rows | OUR share |
      |---|---|---|---|---|---|---|
      | 10 000 | 0.30 MB | 6.5 ms | 1 546 ms | 7.3 ms | 2.2 ms | 0.9% |
      | 100 000 | 3.21 MB | 20.4 ms | 13 686 ms | 18.3 ms | 6.0 ms | 0.3% |

      The number says the opposite of the Python twin's. There, 60% of
      the trip was OUR parser; here our two halves are 0.3% and the
      other 99.7% is R. Nor is it the pipe: 3.21 MB in 13.7 s is
      ~230 KB/s, and a pipe does that in milliseconds — we encode and
      decode the same bytes at ~83 MB/s. So the cost is jsonlite
      walking the STRUCTURE we hand it, and the structure is the
      suspect below. Arrow would still remove it, at the price of the
      `arrow` package (native, heavy) on R's side and an Arrow reader
      on ours — a big dependency for a module whose only dependency
      today is jsonlite. Try the cheap shape change first.

- [ ] r-restarts — GATED twice: on r-subprocess and on a restart
      consumer; the one resumable-capture case (specs/r.md)

## optics-outside — optics as an API, not an implementation (specs/optics-outside.md)

The arc the operator opened 2026-09-10, after `specs/optics.md` closed.
Optics INSIDE okay are done; this is optics, profunctors, arrows and
categories as the vocabulary a USER of the library writes. The spec's
criterion decides what belongs here and what is decoration: a
declaration earns an optic only when it must be given to more than one
interpreter and at least one of them DESCRIBES it instead of running
it — which a plain `S => A` cannot do. Read the spec's Overview before
taking any of these; it also records where the run-time tax lands (on
the USER's data path now, so a per-element optic ships behind `Fuse`
or not at all).

- [x] optics-outside-routes — DONE (2026-09-10, dcc1d76e). A route is a prism. One declaration
      answers three questions: does this path match (server), what is
      the path for these parameters (client, reverse routing), and
      what does it look like (OpenAPI, an MCP tool). The prism law
      `unapply(url(a)) == Some(a)` is the feature, not a nicety.
      Motivated by what is in the tree now: every `routes` in the
      repository is `case r if r.method == Get && r.url == "/healthz"`
      (okay-ops, okay-admin, okay-acme, okay-demo) — no route has a
      typed parameter at all, and the one router that does have
      parameters (okay-script's `Site.resolve`) hands them back as an
      untyped `Map[String, String]` and splits on `/` before decoding,
      so an encoded `%2F` inside a parameter becomes a segment
      boundary. Stage 1 of the spec. Landed WITH a caller:
      `Acceptance.routes` became a `Router`, and the conversion found
      two live defects — `startsWith("/person")` answered `/personal`,
      and every route answered every verb.
- [ ] optics-outside-routes-adopt — the other hand-written routers.
      okay-ops, okay-admin, okay-acme and okay-demo all still match
      with `case r if r.method == Get && r.url == "/healthz"`, and
      okay-script's `Site.resolve` still decodes before it splits.
      Each conversion is small; each is a BEHAVIOUR change at a live
      endpoint (a query string now matches, a wrong verb now misses),
      so it wants its own lane and its own gate, not a sweep.
- [ ] optics-outside-routes-query — stage 2 of the spec: query
      parameters, headers and bodies. Same structure, a different
      monoid, and it is what stage 3 (rendering OpenAPI and MCP tool
      declarations out of `describe`) needs before it can start.
- [x] optics-outside-tools — DONE (2026-09-10, 21bc8a5c), and it was not
      on this list. Stage 2 of the spec: a tool was declared three
      times (hand-written JSON Schema, a dispatch map re-reading the
      same field names as string literals, and the name written
      twice). `Toolbox` gives `specs` and `table` from one vector.
      The finding: the hand-written schemas never declared `required`,
      so no model was ever told `board_add` needs both its fields.
- [ ] optics-outside-tools-effectful — `Toolbox` handlers are
      `A => String`, because that is the seam `Mcp.Server`,
      `Handlers.tools` and `Stepper` already take. A tool that must do
      I/O has to close over its own runner today. Widening to
      `A => String ! Rest` is a separate decision with those three
      callers to carry; see the spec's Out of scope.
- [ ] optics-outside-tools-adopt — the remaining hand-written pairs.
      okay-mcp's and okay-http's test tables build a `ToolSpec` and a
      `Map` side by side the same way; they are tests, so the cost of
      the drift is lower, but they are also the examples people copy.
- [ ] optics-outside-policy — a projection policy is a traversal:
      which fields of a record may be seen, embedded, logged. The
      interpreter that earns it is the AUDIT — "name the fields this
      policy touches", with no document in hand — and the law that
      couples the two interpreters is that the audit names exactly
      the fields the redaction changes. This repository has already
      paid for not having it: price and contact in a summary's text
      sank a priced offer from 0.63 to 0.13
      (`weighed-not-read-out-of-the-embedding`). Cheapest of the six.
- [ ] optics-outside-live — subscribe to a lens. The lens compiles to
      a wire path, the server pushes only the focused part of the
      document, and a client write comes back as `set`. The most
      valuable of the six to a user and the most work: the optic must
      be reifiable and must survive serialisation. okay-live,
      okay-persist, okay-crdt.
- [ ] optics-outside-query — a query is an optic. `Forget[Sql]`
      compiles it to SQL, `Function1` runs the SAME predicate over a
      `Vector` in a test, `set` compiles to UPDATE. The seat is empty
      — okay-sql is strings plus `Schema` for rows — but typed query
      DSLs are a swamp, which is why the spec ranks this fourth and
      not first. Do not start it before routes and policy have
      measured the shape.
- [ ] optics-outside-topology — a dataflow is an arrow. The ONLY one
      of the six that meets the staticness condition the spec sets for
      reaching for `Arrow` at all: the graph must exist as a value
      before it runs, to be drawn, fused, or shipped to a cluster.
      Wants `ArrowChoice` or branches fall out of the static picture.
      okay-flink / okay-kafka / okay-reactive, and dataflow's own plan
      value is the incumbent to compare against.
- [ ] optics-outside-conf — a setting is a lens that knows its path:
      the error names `server.tls.port` and the reference table
      generates itself from the same values that read the config.
      Smallest and least urgent; take it as a warm-up if the others
      are claimed.

## okay-http
- [ ] flaky-port-roulette — the full-matrix port/readiness family,
      one ledger: TestMcpHttp 503 (2026-09-01), TestResumable first
      subscribe, TestHttp first GET 404, and TestWire reading
      literal "HTTP" bytes at its handshake (a foreign server
      answered on the expected port) — all green alone, all under
      parallel suites in one sbt JVM; suspect ephemeral-port reuse
      between a closing listener and a dialing client

- [ ] http-flaky-mcphttp — TestMcpHttp "one Serving, three wires"
      answered 503 once in a full-matrix run (2026-09-01); green
      alone and on suite rerun — likely a port/readiness race
      (second sighting, same family: okay-jetty TestResumable
      failed its first subscribe once in a full-matrix run
      2026-09-01, green twice alone — port/readiness race shape)

- [ ] http-streaming-responses — incremental bodies on the NIO and
      Netty backends (Jetty has it); unblocks MCP push there

## okay-ui
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-script
- [ ] script-tls: ALPN/HTTP2, OCSP stapling, cipher policy — still the
      proxy's, and named as such in the spec. A Site behind Caddy/nginx/an ingress needs
      three things from the operator: pass Upgrade for EVERY path
      (a live page's socket is on the page's own path),
      `OKAY_FORWARDED=1`, and to treat `X-Forwarded-For` as a claim.

## okay-py
- [ ] py-arrow — frames via pyarrow (twin of r-arrow). RE-FILED
      2026-09-07 with an honest number: the measurement meant to
      justify it found that 60% of a 500k-row frame's 9.7 s round trip
      was OUR OWN `Json.parse` taking the lossless road
      (json-parse-fast-road). The same frame is now 0.94 s, of which
      the Python side is roughly half and our encode 0.3 s. Arrow
      would still take the serialization hop out, but "the JSON-frame
      road hurts" is ten times less true than when this was filed and
      no consumer has asked. Measure again before building.

## okay-mail
- [ ] mail-consumer-adoption — the consumer who asked for okay-mail
      replaces `Identity.console` with it. Not my lane to do, but the
      one that tells whether the seam is right: their `deliver` is
      `(Channel, String, String) => Unit` and `Mail.Send` has to plug
      in without anything else changing, which was their stated
      requirement.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## okay-parse
- [ ] parse-depth-timer-warns — `TestParseDepth.timeMs(body: => Unit)`
      makes its four callers discard a `Parsed`, which is four E175s
      on every platform and therefore a warning on master that every
      landing inherits. It is NOT a mechanical fix: making the helper
      generic was tried (47dbc639) and reverted, because the test
      then read 27.4x where it asserts under 8 and the honest reading
      is that a TIMING test is the one place AGENTS.md forbids
      rewriting to please a linter. Whoever owns this test should
      change it and re-establish its measurement in the same lane —
      the `: Unit` ascription at the call sites is the candidate that
      generates the same code the implicit discard did.

## okay-cluster / dataflow
- [x] dataflow-reconnect — LANDED, both halves, and the measurement
      the entry asked for says both are needed. TOLERANCE: a worker is
      buried after three CONSECUTIVE failures and any answer clears
      its count, which makes a blip on EVERY worker survivable — with
      a tolerance of one, the new test dies with the same sentence
      stage 5's first seeded test produced, "no workers left (4 were
      given)". That alone is enough for a worker that hiccups and
      cannot be enough for a SOCKET, whose failure is permanent by
      construction, so `Served.reconnecting` dials lazily and drops
      the socket on any failure. Against a server that hangs up after
      every request, `connect` dies and `reconnecting` finishes the
      job — the two roads differing only in which `Serve` the
      coordinator was handed. `Run.failed` (attempts lost) is reported
      beside `Run.retried` (workers buried), because a run can now
      recover from a failure without burying anybody, and two tests
      that asserted the burial were asking the older question.
- [x] dataflow-coordinator — LANDED as stage 8. `Wire.state` makes
      the fold a value, `Checkpoint` is where it goes, and a second
      `Cluster.stream` over the same journal picks the run up. It was
      an assembly: the seam binds to okay-persist's compacted log in
      eight lines, and the reason it is a `save` call rather than a
      barrier protocol is that the epoch loop is lock-step.
- [x] dataflow-coordinator-election — LANDED as stage 10, and it was
      a lane rather than a line for the right reason: the wiring is
      small, and the FENCE it forced is the part that mattered. A
      deposed coordinator now stops at its next commit instead of
      writing over its successor.
- [ ] dataflow-fenced-commit — stage 10's fence is a CHECK before a
      write, so a leader deposed between the two can land one commit.
      Closing it needs a store that offers "save only if the term is
      still mine"; the seam already permits one (`save` may throw).
      One commit wide, named in specs/dataflow.md.
- [x] dataflow-commit-window — LANDED as stage 9, and the entry above
      was wrong about the roads: both the ones it named are worse than
      the window, and the one it did not name is what every engine
      does. The engine cannot close it (the write left the engine), so
      it hands the writer `committed(epoch)` and `recovered(epoch)` —
      told BEFORE the journal, so the worst case is a repeated epoch
      rather than a lost one, and a writer that records the epoch
      beside its rows is exactly-once.
- [ ] dataflow-durable-stage — stage 9's staging sink keeps its epoch
      in memory, so the two-phase commit survives the COORDINATOR's
      death and not the WRITER's. A durable stage (a transaction, a
      temp file per epoch) closes that; the seam already fits.
- [x] dataflow-exchange — LANDED. The crossover is ~100 000
      accumulators and the Wrocław job is three orders of magnitude
      under it, so `Auto` declines the exchange there.
- [x] dataflow-auto-for-a-real-accumulator — CLOSED by measurement,
      and the prediction it rested on is REFUTED. §20's tuple tree
      (`count zip sum zip max`, six objects per add) crosses in the
      SAME 80 000-to-160 000 band as a Long count, in two runs — the
      accumulator's weight changes the slope past the crossing, not
      the crossing. One constant serves both and 100 000 is it.
      A methodological finding came with it: the first version of the
      table used a millisecond clock over lanes of 1-7 ms and reported
      the two crossovers four-fold apart. That was quantisation, not
      physics; the ratio is computed from nanoseconds now.
      The other half of the old comment — that fewer partitions move
      the bound up — is still unmeasured and is now labelled as such
      rather than asserted.
- [x] dataflow-onepass — LANDED. And it produced the number the
      engine had been unable to quote: 5.5x the hand-written lane.
- [x] dataflow-complete-panes — LANDED. 5.50x -> 1.14x of the
      hand-written lane; 122 679 accumulators reach the coordinator
      where ~2.9 million did.
- [x] dataflow-run-complete-panes — LANDED. The windowed `Wide` node
      has the rule: a partition that can finish a pane alone PRESENTS
      it into a per-bucket buffer, and only the boundary panes go into
      the maps a reducer merges. The buffer road, not the threaded
      terminal — the node never learns what it is folded into.
      Measured back to back on one box: the three-plan road goes from
      647 ms (8.19x the hand-written lane) to 182 (2.25x), 3.6x, with
      every other lane unmoved. And the check is a COUNT, not a clock:
      `Run.merged` is reported by the single-stage road now and
      `TestFlow` asserts it equals the fan's exactly, at 2, 4 and 8
      partitions. Two things fell out — `prepass` answers an `Extent`
      rather than one Long (the road had been computing half of what
      the rule needs), and the LAST partition has no upper bound at
      all, since the two bounds guard two different neighbours and it
      has no later one. That last is why a run at ONE partition now
      merges nothing, asserted.
- [x] dataflow-fan-overhead — CLOSED by measurement, and the third
      is not there (MeasureFanOverhead, Live). Re-measured lane for
      lane: the fan is 101-111 ms and its three sinks, each run as its
      own fan, sum to 90-92 — a gap of 9-20%, not a third. The 50 ms
      came from arithmetic across differently-shaped lanes (the
      bunching sink has NO pre-pass; the fan's has two columns) on a
      loaded box, and the parts have changed under it since (topK
      stopped sorting). Of the three candidates: a pre-pass column is
      5-6 ms over 1.25M events and a second column in the same pass
      4-5; an `and` arm that does nothing is 0-3 ms, which is below
      this instrument. And the finding worth more than the entry: THE
      FAN IS NOT FASTER THAN THREE SEPARATE FANS on this feed
      (89-96 against 101-111) — one pass saves ~2 ms of source reads,
      because the source is an in-memory array, and pays more than
      that for three operators' state being live at once. What a fan
      buys is a source read ONCE, which matters when the source is a
      file, a topic or a socket, and no shuffle. Pricing the residue
      needs JMH; nothing has asked.
- [ ] (SUPERSEDED, kept for the reasoning) dataflow-complete-panes: 84%
      of the Wrocław run was the sliding stop-window sink, and not
      because the windowing is slow: the job makes 362 983 stop panes,
      every partition holds most of them, and the coordinator merges
      ~2.9 million accumulators on ONE thread.
      `OkayLane.parallel` does not parallelise that merge, it avoids
      it — it emits at the slice every pane no other slice can touch
      (`p.start > hi(i-1) && p.end <= hi(i) - back`) and hands back
      only the handful that span a boundary. The engine already
      computes `hi`: it is the prefix-maximum array gathered for the
      watermark seeding. What is missing is `back`, the greatest
      backwardness, which is two more columns in the same pre-pass
      (each partition's local backwardness and its minimum event
      time). The bar is MeasureWroclawFlow's table: 121 ms against
      the hand-written 22.
- [ ] dataflow-fan-exchange — a fan finishes by merge, and stage 3
      found a stage that wants otherwise (~3x10^5 accumulators per
      partition, above the 100 000 crossover). Consider only AFTER
      dataflow-complete-panes: that lane may remove the merge rather
      than parallelise it, and then this one has nothing to buy.
- [ ] dataflow-processes — stage 4: the worker protocol. Jobs by
      NAME plus typed parameters (Claim 3: nothing ships a closure),
      chunked framed transport, partials back. Acceptance: the full
      Wrocław Result across four OS processes.
- [ ] dataflow-recovery — stage 5: a lost partition recomputed from
      lineage (a partition is a thunk, so this is nearly free), and
      the same under `Sim` seeds rather than under luck.
- [ ] dataflow-streaming — stage 6: unbounded sources, the watermark
      as the minimum over input channels, keyed state in a backend
      (okay-persist), exactly-once OUTCOME at the sink.
- [ ] windows-packed-key — `Flows`'s windowed partial keys panes by
      `(Long, K)`, a tuple per update. §20's hand-written operator
      packs window and key into one Long and is faster for it; the
      general operator cannot, but a specialisation for small integer
      keys could. Measure before writing it.

## okay-spark
- [ ] spark-4-2 — bump `spark-sql` 4.0.0 -> 4.2.0 and move
      `scala-reflect`/`legacyStdlib` from 2.13.16 to 2.13.18 with it
      (4.2.0 resolves 2.13.18; the two must stay a matched pair, and
      build.sbt says so beside the pins). The suite passed on 4.2.0
      during the migration, so this is a read of the release notes and
      a gate, not an investigation. Spark is still 2.13-only at 4.2.0,
      so nothing about the `for3Use2_13` arrangement changes.
      (was filed under "spark-4-2" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] §20's Spark row is a BATCH job over RDDs: no event time, a window
      is a key, the arrival index carried through the shuffle. Spark's
      answer to an event-time question is Structured Streaming —
      `.withWatermark("ts", "30 seconds").groupBy(window($"ts", "5
      minutes"), $"route")` — and that is the lane that would be
      like-for-like with Flink rather than with `groupingBy`. What it
      needs, all of it known: a `SparkSession` (so the two-stdlib
      classpath hack, which means the lane lives in okay-spark's tests
      and prints its own table), encoders for the row type, a memory or
      rate source with `Trigger.AvailableNow`, a checkpoint directory,
      and reading the result back from the sink. `SparkInterop.toSpark`
      already gives the typed aggregator for the Dataset side, so the
      "one Aggregator, every engine" line holds there too. Trigger: a
      reader who asks what Spark's watermark costs against Flink's.

## okay-deploy
- [ ] deploy-cli-native — a GraalVM/Scala Native binary needing no
      JRE. The renderers are pure string builders and would port; the
      question is whether a second build toolchain is worth paying
      for, and nobody has asked yet.
      (was filed under "deploy-everywhere" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] deploy-host-verified — put the rendered systemd unit in front of
      a real `systemd-analyze verify` and the install script in front
      of a real rented box. Needs a Linux host; the unit is currently
      proven by its text, which is not the same thing.
      (was filed under "deploy-everywhere" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## okay-resilience
- [ ] **hedge-start-timing-flake** — `TestHedgeStart."an attempt forked
      while the answer arrives leaves neither a running attempt nor an
      armed timer"` fails a landing gate with "timed out waiting for
      the first attempt to answer" when the box is loaded (1-minute
      average ~19, three sbt matrices), and passes alone on the same
      tree seconds later. Same family as `parse-depth-tests-out-of-the-
      gate`, just landed: a wall clock on a shared box. Either the wait
      needs to be a condition rather than a deadline, or the test
      belongs in `integrationTest`. Seen 2026-09-10 by the
      dataflow-numbers gate, which does not touch okay-resilience.
- [ ] microservices-next — the audit's remaining gaps, each its own
      spec when picked. DONE 2026-09-09 (service-lifecycle): graceful
      shutdown and RED metrics, both in okay-ops. DONE 2026-09-09
      (outbox): transactional outbox / inbox / dead-letter as
      okay-outbox (specs/outbox.md). DONE 2026-09-09 (discovery):
      service discovery + client-side balancing in okay-resilience
      (specs/discovery.md). DONE 2026-09-09 (schema-compat): Schema
      compatibility between services, `okay.codec.Compat`
      (specs/codecs.md). DONE 2026-09-09 (obs-log): a Log effect with
      trace correlation, `okay.obs.Log` (specs/obs.md, "The third
      leg") — the audit's list is now closed except: saga over `Durable`
      + persist with compensations as values; transactional outbox /
      inbox / dead-letter when the truth is in SQL; service discovery
      + client-side balancing (cluster.md lists it out of scope);
      Schema compatibility checks between services; a `Log` effect
      with trace correlation (0 hits for one today).
      (was filed under "resilience" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## okay-persist
- [ ] the only thing genuinely absent is NUMBERED QUEUES — a ticket
      per waiter, served in order — and the entry filed that as a
      question rather than work. It stays a question: nothing in the
      tree asks for one, and `Channel` already serves waiters in
      order within a partition.
      (was filed under "leases" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## bulk — the loading seam (specs/bulk.md)
- [ ] bulk-plan-next — with a workload that asks: a `Where` whose
      predicate is structural (a column equals a value) pushed under a
      join, and a size estimate for a held table from the count its
      `Cache` already made. Neither is worth a line until something
      measures for it.

- [ ] bulk-parquet — `Bulk.csv` is the only source; the taxi demo
      (TestTaxiAlgebra) still reads its parquet through Spark's API.
      A `source` per format, or `Bulk.read(Format)`, with the local
      instance reading parquet without Spark (okay-delta already
      carries a Delta Kernel road, specs/data.md).

- [ ] bulk-flink — `flink-core` alone carries no DataStream; an
      instance needs flink-streaming-java. The seam's `Any`-element
      choice is what a `DataStream[AnyRef]` instance would do too.
      (2026-09-10: flink-streaming-java and flink-clients are now on
      okay-flink's TEST classpath for the §20 benchmark, so the
      dependency question is answered — a `Bulk` instance would still
      need them in `compile`.)

## okay-actor
- [ ] ~~Say so.~~ Not taken: the operator asked for the actors to
      WORK on JS, which is the second answer.
      (was filed under "actor-on-js" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## Refuted, declined or answered — do not retake blind

One line each; the measurement and the reasoning are in
`BACKLOG-ARCHIVE.md` under the same slug.

- writer-test-no-some — REFUTED 2026-09-09, no code landed. Built
  (ClassTag class test in the companion, Typeable fallback by given
  priority) and …
- single-shot-row — PRICED AND REFUTED 2026-09-09: a mutable cell buys
  Writer's reverse and nothing else (-8.9% B/op on the mixed program,
  gate was …
- handler-fusion-eff — DONE 2026-09-09, REFUTED: Eff + composite is
  0.58x of the fused Free loop, 2.4x the bytes; the best tree-free road
  0.86x …
- default-scheduler-shape — REFUTED 2026-09-08 by its own disqualifying
  evidence, and the entry was built on a mismatched pair besides.
  Matched by …
- growing-adopted-part0 — REFUTED 2026-09-08, and there was never a
  defect. The entry rested on a MISMATCHED PAIR, filed by me:
- chunked-lexer-bookkeeping — REFUTED 2026-09-09, nothing landed. The
  per-chunk bookkeeping was rewritten away (one traversal into a
  growable array) …
- lexer-buf-without-concat — REFUTED 2026-09-09, nothing landed. The one
  candidate that needs no input (a doubling char array in the state) is
  WORSE by …
- stm-sync-commit-fastpath — MEASURED AND DECLINED 2026-09-07 (§18e).
  Built as filed, with the first attempt inside a `Run` so nothing runs
  at …
- optics-fast — BUILT, MEASURED, DECLINED 2026-09-10 (operator's call;
  specs/optics.md "optics-fast"). The premise was refuted: compiling an
  optic to …
- sql-plan-cells — MEASURED AND DECLINED 2026-09-07 (taken up on the
  operator\'s word despite its own condition). Compiling each field\'s
  Shape into a …
- intent-live-provider — LANDED 2026-09-03, and it REFUTED the claim it
  set out to quantify: the early stop saves 0.0% against a real model,
  under a …
- intent-gate-non-english — LANDED 2026-09-04, and it REFUTED its own
  premise. Re-measured on domain-bearing names, the gate does not pay in
  any of six …
- channel-chunk-batch-size — REFUTED TWICE 2026-09-06: the consumer
  already batches at 62 of 64 Taken as channel-batch-floor on the
  finding that …
- intent-split-other — MEASURED AND DECLINED 2026-09-05. Carving the bin
  into Social/Support/Errand takes `Other` recall from 46.7% to 6.7%
  (composite …
- intent-label-model — MEASURED AND DECLINED 2026-09-07
  (MeasureLabelModel, offline). Six offline labelers combined by
  agreement-estimated weights …
- intent-offline-other — MEASURED AND DECLINED 2026-09-07
  (TestOfflineGate, offline, no network). The offline analogue of the
  model path's binary gate: …
- channel-per-element-effect-cost — CLOSED as an interpreter lane
  2026-09-06, redirected Taken as free-cont-stack on the hypothesis this
  entry invites: …
- actor-receive-offer-first — MEASURED AND DECLINED 2026-09-06: +19% in
  the regime that matters Built: `receiveNow(): Poll[A]` on `Channel`
  (default …
- source-unfold-tuple — DECLINED by design 2026-09-07 `Source.unfold`
  costs 13% over `Source.range` on its lane (section 6c),
- drain-copy-per-element — DECLINED by design 2026-09-07 `Drain` is a
  case class and `Stream[Drain, Async].uncons` answers
- native-interpreter-allocation — DONE 2026-09-06: the collector is not
  it; the count is six objects per bind §18: `bindChain`, N nested
  flatMaps with …
- adaptive-as-default — decided 2026-09-07: NO, and here is the number
  that would change it `Queues.strong.adaptive` wins many-to-many (0.63
  of our …
- jiffy-hole-scan — MEASURED and DECLINED 2026-09-07: a real latency
  hazard with no measurable throughput cost The operator pointed at
  Jiffy (Adas & …

## json-raw-nesting-jmh-pending (2026-09-10, found by json-raw-nesting-threshold-trampoline)

- [ ] `json-raw-nesting-threshold-trampoline` landed with correctness
      fully proven (184 tests, three platforms, an A/B at 100 000
      levels via a scratch `Codecs.maxDepth`/`NativeThreshold`) but NO
      trustworthy JMH number: system load climbed from ~20 to 88 while
      measuring (a shared box, unrelated to this lane), and
      `parseOnly`/`parseValueOnly` — two benchmarks with IDENTICAL
      bodies — read 282±26 vs 600±237 ns/op in the SAME run, proof the
      box could not answer the question regardless of fork count.
      Re-run `compare/Jmh/run -i 5 -wi 3 -f 5 .*parseOnly.*|.*parseValueOnly.*`
      once `uptime`'s load average is near the core count, and update
      `specs/iterative-recursive-decode.md`'s "Targets 1+2 done" entry
      with the real delta.
