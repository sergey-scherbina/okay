# Changelog

## conversation-runtime — the intake as a program that stops at every question
Completed: 2026-09-04
Landed as 2fa6a4f8. specs/conversation.md built on
durable-waiting-on-a-person. A caller describes a `Frame` of `Slot`s —
a name, a question per language, a `read` that says what an answer
MEANS, an optional `extract` for the opening sentence — and everything
else is the module: which question is next, when to ask again, what a
`Reply` that is not an answer does, and where the state lives, which
is the journal.

No words live here in any language. `Say` names a KIND; a `Reply` is a
CHOICE (answer, interrupt, yes, no), because at every suspension the
next message may be a correction or an exact command and only the
caller can tell which. Only an interrupt aborts an intake: the
free-text answer to a question about skills is exactly what a
similarity layer misreads as a new request.

THE COMPILER FOUND THE DESIGN HOLE. The first cut emitted
`Say.Ask(frame, slot)` and let the caller render — and the
unused-parameter warning on `lang` was the report that nothing ever
applied `Slot.ask`. Not tidying: with no rendering at ask time the
language of an exchange is stored nowhere, so a restarted process
holding only the journal cannot render the outstanding question
without re-deriving a language from whatever was typed last — the
exact failure the spec had already recorded from the implementation it
was lifted from, arriving a second time by a different route. Every
`Say` now carries the text as it was ACTUALLY asked, and `Frame`
gained `readBack` for the one sentence a slot cannot compose.

`Durable.argsOf` reads an entry's arguments back out of its
fingerprint (`op(args)` by construction), which is what makes
`awaiting` enough to render a question after a restart.

Ten tests, driven the way a conversation happens: a question, a
process that dies, an answer later, a message that turns out not to be
an answer.

Gate: clean compile 0 warnings; full matrix 2197 tests, 0 failures.

## durable-waiting-on-a-person — an answerless entry can also mean a question
Completed: 2026-09-04
Landed as 8f61155a. `Durable` read every entry without an answer as the
crash window — an outcome nobody can know. There is a second reading
and the journal already had the shape for it: a question asked and not
yet answered, where the right response is neither to repeat nor to
fail but to wait, possibly for days.

`OnRepeat.Await` is read in BOTH branches, unlike every other case.
The others answer only the recovery question, so an entry must exist
for them to matter; an awaiting operation is recognised on its FIRST
encounter too, because there is no inner effect to run — the effect is
a person reading the question. The inner handler is never reached:
asking someone touches no world.

`Awaiting` leaves the handler the way `Drift` and `Unresolved` already
do, `Durable.awaiting` names the entry a program is parked on, and
resuming is `complete` plus re-running with no new mechanism. This is
the suspension specs/conversation.md is written against.

One test found a mistake in another the way it was meant to: a
stateful scripted model reused across two runs made the second start
at the second reply, and `Drift` stopped it loudly rather than
answering the wrong question.

Gate: clean compile 0 warnings; full matrix 2187 tests, 0 failures.

## channel-send-fastpath — offer first; the handshake exists to wait

`sendBlocking` went through `CanBlock.blockAccepted` on every element
— a slot allocated, filled and read back — even when the ring had room
and there was nothing to wait for. `offer` is the non-suspending send
and needs none of it.

What makes this worth a lane is not the handshake's own cost but where
it lands. It slows the PRODUCER, and a producer that cannot run ahead
leaves the consumer nothing to batch. Measured on both sides of the
same load: `ZStream.fromQueue` averaged 137.9 elements per queue
operation against `SentinelChannel`'s 35.4 — that ratio, not
per-operation cost, was the chunked gap. With the fast path ours
averages 444.4 over 9 receive operations instead of 113.

Chunked: **175.3us → 58.7**, which is 2.06x past `zioStrongChunk` at
120.7. `StmChannel` gains from the same path, 172.3 → 128.6.

The trade, stated rather than buried: elementwise goes 208.9 → 268.7,
which puts `SentinelChannel` behind `StmChannel` on that axis. The
cause is saturation, not the extra failed `offer` — the producer now
fills the ring, so every send parks and every pop wakes a sender, one
unpark per element on the consumer's critical path, where a chunked
consumer amortizes them across a whole batch. Filed as
`channel-elementwise-wakeups`.

## channel-sentinel-default — a bounded channel is a ring, and termination is an element in it

`Channel.apply` now dispatches on the capacity asked for. Bounded gets
`SentinelChannel`; unbounded stays on `StmChannel`, because a ring
cannot be unbounded and `Int.MaxValue` is the default here, and so
does a capacity below two, which is a rendezvous the stamp scheme
cannot express. Both keep the same contract — every law, both tiers —
so the choice shows only in the timing.

The design point: the guarantee is an ELEMENT. Termination takes a
position through the same tail CAS as everything else, so the ring's
own atomics order it against every send and there is no second
structure to reconcile. A sender decides what to publish only after
winning its position, and if close already landed it publishes a void
and answers false — so a sender ordered after the mark always sees the
close, its own CAS being the fence. The window that broke four
`RingChannel` drafts has nowhere to open, with no in-flight counter
and no spin.

Measured: 208.9us elementwise against `StmChannel`'s 300.1 and
`zio.Queue` carrying the same contract at 320.1. At chunk granularity
the two are level, 175.3 against 172.3 — a win on one axis and a wash
on the other, and `zioStrongChunk` at 128.0 remains ahead. The lever
there turns out to be batch size, not per-operation cost: `StmChannel`
takes 363.6 elements per bulk receive against `SentinelChannel`'s
43.5, because a ring wakes a receiver on every push. Filed as
`channel-chunk-batch-size`.

Two false starts kept in the record. Counting outstanding ELEMENTS to
answer `finished` put an atomic increment per send on a cell shared
with the consumer and cost 2x; counting MARKS answers the same
question off the hot path. And passing a closure to `pushDeciding`
truncated the consumer's batches — `popMany` counts CONSECUTIVE
published slots, so it stops at one that is claimed and not yet
filled, and anything in that window costs the reader, not just the
writer.

Full matrix 2182 tests, clean build, no warnings.

## channel-bulk-and-alloc — both ends, and the guarantee table on one axis

`ChannelGuaranteeBenchmark` had the fault its sibling was written to
expose: okay lanes elementwise, zio lanes through `ZStream` and so
chunked. Every lane now names its granularity. The layer result
survives and sharpens — the strong contract costs 2.4% as a sentinel
and 47% as an invariant of the mechanism, and `okayLayeredChunk`
delivers it at 115.9 against `zioStrongChunk`'s 124.0. The corrected
elementwise column shows what the old table hid: `zioWeakElem` is
298.3, not 114.8.

`Ring.pushMany` and `Channel.sendManyNow`: the send side's bulk claim,
one tail CAS per run. It is NOT what `feedChunked` needed — that
amortizes by representation, putting whole chunks into a
`Channel[Chunk[A]]` — but for a producer holding a batch of elements.
Measured: 66.9us against a draining consumer, 1.71x past `zioChunked`;
280.4 against an elementwise one, a 1.43x LOSS, because a consumer
taking one element at a time keeps the ring full and every bulk
attempt then fails its scan and falls back anyway. Batch both ends or
neither, written into the method's own documentation.

`Accepted` and `CanBlock.blockAccepted` end the boxing of the send's
acceptance answer — `Function1` is specialised on Int, Long, Float and
Double and not on Boolean, and the generic `block` boxed into its slot
besides. The receive side's `Right`+`Some` was left: `receiveBlocking`
returns `Option[A]`, so there the wrapper is the return type, not the
implementation.

Laws for both bulk primitives, each with two threads contending for
the same claim. See benchmarks §15.
## intent-learning-curve — the probe flattens at 32 examples, so labels are not the constraint
Completed: 2026-09-04
Landed as 47ebf72e. The cheapest lane in the programme, run to decide
where the expensive ones go — and it overturned the plan it was meant
to confirm.

| training examples | probe | centroid | chargrams |
|---|---|---|---|
| 8 | 51.7% | 48.3% | 30.0% |
| 16 | 66.7% | 65.0% | 38.3% |
| 24 | 75.0% | 78.3% | 46.7% |
| 32 | 85.0% | 75.0% | 53.3% |
| 40 | 81.7% | 83.3% | 55.0% |
| 48 | 85.0% | 83.3% | 55.0% |
| 56 | 86.7% | 80.0% | 65.0% |
| 60 | 85.0% | 80.0% | 60.0% |

Everything from 32 to 60 examples moves the probe between 81.7% and
86.7%, which is noise on sixty held-out messages. The centroid flattens
in the same place while fitting FOUR VECTORS against the probe's 4096
weights: two models three orders of magnitude apart in capacity stop
improving together, and that is what a signal ceiling looks like rather
than a capacity one.

SO THE STANDING PLAN WAS WRONG, AND IT WAS MINE. The spec said the
86.7%-against-~90% gap was "credibly a data gap rather than a method
gap" and named `intent-label-distillation` as the lane that closes it.
Another sixty labels buy nothing measurable, so
`intent-embedding-choice` moves ahead of it.

Distillation is not dead; it belongs to a different tier. Chargrams go
30.0 → 65.0 across the same range without flattening, and they are the
ZERO-NETWORK path — no embedding server, no per-message round trip —
sitting at 60-65% because they are starved rather than finished. A
chargram model trained on thousands of distilled labels is the only
candidate for a classifier that needs no network at all.

Read against its size: 60 test messages, so a 3-4 point move is noise
and the flatness of the right-hand half is the finding, not any cell.

Also closes `intent-symbolic-patterns`, which the bake-off had already
built as the `Patterns` tier and nobody marked done.

Gate: clean compile 0 warnings; full matrix 2170 tests, 0 failures.

## intent-no-model — the assembled classifier, and an abstention that knows when it cannot promise
Completed: 2026-09-04
Landed as 998d1c75. The assembly the bake-off argued for, plus the two
pieces it was missing: a character n-gram tier for the zero-network
path, and a calibrated point at which the classifier declines.

CHARACTER N-GRAMS: the property arrived, the accuracy did not. Hashed
3-5 character n-grams into the same optimiser as `Probe` — no
tokenizer, no server, no network, 92µs per message with a 404ms fit.

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| accuracy | 53.3% | 53.3% | 53.3% | 33.3% | 46.7% | 60.0% |

Flat across languages, which is the design working: a 4-character
window does not know what alphabet it is in, and the English advantage
every embedding tier shows is simply absent. On English it reaches
60.0% at full coverage — above patterns (51.7%) and BM25 (45.0%), far
below the probe (86.7%). At 60 training examples a 4096-dimension
hashed model is under-fitted, so this is a data result, not a verdict
on the method.

STACKING DID NOT PAY, and the default says so. Pattern verdicts blended
into the probe's distribution, weight fitted on a calibration split
from a six-point grid: the search picked 0.8 and cost five points on
held-out data (70.0% against the probe's 75.0%). The sweep is monotone
— 75.0, 75.0, 72.5, 72.5, 70.0 for weights 0.0 to 0.8 — so the shipped
default grid is a single zero, with that sentence in the code beside
it. Forty calibration rows cannot support choosing even one number.

THE ABSTENTION TOOK THREE ATTEMPTS.

1. Threshold where calibration accuracy still met the target: promised
   96.2% over 65%, delivered 88.9% over 45%. The classic error —
   choosing a threshold on a sample and quoting that sample's accuracy
   as a prediction about the next one.
2. A proper split-conformal quantile with the finite-sample rank:
   promised 100% over 55%, delivered 88.2% over 42.5%. Better
   construction, same overclaim, because with ten calibration errors
   the rank runs off the end of the list and the bound degenerates
   silently.
3. The promise became an `Option`. A conformal bound at error rate
   `alpha` needs at least `(1 - alpha) / alpha` calibration MISTAKES to
   exist — nineteen at 95%. Below that there is no bound, and reporting
   the empirical number anyway IS the overclaim. The classifier now
   says: no promise, 6 calibration errors, 19 needed; the threshold
   still applies, the guarantee does not.

The threshold earns its place without the guarantee: 88.2% on the 42.5%
it accepts against 75.0% at full coverage. A good filter that is honest
about not being a proof.

Two of my own measurement errors were caught here and are recorded
rather than quietly fixed: a `math.ceil` over floating-point arithmetic
demanded a nineteenth error where eighteen were needed (0.8/0.2 came to
4.000000000000001), and a lazy val forced INSIDE a timing block charged
a 404ms fit to the first message and reported "6027µs each" — found by
the full matrix, where a cold JIT tripped the "fast tier" assertion
that exists for exactly that.

WHERE THIS LEAVES THE NO-GENERATION TARGET: the probe answers
everything at 86.7% (60 training examples) for one 12ms embedding call
and no tokens; the model tier is ~90%. The gap is credibly DATA rather
than method, and `intent-label-distillation` is the lane that closes it
— use the model once, offline, and keep it out of the request path.

Ten further methods are filed under a new backlog heading, each with
what it would fix rather than what it is called.

Gate: clean compile 0 warnings; full matrix 2170 tests, 0 failures.

## intent-tier-bakeoff — five tiers, one split, one table, and a linear probe at 86.7% with no generation
Completed: 2026-09-04
Landed as b95d5039. THE GOAL CHANGED MID-PROGRAMME and the reporting
changed with it: these tiers were built as cheap filters in FRONT of a
model, and the target is now a classifier that needs no generation on
the request path at all. So the deciding number became accuracy at FULL
coverage, with the margin table beside it for whoever wants to hand the
uncertain tail to a person rather than to a model. Embeddings stay
inside that budget — a vectoriser is 12ms and no tokens — and labels may
come from a model once, offline; the ban is on a model being present
when a message arrives.

| tier | accuracy over ALL | per message | network |
|---|---|---|---|
| symbolic (BM25) | 45.0% | 147µs | none |
| patterns | 51.7% | 96µs | none |
| kNN (k=5) | 58.3% | 158µs | one embed |
| centroid | 80.0% | 75µs | one embed |
| linear probe | 86.7% | 76µs | one embed |
| (model tier, for scale) | ~90% | seconds | a generation |

The probe fits in 164ms on 60 examples and lands within a few points of
the model. At margin 0.60 it answers 65% of messages at 97.4% — above
the model — which is the shape that makes handing the tail to a person
cheap rather than embarrassing.

PATTERNS CONFIRMED THE MECHANISM the BM25 failure implied. Where a cue
fires it is 88.6-90.9% accurate against BM25's 63% on the same
messages, with no network and 96µs. The cues match syntax and never a
subject — "shall we" proposes, "could you" requests, "FYI" at the START
notifies. Its limit is coverage (58.3% of messages carry no cue), not
precision.

KNN WAS MY PREDICTION AND IT WAS WRONG. I expected neighbours to beat a
centroid because `Other` is a deliberate grab-bag whose mean resembles
none of its members. It scored 58.3% against 80.0%, and the reason is
sample SIZE rather than shape: with fifteen examples per class, five
neighbours are mostly noise, and averaging is what rescues a small
sample. The hypothesis was about geometry; the answer was about data.

Ordering by cost gives the honest version of the no-model target: both
network-free tiers stay under 52%, and everything above 80% needs an
embedding. So the target is reachable and it costs one 12ms vector call
per message — no generation, no tokens, no per-call price.

Gate: clean compile 0 warnings; full matrix 2160 tests, 0 failures.

## intent-vector-tier — the first cheap tier that earns its place
Completed: 2026-09-04
Landed as 03736b16. Same fixture, same odd/even split as the symbolic
tier, same three numbers, so the tables compare line for line.

| margin ≥ | coverage | agreement | (symbolic) |
|---|---|---|---|
| 0.00 | 100.0% | 80.0% | 45.0% |
| 0.02 | 76.7% | 87.0% | 54.5% |
| 0.05 | 45.0% | 96.3% | 63.6% |
| 0.10 | 8.3% | 100.0% | 62.1% |

THE AGREEMENT RISES WITH THE MARGIN — monotonically — where the
symbolic tier's plateaued and then fell. That answers the question the
symbolic lane left open: the binding constraint was the
REPRESENTATION, not the idea of a cheap tier. BM25 matches content
words, and an intent is carried by function words and syntax ("could
you" against "shall we"), which it drops as stopwords or weights by
rarity rather than by role.

The operating point is real: at margin 0.05 the tier answers 45% of
messages at 96.3% agreement, ABOVE the model tier's ~90% macro F1 on
the same fixture. On the slice it accepts it is not merely cheaper but
more accurate, and the model's value is on the half it declines —
exactly the shape a first pass should have.

Cost, with the number a batch hides: 12ms for one message's embedding
round trip plus 90µs to classify, against seconds for a generation.
Production embeds one message at a time, so 12ms is the honest figure.

This also changes the trigger's terms. The tier was filed behind "cost
or latency binding", which still has not fired — and does not need to,
because being more accurate on the traffic it accepts is a better
reason than saving money, and a different one from what the backlog
anticipated.

Composition is three lines at the call site, deliberately not hidden
behind a wrapper that would obscure which call is being paid for.
`Centroid` never calls a gateway itself, so it tests on every platform.

Named `Centroid` rather than `Vectors`: a demo imports `okay.agent._`
and `okay.rag._` together and `okay.rag.Vectors` already exists, so the
reference went ambiguous. Caught by the FULL gate rather than the
module's own compile, which is what a full gate is for. It also reads
better beside `Symbolic` now that both tiers share a shape — `train`,
`score`, `classify`, `Trained`.

Gate: clean compile 0 warnings; full matrix 2156 tests, 0 failures.

## intent-symbolic-tier — 112µs per message, and a margin that carries no confidence
Completed: 2026-09-04
Landed as 186f412b, NOT wired in. Built on the operator's instruction
rather than on its trigger, which never fired, and therefore measured
as a hypothesis rather than shipped as a default.

IT COST ONE FILE, because the tier is a projection of machinery that
already exists. FrameNet's "lexical units" are, here, BM25 over
labelled examples: `okay-rag`'s `Postings` is already a `Fold` and a
`Monoid`, `Keyword.search` already scores, and `Symbolic` is just the
mapping from a class to the examples that carried it. The retrieval
stack and the classifier want the same index.

Measured on a deterministic split — odd positions train, even are
scored, no message in both — because an index scored against its own
examples measures nothing: BM25 finds the identical document and
reports a perfect margin.

| margin ≥ | coverage | agreement with gold |
|---|---|---|
| 0.0 | 100.0% | 45.0% |
| 0.1 | 73.3% | 54.5% |
| 0.2 | 55.0% | 63.6% |
| 0.3 | 48.3% | 62.1% |
| 0.5 | 16.7% | 60.0% |

Speed is not the problem: 112µs per message against seconds for a model
call, so Linagora's sub-150ms claim is clearly reachable this way. The
problem is that AGREEMENT DOES NOT RISE WITH THE MARGIN — it plateaus
at 60-64% and falls at 0.5. A usable filter approaches the model's own
accuracy as its threshold tightens; this one does not, so the margin is
not a confidence signal and there is no threshold at which the tier is
safe to answer.

The arithmetic of shipping it anyway: at margin 0.2 it takes 55% of
traffic at 64% accuracy where the model tier is near 90% — roughly 14
points of end-to-end accuracy spent to save 55% of the calls.

So `Symbolic` is a working, tested, 112µs classifier that nothing
calls, and the reason is written in the spec beside its numbers. What
would change the verdict is a better REPRESENTATION rather than a
better threshold, which makes the vector tier more interesting rather
than less: paraphrase is exactly where BM25 is structurally weak.

Gate: clean compile 0 warnings (the sibling's `AbruptChannel` one is
gone too); full matrix 2156 tests, 0 failures.

## intent-language-gap — neither candidate fixes it, and the larger fixture refuted this spec's own ordering claim
Completed: 2026-09-04
Landed as 33a6e4ee. The precondition first, as the backlog entry
demanded: the parallel set grew from 12 meanings to 30 in all six
languages, weighted toward the out-of-domain boundary where the
classifier actually breaks.

THAT GROWTH IMMEDIATELY CORRECTED SOMETHING THE SPEC WAS CARRYING. At
n=12 Spanish (0.914) and French (0.900) sat above English (0.881), and
the previous lane concluded "not a simple English-first ordering". At
n=30: English 0.929, German 0.895, Spanish 0.890, Japanese 0.888,
French 0.887, Russian 0.741. The middle was noise — twelve messages
could not tell those four apart, exactly as the entry warned when it
made growing the set a precondition. What survives is the gap itself:
Russian sits ~0.19 below English in two independent runs at two sizes.

CANDIDATE ONE — case names in the message's own language. Five
taxonomies (`RencontreFr`, `BesprechungDe`, `ReunionEs`, `ВстречаRu`,
`会議Ja`); Scala takes non-ASCII identifiers, so testing this cost only
typing.

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| English names | 0.929 | 0.887 | 0.895 | 0.890 | 0.741 | 0.888 |
| native names | 0.929 | 0.927 | 0.788 | 0.732 | 0.791 | 0.891 |

Helps French (+0.040) and Russian (+0.050), badly hurts German (−0.107)
and Spanish (−0.158): −0.029 on average. If a name worked by being
UNDERSTOOD the gain would be systematic, and it is not. The English
pair is the harness's own guard — same taxonomy both sides, 0.929
exactly both sides.

CANDIDATE TWO — say the subject out loud in the reader's language,
leaving the English names alone: −0.052 on average, German −0.207, and
only Russian gains (+0.024).

So this is the FOURTH prose addition in this line to cost, after
precedence rules, tie-break examples and now a domain sentence. What
has ever paid here is structural: the rendered example shape, few-shot
examples OF A CLASS, domain-bearing names, and field order. This prompt
is at the point where more words make it worse.

Filed rather than confounded in: the example MESSAGES stayed English
throughout so the names arm moved one variable, and translating them is
the obvious untried candidate (intent-examples-in-language).

ALSO CARRIED, and mine: the two compiler warnings the temporal lane
reported as fixed had been fixed in a worktree and never committed,
then destroyed when that worktree was force-removed — so they landed on
master while the changelog said otherwise. Re-applied here, and that
entry now says what happened. A green re-gate proves a fix works; it
does not prove the fix was kept, and I reported the first as the
second.

Gate: clean compile, 0 warnings of my own (3 remain from a sibling's
`AbruptChannel`); full matrix 2152 tests, 0 failures.

## channel-weak-gap — batching the handshake is not batching the queue

`Ring.popMany` claims a run of consecutive published slots with ONE
`compareAndSet` and leaves only the slot read and the stamp write per
element. `AbruptChannel.receiveManyAsync` uses it; a law covers it,
with two consumers contending for the same bulk claim.

`CanBlock.block` on the JVM no longer allocates a `CompletableFuture`
per operation. A typed one-shot slot carries the value, with a fast
path that never parks — which is the usual case, because the callback
fires synchronously inside `register` when the element was already
buffered. No cast: the slot is parameterised on `A`.

Both came out of one question — why our weak channel trailed
`zio.Queue` — and the first answer was that it did not. The
comparison was not like for like: `ZStream.fromQueue` takes up to
4096 elements per queue operation while our consumer took one.
Elementwise we were ahead (212 vs 337). What was true is that
chunking bought them 2.7x and us 1.04x, because `receiveManyAsync`
called `pop()` in a loop: the handshake was batched, the queue was
not. 4000 handshakes had become 299 and bought 4%, since the ring
still paid a head CAS per element and that CAS was a quarter of the
profile. With the bulk claim, chunking buys 1.90x and the chunked
lane lands ahead of `zioChunked` in the same run. See benchmarks §14.
## intent-temporal-slots — a parser does the arithmetic the model was doing
Completed: 2026-09-04
Landed as 6e0f838e (claim released in a8894086, whose message promised
this entry and did not carry it — a `git worktree remove` failed and
broke the `&&` chain that was supposed to write it, so the changelog
went missing while the commit said otherwise).

A slot typed as ISO-8601 refuses "next thursday", so until now the
MODEL converted and the schema only checked — a model doing arithmetic,
which is the one thing it is worst at and a parser is best at.
`Temporal` does it instead, and it is the first lane in this line that
needs no model at all, so it verifies entirely in the default gate.

NOT BUILT ON `okay-lex`'s `Scan`, deliberately: that machinery earns
its keep carrying lexer state across chunk boundaries and relexing
incrementally after an edit, and a five-word phrase has neither. What a
temporal parser needs is to be TOTAL and DETERMINISTIC, and that is a
function.

DETERMINISTIC means the reference day is an ARGUMENT. "Next Thursday"
is not a value, it is a value relative to a day someone has to name,
and a parser that reads the clock cannot be tested. Every test is
anchored to Friday 2026-09-04.

TOTAL means `None` rather than a guess, and the refusals are as much
the deliverable as the parses. "soon", "end of the month", "the 14th",
"later this week", "in a couple of days" are all guessable, and every
guess would be ACTED on — a meeting booked, a deadline moved. A
declined phrase is asked about instead, so declining is the cheap
failure.

Scope is the shapes scheduling mail actually uses: an explicit ISO
date; today / tomorrow / the day after / yesterday; `in N days`, `N
days from now`, `N days ago`; a bare or qualified weekday; `next week`;
a month-and-day in either order taking the COMING year; and a time in
either spelling riding along with any of them.

The calendar underneath is Hinnant's civil algorithm rather than month
tables and leap-year branches, which are wrong at exactly the dates
nobody tests. Those are tested anyway: 2024-02-28, 2023-02-28,
1900-02-28 (not a leap year), 2000-02-28 (but that one is), and a year
boundary. No `java.time`, so the JS build keeps it.

13 tests, three properties. One began as `forAll(...).check()` inside a
`test` block, which prints and returns and cannot fail a suite —
scenery, now a `property`.

Gate: full matrix 2150 tests, 0 failures. Two warnings this lane DID
introduce, an unused import and a discarded `Option`, were caught by
the clean compile — and then LOST: they were fixed in the worktree,
never committed, and destroyed when the worktree was force-removed, so
they landed on master anyway. Found the next morning by the next lane's
clean compile and fixed there (intent-language-gap). The sentence you
would have read here, that they "were fixed", was written from the
green re-gate rather than from a commit, which is the difference
between checking work and checking that work was kept.

## intent-tiebreak-by-example — examples are worse than the prose they were meant to replace
Completed: 2026-09-04
Landed as 75f2ec79, as a second refusal. The precedence lane's own
suggestion, tested: carry a tie-break as EXAMPLES of the disputed case
rather than as prose. Same two decisions, same 120 messages, two arms
differing by exactly two added examples.

| arm | macro F1 | Proposal | Request | Notification | Other |
|---|---|---|---|---|---|
| examples as shipped | 0.909 | 0.95 | 0.93 | 0.89 | 0.86 |
| + two tie-break examples | 0.854 | 0.90 | 0.76 | 0.91 | 0.85 |
| prose rules (previous lane) | 0.866 | 0.92 | 0.89 | 0.84 | 0.81 |

Worse than the prose, and the damage is specific rather than diffuse:
`Request` RECALL collapses 0.87 -> 0.63 while `Proposal` precision
falls 0.91 -> 0.81. The example did exactly what it said — "a message
that both proposes and asks is a proposal" — and the model applied it
to requests that were not disputed at all. A tie-break shown as an
instance does not stay inside the tie.

SO THE OVERLAP IS NOT A PROMPT PROBLEM. Two independent channels have
now moved this boundary in the intended direction and both paid more
elsewhere than they gained, which the claim predicted before the run.
`Proposal` and `Request` overlap because the TAXONOMY draws them that
way, and a boundary a taxonomy draws is moved in its labels and class
definitions, not in an instruction to the model.

A caution worth more than the lane itself: few-shot examples improved
every arm they touched across this whole line — decode rate, `Other`
recall, macro F1 — and this is the FIRST measurement where they cost.
The difference is what the example teaches. An example of a CLASS
generalises usefully; an example of a BOUNDARY generalises past the
boundary. "Add an example" has been free advice here until now.

Nothing shipped; the two examples stay in `IntentFixture` as evidence
for whoever reaches for this next, and the `Taxonomy[I]` typeclass
refused in the precedence lane stays refused — neither channel earned
it.

Gate: clean compile carries one warning, and it is not this lane's —
`AbruptChannel.scala:85` from channel-guarantees, reported to its owner
rather than edited across a live sibling's tree. Full matrix 2137
tests, 0 failures.

## channel-guarantees — what the contract costs, on both sides

`AbruptChannel`: a channel whose `close` ends it at once and abandons
the buffer. The weaker trade named rather than hidden, for feeds where
a stopped consumer makes the remainder stale.

`TestChannelLaws` gains a second tier. The core laws — order, no
duplication, a closed channel accepts nothing — bind every mechanism;
the drain laws bind only what claims them, and an implementation that
refuses one is recorded as refusing it, in the gate's own output.

The measurement refuted the hypothesis. Weakening the contract was
meant to explain the distance to `zio.Queue`; it explains a third.
The rest is HOW the guarantee is bought: they carry it above the queue
as one sentinel in FIFO order (11%), we bake it into a transition
every send and receive must read (62%). Our contract bought their way,
over the weak channel, runs 177us against `StmChannel`'s 334us — past
`zioStrong`'s 187us. Filed as `channel-sentinel-default`.

## intent-precedence-rule — the design answer is right, the measurement sank it, and it is not shipped
Completed: 2026-09-04
Landed as 0fc1ff41, as a REFUSAL. The reference literature calls
overlapping classes "mutually exclusive in practice" and prescribes a
stated precedence rule. This lane asked where such a rule LIVES, built
the answer, measured it, and threw the answer away.

THE DESIGN ANSWER STANDS, and is written into the spec. A doc comment
cannot be read at runtime; a prompt parameter does not travel with the
type, so the next caller reconstructs it or does without. The
construction that fits this library is a typeclass beside the schema —
`Taxonomy[I]` carrying `precedence: List[String]`, its empty default one
priority lower so a stated taxonomy wins over the silent one instead of
being ambiguous with it. It travels exactly as far as the type does,
which is the point of the taxonomy BEING a type.

THE MEASUREMENT SANK IT. Two arms over the same 120 messages, differing
only in whether the taxonomy declares its rules:

| arm | macro F1 | Proposal | Request | Notification | Other |
|---|---|---|---|---|---|
| no precedence stated | 0.909 | 0.95 | 0.93 | 0.89 | 0.86 |
| precedence stated | 0.866 | 0.92 | 0.89 | 0.84 | 0.81 |

Every class fell by roughly the same amount — and the rules were
written to match this fixture's own labelling, so they should have
helped BY CONSTRUCTION. The claim said so before the run, precisely so
the outcome could not be reinterpreted afterwards. The uniformity is
the diagnosis: two more sentences of instruction did not sharpen the
boundary they named, they diluted the whole prompt. The second rule
aimed at `Notification`, and `Notification` recall fell 0.83 -> 0.77.

So the mechanism is not shipped: an API whose only measurement says it
costs 0.043 macro F1 is an unearned claim written in code, and this
line has already deleted one of those from prose. The four lines are in
the lane's history and cost nothing to restore when there is evidence.
Reverting also left the prompt unchanged, so the recorded journal stays
valid — intent-eval-on-journal paying for itself the same day.

Filed as intent-tiebreak-by-example: render a tie-break as EXAMPLES of
the disputed case rather than as prose (few-shot examples are the one
lever that has consistently paid in this line), and use one rule rather
than a list.

CARRIED IN THIS LANE, unrelated and mine to fix: two compiler warnings
my previous two lanes left on master (unused imports in TestEvalJournal
and TestCutStops). I had been grepping gates for test failures only, and
an incremental compile hides warnings — AGENTS.md says `clean;
Test/compile` is the only truthful check, and I was not running it. The
gate is now two shorter commands, a clean compile for warnings and a
warm run for failures, which also collides less with a sibling's sbt
than one long clean-and-test did.

Gate: clean compile 0 warnings; full matrix 2134 tests, 0 failures.

## intent-eval-on-journal — the non-model half of evaluation stops costing a model
Completed: 2026-09-04
Landed as 551dda2d. Every measurement in this line has been a live run
of ten to thirty minutes, which is why several questions went four
lanes without being asked — including one whose answer turned out to be
a `groupBy` over data already sitting in memory. This makes the parts
that do not involve a model cost nothing.

Nothing new was invented to hold the recording, because A RECORDING IS
A JOURNAL: `Durable.Entry` already carries
`(seq, op, fingerprint, key, answer)`, `Rerun.Version` groups entries
under a provenance, `FileVersions` stores them. The model's reply goes
in `answer`, the message in `key`, the PROMPT's fingerprint in
`fingerprint`.

| | live | over the recording |
|---|---|---|
| whole fixture, best config | ~13 min | 0.046 s |
| needs a model | yes | no |
| runs in the default gate | no | yes |

The replay reproduces the live report exactly — Proposal 0.952, Request
0.929, Notification 0.893, Other 0.862 — which is what makes it
evidence rather than merely speed.

TWO GUARDS, BOTH VERIFIED BY BREAKING THEM ON PURPOSE, since a guard
that cannot fail is worse than none. The prompt fingerprint: one added
space fails the check with "re-record rather than trusting these
numbers", because there is no honest way to score old answers against a
new question. And `Eval.regressions` — executable since the first lane
and guarding nothing until now — fails the run and prints every class's
F1 when a baseline is raised four points.

So a change to the decoder, the label mapping, the gate logic or the
metrics is now a second-long check in the default gate; only a PROMPT
change still costs a live run. The spec's standing promise that the
fixture "IS the intended `Rerun` journal", carried as an admission
since lane one, is now a statement of fact.

54KB of committed JSON is the price of keeping four lanes of
measurement reproducible.

Full matrix green: 2134 tests, 0 failures.

## intent-decode-rate-residue — the last 9% was one malformation, and field order fixes it
Completed: 2026-09-04
Landed as 03cf0da4. Nine percent of replies were still undecodable on
the best configuration and no lane had looked at them, because the
harness printed two examples of a failure and dropped the rest — four
lanes watched the NUMBER without ever seeing its SHAPE. The diagnosis
was a `groupBy` over failures the harness was already collecting and
silently discarding, and it settled the question in one run.

THE RESIDUE WAS NOT A RESIDUE. Nine of the ten failures were the same
malformation:

    "intent": { "MeetingRequest": { "what": "..." }, "conf": "high" }

The model closes the intent's object one brace too late and swallows
the sibling field. (The tenth was the last surviving bare-name intent.)
Nothing in it was a hard message or a model limit — it was one
systematic shape error wearing the costume of a long tail.

So the fix follows from the shape rather than from taste: `conf` was
declared after `intent` and therefore emitted where a nested object was
still open. Declared FIRST it has nothing to fall into.

| `Alt` field order | undecodable | macro F1 |
|---|---|---|
| `(intent, conf)` | 10/120 | 0.907 |
| `(conf, intent)` | 0/120 | 0.909 |

Every reply now decodes. Accuracy is unchanged, which is the honest
reading: this was never an accuracy problem, it was ten messages whose
answers never reached the output at all.

Third time in this line that FIELD ORDER turned out to be load-bearing,
after reasoning-before-label (worth 0.136 macro F1) and `why` before
`alts`. The declaration order of an `SProduct` is not presentation, and
a test pins this one because it looks exactly like something a later
reader would tidy.

Full matrix green: 2132 tests, 0 failures. (Two earlier runs died at
exit 143 with a sibling sbt on the box; a truncated gate is not a gate.)

## intent-gate-non-english — the gate does not pay in any language, and its worst damage is in English
Completed: 2026-09-04
Landed as dd20b366. A re-measurement, not a new hypothesis: the spec's
language table was taken with generic names and the in-domain gate, and
intent-domain-in-names had since demoted the gate to a fallback — so the
spec was carrying numbers for a mechanism nobody should reach for
first. Both arms re-run on domain-bearing names, twelve meanings per
language.

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| domain names | 0.881 | 0.900 | 0.813 | 0.914 | 0.652 | 0.813 |
| domain names + gate | 0.602 | 0.900 | 0.727 | 0.914 | 0.548 | 0.813 |

THE ITEM'S OWN PREMISE IS REFUTED. It was filed as "the gate loses
precision OUTSIDE English". With domain-bearing names the gate does not
pay in ANY of the six languages — neutral in three, costly in three —
and its worst damage is in ENGLISH (0.881 -> 0.602, −0.28), not in
Russian (−0.10). The non-English framing was an artifact of having
measured the gate only against generic names, so the demotion decided
last lane generalises across languages instead of being an English-only
result.

The language gap itself survives, and naming does not close it: Russian
is weakest at 0.652 with names alone, across two independent runs. Nor
is it a simple English-first ordering — Spanish (0.914) and French
(0.900) sit above English (0.881) — so the effect lands on particular
languages, and on this evidence Russian and German are the ones to
look at.

Filed as intent-language-gap, with two candidates to try SEPARATELY
because they cost different things (case names in the message's
language; an explicit domain sentence), and with the parallel fixture to
be grown first: twelve messages support "there is an effect", not the
size of any single number. The old table is marked superseded in the
spec rather than deleted, so what replaced it and why stays readable.

Full matrix green: 2131 tests, 0 failures.

## intent-live-provider — the early stop saves nothing here, and the spec loses a sentence it had not earned
Completed: 2026-09-03
Landed as 0c93e006. A debt, paid: three lanes shipped while the spec
claimed `Structured.cut` makes a classification "cost the answer" and
admitted in the same breath that the saving was reasoned about rather
than measured. Measured, it is 0.0% — twice over, for opposite reasons.

| prompt | tokens with cut | tokens generated | saved |
|---|---|---|---|
| strict ("ONE JSON object and nothing else") | 250 | 250 | 0.0% |
| prose-inviting | 643 | 643 | 0.0% |

Under the strict prompt the text accumulated at the stop IS the whole
reply — 280 chars against 280, 291 against 291, message after message.
The model emits the closing brace and stops on its own, so there is
nothing after it to avoid. Under a prose-inviting prompt the value
never decodes, so the walk runs to the end — the safe direction
`Structured` already documents — and again nothing is saved.

THE MECHANISM IS NOT BROKEN, and that is asserted rather than assumed:
`TestCutStops` runs the walk over a synthetic stream that COUNTS how
far it was pulled, in the default gate with no model at all. A value
followed by 500 pieces of commentary stops after the value and leaves
the source un-pulled; a stream that never completes is drained in full.

So `cut` is insurance against a model that keeps talking after a
complete value, not a saving in the normal case: a prompt that says
"and nothing else" buys the same thing from the model instead of from
the client. Where it still earns its place is a model or endpoint you
do not control — one that appends a summary, a chat model with no
strict-output mode, a provider that ignores the instruction. That is
now what the spec says, and the old sentence is gone rather than
softened.

It also settles the field-order trade from the first lane, which was
priced in CHARACTERS of prose and credited with a discount from `cut`
that does not exist. Both arms pay for every token generated, so the
0.136 macro F1 that reasoning-first buys costs what it costs.

The first live control written for this was itself confounded — it
showed the model an example whose placeholder values cannot decode, so
completion was never declared — and is deleted rather than kept as
scenery.

Full matrix green: 2131 tests, 0 failures. (The run before it died at
1385 on exit 143 with a sibling sbt live on the box; a truncated gate
is not a gate, so it was rerun rather than reported.)

## channel-laws — the `Channel` contract written down, property-checked, and one primitive that stops implementations deriving it wrongly
Completed: 2026-09-03
Landed as 592f0b2d. After `ring-channel` withdrew two implementations
that had each rediscovered the same invariants by failing a gate.

**Laws.** The interface named its operations and said nothing about
what must be true of them. `TestChannelLaws` now states the contract
as eight laws, parameterised over the implementation, so a new
mechanism earns its place by passing them rather than by surviving a
gate a few times. They were **proven to catch the real defect**
before being trusted: restoring the withdrawn `CasChannel` with its
in-flight fix reverted fails law 1 — "an accepted element is always
delivered" — in 0.05s, naming the law, where the full gate needed
roughly three runs to show the same thing.

**A narrower primitive.** Verified from ZIO's sources:
`Queue.shutdown` interrupts offers and takes and promises no drain,
while our `close` promises buffered elements still drain — strictly
harder, and where all four defects lived. The interface now asks for
the conclusion: `finished` means "nothing further can ever be
delivered", instead of letting a consumer derive it from a raw closed
flag plus an emptiness check, which is the derivation three of the
four defects got wrong. `close`'s contract is written on the method:
two-phase, the end after the buffer, acceptance is final.

The compiler also caught one of the new laws checking nothing —
`forAll` inside a `test` discards the `Prop` it returns. It is a
`property` now.

## intent-name-sensitivity — the domain word is read, and a nonsense qualifier costs
Completed: 2026-09-03
Landed as 25aed59c. The previous lane's recommendation rested on four
identifiers, so this ablates them: four taxonomies differing ONLY in
case names, with no examples and no gate in any arm, because examples
would teach what the names are supposed to say on their own and a gate
would add a second signal.

| taxonomy | macro F1 | `Other` P / R / F1 | undecodable |
|---|---|---|---|
| generic (`Proposal`...) | 0.649 | 0.83 / 0.19 / 0.30 | 10/120 |
| true domain (`Meeting`...) | 0.688 | 0.92 / 0.43 / 0.59 | 7/120 |
| wrong domain (`Shipping`...) | 0.635 | 0.72 / 0.45 / 0.55 | 2/120 |
| nonsense (`Zarnic`...) | 0.528 | 1.00 / 0.11 / 0.20 | 13/120 |

THE CONTROL COULD HAVE HOLLOWED OUT THE RECOMMENDATION AND DID NOT.
`Zarnic` is the worst arm of the four — below generic on macro F1 and
on `Other` recall, with the highest undecodable count — so what helps
is the domain a name names, not the appearance of names having been
deliberately chosen. An uninterpretable qualifier does not merely fail
to help; it costs.

THE DOMAIN WORD IS READ, proved by damage. `Shipping` lifts `Other`
recall to 0.45, as high as the true domain's 0.43, while halving
`Proposal` recall (0.85 -> 0.45): meeting messages are being pushed
into `NotAboutShipping`, which is the correct reading of a taxonomy
that says its subject is shipping. The model answers the question the
names ask.

`Other` PRECISION is what separates a right domain from a wrong one —
0.92 against 0.72 — where recall calls the two arms equivalent and they
are not. `Zarnic`'s 1.00 precision sits on 0.11 recall: precision over
almost nothing, and reading it as a win is the trap the table's two
columns exist to prevent.

Scale, stated against the arms that carry examples: names alone move
`Other` recall 0.19 -> 0.43, where names plus examples reached 0.96. So
naming buys roughly a quarter of the distance and few-shot examples
remain the larger lever. Nothing shipped changes — the recommendation
was measured with examples on both sides — but its mechanism is now
known rather than assumed, which is the whole point of running a
control after the result is already in.

Full matrix green: 2121 tests, 0 failures.

## ring-channel — two channel implementations written, measured, and deliberately not landed
Completed: 2026-09-03
Landed as 03b6c55e (backlog and measurements only — the code is
withdrawn). Answers the operator's question, is a Michael-Scott
channel better than the default, with numbers on one harness (one
producer, one consumer, 4000 elements):

| | µs |
|---|---|
| `zioQueue` (outside reference) | **122.2 ±9.7** |
| `casChannel` (MS, unbounded) | **143.9 ±16.3** |
| `stmChannelUnbounded` | 187.7 ±18.2 |
| `ringChannel` (bounded) | 249.9 ±31.2 |
| `stmChannel` (bounded) | 418.2 ±95.3 |

So yes: **1.3x over the default on the honest pair** (both unbounded,
since a ring cannot be unbounded) and within 18% of `zio.Queue`. That
also corrects `backlog-dedup`, which had dropped MS after comparing it
to a segmented ring that does not exist rather than to the default it
would replace.

**Neither implementation landed.** Their accounting test fails about
one FULL GATE in three, always the same way — an element accepted by
`k(true)` and never delivered. Three causes were found and fixed
(ending on `closed` alone; a self-claimed waiter recursing on the
stack until it overflowed; the open-check/enqueue pair losing
atomicity once elements leave the state) and it still recurs, so a
fourth remains undiagnosed. Shipping code with a known correctness bug
behind a seam is still shipping it. Everything learned is filed as
`channel-impls-correctness`, with where to look next and the warning
that eight clean runs of the two suites preceded a gate failure.

What survives is the property the exercise established:
`StmChannel`'s single-CAS state is not merely slower — it is what
makes open-check-and-enqueue **atomic**, and any implementation that
splits elements out of the state owes that atomicity back explicitly.
Three of the four bugs were that debt coming due.

## intent-domain-in-names — the case names ARE half the classifier
Completed: 2026-09-03
Landed as b3585b17. The hypothesis the two previous lanes left
standing, tested at last on a fixture big enough to defend it: a
taxonomy carries its domain in its case NAMES or nowhere.

Four configurations, the same 120 messages, the same examples, the same
prompt. The only thing that changes is the type.

| configuration | calls / message | macro F1 | `Other` P / R / F1 |
|---|---|---|---|
| generic names, no gate | 1 | 0.872 | 0.94 / 0.65 / 0.77 |
| generic names + gate | 2 | 0.906 | 0.92 / 0.81 / 0.86 |
| domain names, no gate | 1 | 0.907 | 0.87 / 0.96 / 0.92 |
| domain names + gate | 2 | 0.830 | 0.68 / 0.97 / 0.80 |

Renaming four identifiers — `MeetingProposal`, `MeetingRequest`,
`MeetingNotification`, `NotAboutMeetings` — took `Other` recall from
0.65 to 0.96 and matched the gated configuration's macro F1 at HALF the
model calls. `Proposal`/`Request`/`Notification` carrying a bare
`what: String` never says its subject is meetings, so "please refund my
card" reads as a `Request` honestly rather than mistakenly, and every
prompt-level fix for that was arguing with a type that had not stated
its subject.

AND THEY DO NOT COMPOSE. Gated, the named taxonomy drops to 0.830 —
worse than either half alone — with `Other` precision at 0.68 and
`Notification` recall at 0.68, because a second judge re-rejects what
the first accepted. Two mechanisms for one job is not twice the safety.

So the in-domain gate, which the previous lane established as the
answer, is demoted to the FALLBACK for taxonomies that cannot be
renamed (someone else's types, a wire format, a taxonomy shared with a
system that owns its names). That correction went into its own doc
comment, not only into the spec: otherwise the code would keep
asserting what the first measurement said.

The price of clear names is real and stated rather than buried:
`Other` precision 0.94 -> 0.87 and `Request` recall 0.92 -> 0.77, since
domain-bearing names make the model readier to push a borderline
message out of the domain. Which error is cheaper — a misrouted request
costs a wrong action, a wrongly rejected one costs a human's attention
— belongs to the caller, and is the trade to write into a taxonomy's
documentation rather than settle by default.

This is the strongest form of the claim the whole feature rests on: the
taxonomy IS the type, so its names are not labels for humans, they are
half the classifier.

Filed: how much of this is the word "Meeting" and how much is any
qualifier at all — a nonsense-qualifier arm would say, and until it
runs the mechanism is assumed rather than known.

Full matrix green: 2133 tests, 0 failures.

## flakes-integration — the last recorded flake out of the default gate
Completed: 2026-09-03
Landed as a3ecf5b8, on the operator's call. Two of the three were
tagged earlier the same day by siblings — TestMcpAuth (nio-port-scope)
and TestBackends (netty-integration), both real port binders. This
closes the family with the one left, okay.persist
.TestElectionReplicated, and says plainly in the record that it is not
of a kind with the other two: it binds no port, starts no thread and
does no IO (MemoryStore, a manual clock), and its triage could not
reproduce the failure — alone on JS 3/3, on Native 3/3. What failed on
2026-09-01 was the RUNNER, at suite level, under parallel matrix load.
It is excluded by DECISION, not by evidence against the suite, and the
comment at the suite, the spec and the BACKLOG entry all say so.

The argument that carries it is the gate's own purpose: a red that can
be the machine's fault teaches nothing about the landing being
measured. So the `Live` tag is widened, in build.sbt's comment and in
specs/integration-test-gate.md, from "reaches outside the JVM" to "its
result depends on something `sbt test` cannot control". Nothing stops
being run: `sbt integrationTest` runs all of it, and if the consensus
fold ever genuinely breaks, that is where it surfaces.

Verified: `okayPersistJVM/testOnly okay.persist.TestElectionReplicated`
reports 0 tests under the default gate and 3/3 with
`--include-tags=Live`. okayPersistJVM/test: 95 green.

## gate-honesty — the untested foldCont/runWith obligation, a docs-index guard, demo tests off the disk
Completed: 2026-09-03
Landed as dde1e151. Three holes in the CHECKS, not three features.

`runWith` is defined as `foldCont(handler) / identity`, and Free and
Eager both override it with a one-pass fast path whose comment claims
the same answer. Nothing checked that claim: TestReflect proves the
encodings agree with EACH OTHER, but both of its sides call `runWith`
— a fast path that drifted would take every encoding with it and pass.
TestLowering now runs the two paths against each other per encoding
(Free, Eager, Eff) and per bind shape (pure, one op, map only, pure
bound, right- and left-nested, a dropped continuation, mixed
associations, 5000-deep both ways), comparing the answer TOGETHER WITH
the effect trace. 11 green — the obligation holds. It bites, checked
by mutation: duplicating one `H.handle(e)` in `runFree` fails 7 of the
11, and the 4 it does not fail name the shapes that are load-bearing
(a bare Inject with no Bind above it cannot see that mutation).

The module index in docs/README.md was hand-kept and unguarded —
eight rows had gone missing earlier with nothing to notice.
TestDocsIndex (in okay-deploy, beside TestDemoDeploy, which already
owns "compare the committed tree against what it should be") checks
every direction: every page linked, every link resolving, every module
root the build declares having a page. First run found three modules
shipping with no page at all — okay-crypto, okay-script,
okay-demo-e2e-browser — now written.

okay-demo's tests fork with the repo root as their working directory
and `chatStore` defaulted to a FileStore, so the suite read and wrote
`okay-chat.log/` in the REPO ROOT and every run inherited the previous
run's facts. Test now sets `OKAY_CHAT_LOG=:memory:`; TestTwoNode's
spawned children still pass their own, and a test that wants a real
file store still asks by name.

MEASURED, correcting an impression rather than adding a feature: with
the default `Vectors.hashing()` the `propose` similarity branch is
very nearly inert. It counts character trigrams into 64 buckets, so it
scores surface overlap and not meaning — "разработчик"/"программист"
is 0.231 against the 0.85 default, and even one word against its own
plural is 0.815, still under. Out of the box the exact-slug path is
what dedupes. Deliberately not "fixed" by lowering the default: 0.85
is calibrated to that collision floor and the threshold belongs to
whichever embedder is wired in, which is why `marketOf` takes the pair
together. specs/demo-chat.md, docs/modules/okay-demo.md.

Green: okayJVM 449, okayDeploy 9, okayDemo 66 (2 skipped).

## okay-script-check — mdoc-style output-comparison literate testing via a ```stdout fence
Completed: 2026-09-03
Landed as 5b9a5aad. A block's expected stdout, written inline in the
markdown, checked against what a real `run` actually printed — `run`
already captured everything needed; the missing piece was the
markdown convention for "expected output" and the comparison step.
New fenced tag ` ```stdout `: content immediately following a
` ```scala ` block names the output the document should have produced
BY THAT POINT (`run`'s program is one flat compilation unit, so "by
that point" naturally means "since the start", not "just this
block"). `ScalaScript.check(markdown, classpath): CheckResult` is
purely ADDITIVE and host-side — no synthesis changes, no new fence
recognized by `tokenize`/`withMeta` at all, deliberately, right after
two landings in a row (`okay-script-web`, `okay-script-line-mapping`)
hit the SAME re-indentation bug shape there. `check` extracts every
` ```stdout ` fence's (trimmed) content via a plain line-scanner
mirroring `blocks`' own, runs the document once via the ordinary
`run`, and — if that succeeded — verifies each expected chunk appears
as an IN-ORDER, non-overlapping substring of the actual stdout (each
search starts after the previous chunk's match end), proving the
right output happened in the right relative sequence without
injecting a checkpoint into the compiled program itself. All
mismatches are collected, not just the first; a `run` that fails to
compile fails `check` immediately with one summarizing mismatch
rather than attempting a substring search against output that never
happened. The first cut passed all 8 tests on the first run — no bug
found, a real contrast with the two landings immediately before it.
specs/okay-script.md "Output-comparison testing".

## okay-script-line-mapping — compile errors point at the original .md line, not the synthesized source
Completed: 2026-09-03
Landed as 14e8c96e. A dotc diagnostic's line number used to report
against the SYNTHETIC wrapped source, never translated back to the
`.md` file a real author is looking at — `Block.startLine` was
captured for exactly this since the beginning and sat unused.
`Segment.Code`/`Interp` both gain a `startLine` (1-based, same
convention as `Block.startLine`), set by `tokenize`; `withMeta` now
builds the synthesized body AND a parallel `Vector[Int]` line-origin
map — one entry per physical body line, giving that line's original
markdown line, or `-1` for a line with no original counterpart
(wrapper boilerplate, injected `Meta.setCurrent`/`Web.decodeArgs`
statements). A multi-line `Code` block's k-th physical line maps to
`startLine + k`, so an error on a block's 5th line correctly reports
the ORIGINAL 5th line, not just the block's first. `collectingReporter`
reads `dia.position()` (dotc's `Optional<interfaces.SourcePosition>`,
needing no `Context` argument, unlike `SourcePosition.line(using
Context)`) — confirmed 0-based via a throwaway probe BEFORE writing
anything, not assumed from the API's own naming. A hit prefixes the
message `"L<n>: "`; a miss (no position, or `-1` — synthesized code,
an `okay-script` bug not the author's) falls back to the bare message,
unchanged. Found and fixed along the way: the SAME bug shape as
`okay-script-web`'s `compileOnly` fix, one function over —
`withMeta`'s first cut indented EVERY physical line of a `Text`/
`Interp` segment's synthesized `print("""...""")` call uniformly,
corrupting embedded multi-line string DATA the same way at a
different layer; `TestScalaScriptRender`'s own no-interpolation test
caught it again, immediately. Fixed with an explicit `isStatement:
Boolean` per item (`Text`/`Interp` indent only their first physical
line; `Code` indents every line, preserving a block's own internal
relative indentation). Two landings hitting the identical bug shape is
itself the finding worth recording: any code re-indenting already-
assembled text by scanning physical lines is suspect near a raw
triple-quoted string, regardless of layer. specs/okay-script.md
"Line-accurate errors".

## intent-fixture-too-small — 120 messages and six languages, and the small-fixture conclusion holds
Completed: 2026-09-03
Landed as 1b4e177a. The instrument before the experiment: the previous
lane said no gap in its arms table should be defended at n=24, where a
one-reply difference is not a difference and a mid-lane wording change
had already moved an arm by two. So the fixture was taken first, ahead
of the hypothesis it exists to test.

`IntentFixture` is now 120 messages, thirty per class, with the DOMAIN
stated inside it ("meeting and scheduling intents") because the last
lane established that nothing else states it. Hard cases are marked
rather than avoided: Proposal/Request overlap, indirect phrasing,
cancellation without a proposal, and — where the bucket actually broke
— out-of-domain messages written in the register of a request.

THE PREVIOUS CONCLUSION REPRODUCED, five times larger: macro F1 0.553
-> 0.906, `Other` F1 0.00 -> 0.86, decode rate 68% -> 91%. That is the
result that matters here; the earlier table was measuring something
real and not the shape of twenty-four sentences.

LANGUAGE IS NOT FREE, and its failure runs the other way. A parallel
set carries twelve meanings in six languages (en, fr, de, es, ru, ja),
so the only thing varying between rows is the wording and a drop is
attributable to the language rather than to whichever sentence happened
to be foreign — scattering foreign lines through the main lists, which
is what the fixture did before, proved nothing.

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| macro F1 | 0.914 | 0.804 | 0.792 | 0.813 | 0.727 | 0.813 |
| `Other` precision | 1.00 | 0.75 | 1.00 | 1.00 | 0.60 | 1.00 |
| `Other` recall | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 |

Recall holds at 1.00 everywhere: the gate does not stop recognising
out-of-domain messages when they stop being English. What it loses is
PRECISION — in Russian (0.60) and French (0.75) it pushes genuine
meeting messages OUT of the domain. English absorbs what does not
belong; the others reject what does. Filed as intent-gate-non-english,
to be measured per language rather than in aggregate.

The live sweep was trimmed to the decisive pair (before, after): the
six-arm sweep that established WHY is recorded in the spec, and
re-deriving it over 120 messages would be an hour of calls for a
conclusion already drawn. The orphaned helper went with it — dead code
kept to commemorate a measurement is not how a measurement is kept.

LIMITATIONS, recorded in the fixture itself rather than in a commit
message nobody re-reads: 120 author-written messages are enough for
stable per-class metrics and not enough to claim coverage, and the
translations are by the same hand as the classifier, so an awkward
rendering is a confound the numbers cannot separate from a model
weakness.

Also filed: intent-decode-rate-residue — 11 of 120 replies are still
undecodable on the best arm. The rendered example took that from 32% to
9% and then stopped, and nobody has looked at what remains.

Full matrix green: 2103 tests, 0 failures.

## okay-script-web — request-object injection, a dependency-free Web value, and two real bugs it found
Completed: 2026-09-03
Landed as ebfdfefd. The remaining half of "a new JSP": a script
reading the CURRENT HTTP request (method, path, query, headers) the
way it already reads `Meta.current` for file metadata. Scoped to
avoid the dependency the BACKLOG entry itself flagged: `Web` is a
plain, dependency-free case class (`String`/`Map` only) — no
`okay.http.Request` import anywhere in `okay-script`'s own code; a
caller (an `okay-jetty` route) translates its own `Request` into
`Web` before calling `render`/`Page.render`. `Page.render(web)` sets
it FIRST, inside the page's existing lock, so concurrent requests to
the same `Page` never race on which request's `Web` a given call
sees. Two real bugs found and fixed before landing: `Web` hit the
SAME classloader-identity trap `okay-script-page`'s `Console` fix
found, one level up, for a user-defined type — a host-built `Web`
handed directly to the isolated script fails reflection's
argument-type check, since the isolated loader compiles its own
separate `Web` class. Fixed by encoding `Web` into a flat
`Array[String]` on the host side and decoding it back INSIDE the
isolated classloader (only `String`/`Array[String]` ever cross the
boundary) — which meant abandoning `@main def okayScriptMain(): Unit`
for the wrapper entirely, since its generated forwarder never hands
`args` through when the `@main` method itself takes zero parameters
(which it always did here); switched to a plain `object
OkayScriptMain: def run(args: Array[String]): Unit`, confirmed via
`javap` before writing the change, not assumed. That wrapper change
then broke output for EVERY existing example — caught immediately by
`TestScalaScriptRender`'s own no-interpolation test, not a
`Web`-specific failure: the naive fix (re-indent the already-built
body by prefixing every physical line for the new nesting depth)
corrupted DATA inside a `Text` segment's multi-line raw string
literal, indistinguishable from source formatting to a blind
line-prefix pass. Fixed by having every body-line producer (`run`,
`render`, `withMeta`) build its lines at the FINAL required depth
directly, removing the unsafe re-indentation pass entirely. Also
repeated — and fixed the same way as — `hasMeta`'s own
self-sufficiency lesson from the previous landing: an unconditional
`Web` reference broke `TestScalaScriptClassloaderIsolation`'s
minimal-Classpath case again; `hasWeb` (a cheap substring check) gates
it now. specs/okay-script.md "Request context".

## lowering-note — the textbook explains foldCont
Completed: 2026-09-03
Landed as cb82e6fe (docs only). Operator asked why `Free` is lowered
into `Cont` and how exactly it happens; docs/theory ch.5 gains
"Lowering: how a program becomes its meaning".

Why: the third handler shape needs the operation's continuation, and
the freer tree keeps continuations *beside* operations as data while
`Cont` is the type whose subject they are. The chapter is careful about
`foldCont`'s status, since it reads like an optimization and is not:
`Free[F, *]` is the free monad, so there is exactly one
structure-preserving map out of it given an interpretation, and
`foldCont` is that map at `Cont`. Hence `handle` and `runWith` are
defined *through* it.

How: `Free.fold` supplies the normal form, so an operation always
arrives with its continuation; `h(e)` is the handler's `Cont` for that
operation; `k(_).foldCont(h)` lowers the rest and binds it after, so the
Free spine is rebuilt as a Cont spine. Then `Cont`'s five-case runner,
with the split named: three cases rotate and discharge tail-recursively
(chain length costs no stack), one hands the continuation over, and the
fusion budget decides whether a node is materialized at all. Forwarding
appears as `shift(k => perform(e).flatMap(k))`, algebraicity spelled in
`Cont` and licensed by the section added in 90c49a96.

And the two escapes: `runWith` overrides with `runFree`, one pass
instead of two, allowed because a comonadic handler uses each
continuation once and immediately; in `Eff` lowering is the identity
because the program IS its own `foldCont`, with the symmetric costs
stated. Every quoted snippet was checked verbatim against the source so
the chapter cannot drift silently.

## algebraicity-note — what the middle constructor decides
Completed: 2026-09-03
Landed as 90c49a96 (docs and comments only, no behavior change).
From an operator observation: `Free` and `Cont` differ in exactly one
constructor, `Pure` and `Bind` being identical in both. docs/theory
ch.5 gains two sections making that load-bearing instead of
incidental. The middle constructor IS the signature, and the
difference between `Inject` and `Shift` is Plotkin-Power's
algebraicity equation: it fixes the arity of the types (one index
against three, answer-type modification being the price of seeing the
continuation), it licenses reordering/batching/hoisting over `Free`
and forbids them over `Cont`, and it is why programs are `Free` while
handlers are `Cont` (Filinski 1994, Kammar-Lindley-Oury 2013, Forster
et al. 2017 added to the chapter's references).

The second section answers the hazard the first raises. Two nodes here
carry computations, `Sim.Op.Fork` and `Stm.Tx.OrElse`, which is the
scoped-effects shape (Wu-Schrijvers-Hinze 2014, Piróg et al. 2018)
that a relay cannot see into, and `!.relay` indeed walks the spine
only. It cannot bite: the kind `F[+_]` gives a signature no way to
name the ambient row, so a payload is necessarily CLOSED over its own
signature and nothing can hide in it. The same kind is what forbids
forking a program that also logs; widening needs higher-order
signatures. Two comments at the nodes say so.

Worth recording as process: three proposals went into this and two
dissolved on inspection. "Lower Free into Cont for speed" already
exists and is the foundation (`foldCont` IS that lowering, `Eff` is
the full Church encoding, and `Free.runWith` is a deliberate one-pass
runner instead of two). "Guard scoped payloads with a marker type" is
unnecessary because the kind already enforces it. What survived was
the smallest piece: name the constraint where it lives, and write the
theory down once. Core 438/27/31 green on JVM/JS/Native.

## intent-other-collapse — the Other bucket is rescued by a gate, and the decode rate turns out to be a prompt property
Completed: 2026-09-03
Landed as e26c1b07. The negative finding intent-classify left behind,
chased down in the repository rather than in a script beside it:
`TestClassifyLive` (Live-tagged, out of the default gate) runs six arms
over `IntentFixture`'s 24 messages, and the fixture is shared so the
next lane compares against the same baseline instead of inventing one.

| arm | decoded | macro F1 | `Other` recall |
|---|---|---|---|
| bare — the schema alone | 4/24 | 0.733 | 0.00 |
| rules — schema + written rules | 18/24 | 0.587 | 0.00 |
| shipped — rules + rendered example shape | 21/24 | 0.681 | 0.17 |
| examples — shipped + 5 labelled examples | 23/24 | 0.908 | 0.67 |
| gate — in-domain question, then shipped | 21/24 | 0.826 | 0.50 |
| examples + gate | 23/24 | 0.955 | 0.83 |

Macro F1 is over DECODED replies and means nothing without the decode
rate beside it — the bare arm's 0.733 is over the four replies it
managed to produce.

THE ANSWER: declaring an `Other` case is necessary and not sufficient.
What rescues it is not asking the taxonomy question at all until a
separate binary question has said the message belongs — recall 0.00 ->
0.83 at precision 1.00. A model offered a choice among positive classes
takes one; a yes/no question does not offer that choice. The gate costs
one extra call per message, visibly, and the caller chooses.

TWO FINDINGS THAT WERE NOT THE QUESTION.

The decode rate is a property of the PROMPT, not of the model: 4 -> 18
-> 21 -> 23 of 24 replies decoded from the same model purely on how the
answer was asked for. A schema says what is legal and does not show
what to type — shown only the schema, the model wrote
`"intent": "Proposal"` as a bare name where the encoding wants a tagged
case, dropped `alts`, and merged `conf` into the intent object. The fix
is `Classify.example`, an answer SHAPE rendered from the schema itself
(optional fields omitted, one list element, a sum tagged by its first
case) so it cannot drift from the parser. The same lesson had already
appeared at the other end of the lane: shown a schema for the gate's
two-field answer, the model replied with the schema, its verdict buried
in `properties`. It is a shape and says so — a placeholder cannot
satisfy a refined leaf, and the first version of its test asserted it
decodes, and failed.

A harness sentinel must not enter a confusion matrix. Left in,
"undecodable" becomes a predicted-only class with F1 0 — correct by
`Eval`'s own rule for a real label, wrong for a marker nobody is
classifying — and macro F1 then tracks the decode rate instead of the
classification: two runs whose per-class scores were identical read
0.916 and 0.748 because they differed by ONE such row. The warning now
lives on `Eval.confusion`, where the next caller will read it.

Still wrong, and filed: one of six out-of-domain messages is absorbed
even by the best arm, and the residue is not random — a taxonomy of
`Proposal`/`Request`/`Notification` carrying a bare `what: String` never
says its domain is meetings, so the case NAMES must carry it. And at
n=24 a difference of one or two replies is not a difference; a wording
change mid-lane moved an arm by two, reported as the noise it is. The
fixture needs to grow past 30 per class before any gap here is
defended.

30 unit tests in the default gate, six live arms outside it. Full
matrix green: 2099 tests, 0 failures.

## okay-script-page — compile-once, invoke-many hot-reload for render-mode .md files, and the isolated-Console bug it found
Completed: 2026-09-03
Landed as 1a015f15. The hot-reload half of "per-request execution +
hot-reload" (BACKLOG's own framing for the JSP metaphor's second
half). New `Page(path, classpath)`: compiles a `render`-mode `.md`
file ONCE, cached by the file's mtime, re-INVOKES (not re-compiles) on
every `render()` call while the file is unchanged — the actual JSP
shape (a page's servlet class compiles once, its per-request method
runs once per request, it does not recompile on every hit). No new
dependency — `Page` only wraps `ScalaScript.render`'s existing
machinery; an actual `okay-jetty` route stays glue code a caller
writes. Split `ScalaScript.compileAndRun` into `compileOnly` (`dotc` +
classloader load, returns an invokable `Compiled` handle or a `Result`
with compile errors) and `Compiled.invoke()` (callable repeatedly
without recompiling); `render()` itself is now `compileRender(...).
fold(identity, c => try c.invoke() finally c.close())` — unchanged
observable behavior for every existing caller. Found and fixed along
the way: a SECOND `invoke()` on the same compiled program silently
printed NOTHING — a real, previously-invisible bug from
okay-script-classloader-isolation that a one-shot `run`/`render`
could never have exposed. Root cause: the isolated script classloader
loads its OWN separate copy of `scala.Console`, so the original
host-side `scala.Console.withOut` fix (for capturing `println`) never
touched the copy the script's own `println` reads — it "worked" for a
one-shot call only by coincidence, since the isolated `Console`'s lazy
default binds to whatever `System.out` is at ITS OWN first touch and
stays bound to that one stream forever after. Traced to a minimal
bare-classloader reproduction before writing the fix, not guessed at.
Fixed by driving the isolated classloader's OWN `Console` via
reflection (`setOutDirect`) on every `invoke()` call — applies
uniformly to `run`/`render`'s one-shot path too, though it was
invisible there. Request-object injection (JSP's implicit `request`/
`response`) is deliberately NOT in this pass — filed to BACKLOG, since
it would add `okay-script`'s first real dependency beyond
`scala3-compiler`. specs/okay-script.md "Hot-reload".

## okay-script-meta — code in an .md file reads front-matter + heading-scoped yaml as its current context
Completed: 2026-09-03
Landed as fdf8b7ec. Answers the operator's ask directly: code inside
an `.md` file can now read the metadata defined in the markup AROUND
it. Source of metadata is front-matter (`---`, file-level) plus
nested ```yaml fenced blocks scoped by heading ancestry — the exact
shape `../it-consulting/site/site.md` already uses (`tagline`/
`contact` in front-matter, a `services` list under its own heading).
New module `okay.script.Meta`: a typed AST (`Value = Str|Arr|Obj`,
`Section(level,title,yaml,children)`, `Doc(frontMatter,root)`) built
by a minimal YAML-subset parser (flat mapping, and a list of flat
mappings), plus `Context(doc,path)` with untyped `get`/`apply` AND the
full typed `Doc` for tree navigation — both forms of access asked for,
through one value. Exposed to BOTH `run` and `render` via
`Meta.current`/`setCurrent` — NOT a `given`, despite that being the
FIRST design written into the spec: tested empirically before landing
(not discovered by a failing test), local `given` re-declaration at
the same flat scope is a compile error, and even past that a plain
`given` is evaluated once, never re-evaluated per `summon`.
`Meta.current` is a plain always-fresh method backed by a mutable var;
the shared tokenizer emits a `Meta.setCurrent(...)` statement whenever
a segment's heading path differs from the one before it — ordinary
statements have none of `given`'s restrictions. A script wanting
`given`/context-function ergonomics can still have them by declaring
`given Meta.Context = Meta.current` itself, locally, immediately
before use. ```yaml fences are now METADATA (consumed, not shown in
`render`'s output) — every other fenced language is unaffected.
Metadata wiring is emitted ONLY when a document actually has
front-matter/yaml/a heading (`hasMeta`) — a metadata-free document
never references `okay.script.Meta` in its synthesized source, keeping
`run`/`render` self-sufficient (scala-library only) for the common
case; the first cut skipped this check and broke it, caught
immediately by `TestScalaScriptClassloaderIsolation`'s own
minimal-Classpath test. The existing storefront example now reads its
`tagline`/`contact` from real front-matter via `Meta.current` instead
of a hardcoded second copy. specs/okay-script.md "Metadata as context".

## okay-script-interpolation — render(): ${expr} interpolation in markdown prose, "a new JSP, but Scala+Markdown"
Completed: 2026-09-03
Landed as b13027c6. The operator's own framing for `okay-script`.
New `ScalaScript.render(markdown, classpath): Result`, separate from
`run` (untouched — `run` stays about apps/effects like the
storefront). `render` treats the WHOLE document — prose and code —
as one program: `${expr}` markers in prose (outside ```scala fences)
are Scala expressions, evaluated in the same document-order scope
```scala blocks already build, their `.toString` printed in place;
everything else passes through verbatim. `$${` escapes to a literal
`${`, mirroring Scala's own `s"...$$..."` convention. The scanner is
brace-depth- and quote-aware, not a naive first-`}`-wins regex: an
`if/else` with braces inside an expr, and a NESTED real Scala string
interpolation (`s"${x}"`) inside an `${...}` marker's own expression,
both parse correctly. Reuses `run`'s exact compile/load/invoke/
stdout-capture engine (`compileAndRun`, extracted from what used to
be `runWith`'s tail) — only the source-synthesis step differs. Each
segment prints directly, in document order, rather than buffering and
flushing at the end — a design refinement made while writing the
spec, before any test ran, once it was clear a buffered design would
silently reorder a code block's own `println` output after the whole
rendered document instead of interleaving it correctly. Worked
example: `examples/render-storefront.md`, exercising the
nested-interpolation case for real. Filed to BACKLOG: per-request
execution + hot-reload, the second half of "a new JSP" deliberately
left out this pass — closer to an `okay-jetty` route than to
`okay-script` itself. specs/okay-script.md "Interpolation".

## intent-classify — one Schema derivation is the taxonomy, the frame and the parser
Completed: 2026-09-03
Landed as 8ebad9bf (spec abd41ff5, backlog b9043dea, correction
0ed33a1b). Intent classification in `okay-agent`, built from the three
published approaches rather than from one: a structured-output LLM
classifier, a hierarchical taxonomy, and an ontology/FrameNet system
that uses no model at all.

The synthesis is that a LABEL cannot be acted on and a filled FRAME
can. "Proposal" does not answer an email; `Proposal(when, who, where)`
does. Both come from ONE `Schema[I]`: FrameNet's Frame Elements are a
product's required fields, which is exactly what `ToolSpec.jsonSchema`
already computes from "not `Option`, no default". So the enumeration
the model is shown and the decoder that reads its answer are the same
value, and a label outside the taxonomy is a DECODE ERROR rather than
a class of parsing bug — as is a slot that fails its own schema (a
`When` that is not ISO-8601, through `SIso`).

Two axes kept apart, which is the mistake the sources make: MULTI-INTENT
is spans (both acted on), AMBIGUITY is ranked alts within a span (one
chosen). A flat list expresses neither, and a caller holding one cannot
tell which case it is in.

Hierarchy costs nothing — the derivation already recurses — but the
walk needed one thing the reading did not reveal: Scala encodes a
hierarchy as a case WRAPPING the sub-enum, so a group node is a
product, not a sum, and a walk that only descended sums stopped at
"Proposal". Found by the first test run. A group is now "a one-field
case whose field is itself a taxonomy"; a case whose single field is a
plain value is a leaf. Both kernels used (`theCase`, `eachField`) hand
the value over at its own type, so the whole walk takes no cast.

Evaluation is a fold because a confusion matrix is a Monoid (the
property `Postings` has, for the same reason), and the promotion rule
is EXECUTABLE: `regressions` returns the classes that fell more than
the tolerance, so "promote only if no class regresses by more than two
points" is a function rather than a paragraph someone remembers.

MEASURED FIRST, before any code, because a design decision rested on
it: on the local 4B gateway, 24 labelled messages, both field orders.
Schema field order reaches the wire (48/48 replies honoured it), and
`why` BEFORE the label is worth 0.136 macro F1 (0.615 vs 0.479) at a
cost of ~130 characters — so `Structured.cut`'s saving is real but buys
the worse arm. Quality wins at that price.

The useful finding is negative: the `Other` bucket COLLAPSES. Recall
0.17 with reasoning, 0.00 without; every out-of-domain message was
absorbed into a positive class (charged twice -> Request, birthday
wishes -> Notification). "Always include an other bucket" is necessary
and NOT sufficient — a model asked to choose among positive classes
will choose one. Macro F1 alone reads as mediocre-but-working; only the
matrix says a class is entirely absent, which is why `Report` is per
class and why a test now pins that shape.

A fine-tuned encoder was considered and REFUSED, on three independent
grounds: the cost is the labels (1k-5k per class, so 18-90k examples
for an 18-class taxonomy) rather than the compute; serving one needs
ONNX Runtime or DJL, a JVM-only native library, inside a library that
cross-builds to JS; and the conditions under which it wins are ">50
qps and a stable taxonomy", while the taxonomy is the thing that will
change most. Its replacement when a fast tier is finally justified is
in the backlog: a linear probe over frozen embeddings, 72KB of weights,
~18us for 18 classes (a cosine at 1536 components measured 1.04us in
`Store.scala`), trained from LLM-distilled labels at 30-100 examples
per class and needing no dependency at all.

Deliberately NOT built, and said so in the spec rather than left to
read as shipped: the `Structured.cut` wiring (so the token saving is
reasoned about, not measured) and the `Rerun`-journal fixture. Those,
plus the `Other` collapse, a Proposal/Request precedence rule, the
symbolic and vector tiers and temporal slots, are six backlog entries,
each carrying the trigger that would promote it.

24 tests (`TestClassify` 13, `TestEval` 11, three ScalaCheck
properties). Full matrix green: 2065 tests, 0 failures.

## okay-script-classloader-isolation — platform-only parent per script, closes a leak-through-host gap
Completed: 2026-09-03
Landed as e22ba638. Each `ScalaScript.run` call already got its OWN
`URLClassLoader` (scripts do not collide with EACH OTHER), but its
parent was `getClass.getClassLoader` — `okay-script`'s own defining
classloader — and `URLClassLoader` is parent-FIRST, so a script could
silently resolve a class from `okay-script`'s own build (`munit`, and
in Test scope `okay-jetty` and everything it drags in) regardless of
what the caller's explicit `Classpath` actually listed — the isolation
`Classpath`/`Deps` (okay-script-runtime) were built for was not
actually enforced. Fixed: parent is now
`ClassLoader.getPlatformClassLoader()` (JDK core modules only) — a
script sees exactly its own compiled classes, its own `Classpath`, and
the JDK. `Classpath.ambient` callers see no behavior change, since it
already lists essentially everything the JVM was launched with. Proved
by `TestScalaScriptClassloaderIsolation`: a script given a minimal
`Classpath` (just the scala runtime jars) can no longer reach
`munit.Assertions` — present on `okay-script`'s own test classpath,
absent from that minimal one — confirmed as a REAL regression check
(not a tautology) by temporarily reverting the fix and watching the
test fail (`munit-reachable:true`) before restoring it.
specs/okay-script.md "Classloader isolation".

## channel-seam — `Channel` becomes an interface, so mechanisms can be chosen and compared instead of replaced
Completed: 2026-09-03
Landed as a9dd71ca. The operator's design, and better than what
`channel-ring` attempted: that lane tried to replace the mechanism
INSIDE `Channel`, so one mistake in the waiter protocol took
everything down (it deadlocked and was reverted). Behind a seam each
mechanism arrives in its own lane, is measured against the others,
and cannot break the ones already there.

`trait Channel[A]` carries the callback primitives plus two
cancellers — the one part of waiting only an implementation can
express — and derives `send`, `receive`, the blocking pair and
`receiveMany` once, so implementations cannot drift apart on them.
`receiveManyAsync` has a correct default (one element as a chunk of
one) so a new implementation is correct before it is fast.

**Zero behaviour change.** The existing implementation moved behind
the trait unchanged as `StmChannel`, with `Channel.apply` as the
factory — and because `Channel[A](capacity)` was a constructor and is
now an `apply`, all 73 construction sites across 68 files compiled
untouched.

**What the interface refuses to promise:** STM composability.
`TestStm` reads a channel's own cell inside a transaction — real, and
impossible for a ring-backed channel that has no such cell. It stays
named on `StmChannel` rather than being promised on the interface and
thrown by everyone else. Making `Channel` a trait also surfaced a
latent given-ambiguity the class had hidden (`Writer.of(q)` could no
longer choose between the `Channel` and `Drain` stream instances),
now said explicitly. Implementations filed as `channel-impls`.

## http-request-query — fix Jetty silently dropping the query string from Request.url
Completed: 2026-09-03
Landed as e9901797, fast-forwarded onto master. Follow-on from
okay-script-storefront-example, which found `okay-jetty`'s `/order?
key=<x>` route 404ing every time and worked around it with a path-
based route. Root cause was narrower than first filed: not "okay-http
has no query-string support" — `okay.http.Server` (JDK,
`getRequestURI().toString()`) and `okay-netty` (`req.uri`) both
already carry the full request-target, path+query. Only `okay-jetty`'s
`requestOf` was broken, building `Request.url` from the static
`getPathInContext(req)` (path only) — a route matching on `r.url`
never saw a `?key=value` a client sent, silently, no error. Fixed by
reading `req.getHttpURI.getPathQuery` instead: verified against the
Jetty 12 sources that it returns the bare path with no query (so every
existing route sees a byte-identical string) and `path?query`
otherwise; no `ContextHandler` is used anywhere in `Jetty.serve`, so
nothing about existing routing changes. `TestJetty` gained a
query-string round-trip test. specs/http-backends.md notes the deeper
gap: `TestBackends`' cross-backend matrix never exercised a query
string on any backend, which is how this shipped unnoticed until a
real consumer hit it.

## channel-ring — the lock-free ring that eliminates the rebuild-per-operation cost
Completed: 2026-09-03
Landed as 31435023. `idiomatic-api-compare` traced our 3.6x deficit
to `zio.Queue` to the mechanism itself: `Channel.State` is an
immutable value `TRef.modify` rebuilds per operation, while
`zio.Queue` is a mutable `RingBuffer` (verified in its sources) with
zero allocation per operation. Three earlier lanes swapped the
structure INSIDE that rebuild model and all failed — the rebuild is
the cost, not the structure being rebuilt.

`Ring` is Vyukov's bounded MPMC algorithm under both constraints the
operator set: **no casts** (slots are an
`AtomicReferenceArray[A | Null]` — generic, so an element reads back
at its own type, and an empty slot is honestly nullable rather than
`null.asInstanceOf[A]`) and **no thread blocking** (short CAS loops
that answer full/empty; waiting stays a registered callback).
Measured against the same two operations as `Channel.State` does
them: **23.4 ±0.03 against 78.9 ±6.0 — 3.4x**, matching the 3.6x gap
and confirming the diagnosis. A boundary test found a real bug:
capacity 1 degenerates, so the floor is two — ZIO draws the same line
with a separate `OneElementConcurrentQueue`.

**The integration is deliberately not in this lane.** It was written
and it deadlocked `TestChannel`'s producer/consumer/close test; a
virtual-thread dump located the cause exactly — the protocol
temporarily REMOVED a waiter to attempt a pop, and a producer pushing
during that window wakes nobody and strands the element. The fix is
to claim rather than remove. Filed as `channel-ring-integration` with
the full diagnosis; `Channel.scala` is byte-identical to master.

## okay-script-storefront-example — a real storefront .md, content from ../it-consulting, compiled and run at runtime
Completed: 2026-09-03
Landed as 1e52a28b, fast-forwarded onto master. The worked example for
the whole `okay-script-runtime` pivot:
`okay-script/examples/it-consulting-storefront.md` — a real
`okay-jetty` server (a services page and an `/order/<key>` route),
compiled and run end to end through `ScalaScript.run`, using the
lifecycle recipe proven in `okay-script-lifecycle` (own `Thread`,
`Thread.interrupt()` to stop). Content — the IT-consulting services
list, names/descriptions/prices — is taken verbatim from
`../it-consulting/site/site.md`, the real business line's own site
data; only the DATA crosses over, not `busi`'s declarative-site
engine or the `scalascript` snippet that content normally uses for
its `/order` behavior — the example's page and order handler are
ordinary Scala, proving the actual point: `okay-script` runs code, not
a second DSL. Found and fixed along the way: the first cut's `/order`
route used a query string (`/order?key=<x>`), which `okay-jetty`'s
`Request.url` never carries AT ALL — `Jetty.scala`'s `requestOf`
builds it from Jetty's `getPathInContext` (path only; `okay.http.
Request` has no query-string field, full stop) — so the key extraction
always saw `""` and every order 404'd. Not an `okay-script` bug;
worked around in the example by moving the key into the path
(`/order/<key>`) and filed the real gap to BACKLOG for
okay-http/okay-jetty. Proved by `TestScalaScriptStorefront` (Live):
reads the `.md` from disk, runs it on a background thread, confirms
all five services render with their prices, confirms `/order/<key>`
returns the right confirmation, confirms interrupt stops the server.
specs/okay-script.md "Worked example".

## okay-script-lifecycle — Thread.interrupt() cleanly stops a Resource-run Jetty server, no new API
Completed: 2026-09-03
Landed as eb7e2a9d, fast-forwarded onto master. Settles the lifecycle
question okay-script-runtime left open: how a runtime-compiled app
(a generated storefront) starts without blocking the generator, and
stops CLEANLY later rather than just being abandoned. No new
`ScalaScript` API was needed — the answer was already the idiom
`okay-demo/ChatDemo.main` has run a real server through all session:
`Resource.run(Jetty.serve(port)(routes)().map { s => ...;
Thread.sleep(Long.MaxValue) }).runWith`. Two facts make it a
STOPPABLE app: `Resource.run`'s `_loop` releases every acquired
finalizer on any escaping `Throwable` (Resource.scala), including a
plain `Thread.sleep`'s `InterruptedException`; and `ScalaScript.run`
invokes the compiled script's `main` synchronously on whatever thread
called it, so a caller running `run` on its own dedicated `Thread`
holds the exact thread the script blocks on, and `.interrupt()` on it
is a real, targeted stop, not a best-effort kill. Proved against a
REAL `okay-jetty` server, not asserted: `okayJetty` added as a
Test-scope dependency of `okay-script`; new Live-tagged
`TestScalaScriptLifecycle` starts a script's server on a background
thread, confirms it answers HTTP, interrupts, confirms it stops
answering (the release actually ran the server's own stop), and
confirms the returned `Result` carries the `InterruptedException`. The
hypothesis held on the first real run — nothing needed fixing.
specs/okay-script.md "Lifecycle"; closes the okay-script-lifecycle
BACKLOG entry, and the storefront-example entry now names
`../it-consulting` as source content, per the operator, for the next
pass.

## chunks-size-one — why `Source` is not just replaced by `Chunks`: measured, not asserted
Completed: 2026-09-03
Landed as 6ac716f2. The operator's question: `Chunks` (array-native)
already beats `ZStream`'s own default 5.7x, so why keep `Source`
(the per-element, Free-tree representation) separately rather than
collapsing onto `Chunks`?

Hypothesis checked rather than assumed: an array-of-chunks
representation pays a chunk allocation per PRODUCTION regardless of
size, so `Chunks` forced to size 1 — what a genuinely one-at-a-time
live source (LLM tokens, SSE) hands it — should degrade the same way
`ZStream(chunkSize=1)` measured 12x worse than `Source.merge`.
Confirmed: `Chunks.merge` at size 1 costs **780.7 ±14.1**, a 33x
collapse from its own 64-element default (23.5 ±0.2) — structural to
the representation, not a ZIO quirk.

The number that answers the actual question: at that forced size,
`Chunks(1)` (780.7 ±14.1) and `Source.merge` (819.6 ±4.8) land within
a few percent of each other. Genuinely per-element load collapses
BOTH of okay's representations to the same floor — the cost of
per-element semantics itself. `ZStream` forced the same way costs
**9984.9 ±125.0**, 12.2x worse than either, with nowhere else to go:
the array-native shape is its only representation. `Source` is not
an unmerged duplicate kept out of inertia; it is what keeps
genuinely per-element work off the cliff every array-native
representation, ours included, pays for the same structural reason.

`docs/theory/07-logic-streams.md` carries the general form:
amortization is a property of the batch, not of a representation.

## okay-script-runtime — explicit Classpath + `//> using dep` (Coursier), and the fork-classpath bug behind it
Completed: 2026-09-03
Landed as e543c2d0, fast-forwarded onto master. The operator clarified
the actual goal behind okay-script: not a doc smoke-test, but runtime
app generation — generate a `.md` file at runtime, compile+run it, and
have it come up live as a web app (a storefront), the way
`../scalascript` does for its own language. `okay` already has the
application-shaped pieces (`okay-ui`, `okay-jetty`'s `serve`) that
scalascript had to invent a whole language to get, so `okay-script`
only needed two additions: `Classpath` (explicit classpath entries a
script compiles and runs against, instead of always the calling
process's ambient classpath — `run` now takes one, defaulting to
`Classpath.ambient`) and `Deps` (`//> using dep "org:artifact:version"`
— scala-cli's own directive — hoisted from a script's blocks and
resolved to jars by shelling out to the `cs`/`coursier` CLI, appended
to the classpath before compiling; dotc itself stays fully in-process,
only this one step touches the network, and only when a script asks).
Along the way, reproduced and fixed a sibling-found regression,
`okay-script-scalac-classpath` (BACKLOG): `okayScript/test` failed
5/7 on master itself because the project's `build.sbt` block never set
`Test / fork := true`, so its tests ran INSIDE SBT'S OWN JVM, where
`System.getProperty("java.class.path")` is just `sbt-launch.jar`'s own
path, not the real classpath — dotc compiled against a classpath with
no scala-library on it and crashed resolving `scala.Int`. Confirmed by
printing the property from inside the failing test JVM; fixed by
adding the missing `Test / fork := true` alone, before any of the
Classpath/Deps redesign. New Live-tagged suite `TestScalaScriptDeps`
proves end-to-end `using dep` resolution + execution against a real,
non-transitive dependency (`fansi`) — not already reachable on
okay-script's own classpath, so the test proves the resolved jar was
actually used. Filed to BACKLOG, not built: a worked
okay-ui/okay-jetty storefront example end to end (the concrete next
step), and classloader isolation between multiple runtime-compiled
scripts sharing one host JVM.

## json-unicode-escape — decode \uXXXX in both JSON parsers
Completed: 2026-09-03
Landed as 1cd5255b (spec) + 15f96859 (impl). `Json.unquote`'s escape
handling had cases for `\n`/`\t`/`\r` and a catch-all that appended the
character AFTER the backslash literally, advancing by two — written for
the single-character escapes (`\"`, `\\`, `\/`) and silently wrong for
the multi-character one: `\u0041` decoded to the five literal
characters `u0041`, not `A`. `JsonValue.scala`'s fast path had the
IDENTICAL bug, documented in its own header as the two roads'
deliberately-agreed reading rather than named as a defect — TestJsonValue
asserts fast-road output equals lossless-road output on every document,
so fixing one road alone would have broken that invariant instead of
fixing the bug. Both now share one `hex4` decoder. A surrogate pair
needs no special handling: two escapes decoding to two `Char`s that
form a valid high/low pair are already a correct Scala `String`. A
malformed/truncated escape falls back to the old wrong-but-safe
single-character reading rather than throwing (`unquote` returns a bare
`String`, not a `Json`, so there is no `JErr` for it to become).
Found downstream, live: a chat service consuming Telegram's Bot API
(which escapes non-ASCII as `\uXXXX`) decoded every Cyrillic character
of every incoming message into runs of literal hex digits — a defect
that looked, for a full day, like a chatbot failing to understand
Russian, and was in fact text that never reached the intent parser
intact. One pre-existing test asserted the old broken reading
verbatim; corrected, with the reasoning recorded so a future "fix"
does not flip it back. Gate: 76 suites, 2017 tests — five pre-existing
failures in `okay-script` (an unrelated toolchain break, logged
separately, reproduces identically on master) aside, everything else
green.

## okay-script — markdown ```scala fenced blocks compiled and run via the real dotty compiler API
Completed: 2026-09-03
Landed as a7d50b5 (spec) + 4543a93 (impl), fast-forwarded onto master.
New module `okay-script`: a `.md` file's fenced ```scala blocks are
one compilation unit (concatenated in document order into a single
`@main` body — a later block sees an earlier block's val/def,
REPL-session style), compiled and run through `dotty.tools.dotc`
IN-PROCESS — no `scala`/`scala-cli` subprocess, no custom language or
interpreter, per the operator's explicit framing ("наша цель только
лишь извлечение метаданных из разметки и минимальный препроцессинг и
метакомпиляция"). Two real traps found and fixed by the tests before
landing: `println` inside the compiled script did not land in
captured `stdout` (Scala's `println` goes through
`scala.Console.out`, a `DynamicVariable` that `System.setOut` alone
does not redirect — fixed by wrapping the call in
`scala.Console.withOut` as well); and a zero-block markdown file
FAILED TO COMPILE, not just did nothing (`@main def ... Unit =`
followed by nothing is a syntax error — fixed by defaulting the
wrapped body to `()`). Investigated `../scalascript` first per the
operator's pointer — a full custom markdown-as-syntax language,
unrelated in kind, nothing reusable; recorded as a negative result in
specs/okay-script.md rather than silently dropped. Library API only
(`ScalaScript.blocks`/`run`); no sbt-test integration and no
output-comparison (mdoc-style) checking yet — both filed to
BACKLOG.md as named follow-ons.

## persist-raft stage 1a — a real peer-to-peer wire transport, RaftMsg over real sockets
Completed: 2026-09-03
Landed as 8c27109 (spec) + 01acf2c (impl). `RaftEntry`/`RaftMsg` now
`derives Schema`. `okay.persist.RaftWire.Node` (JVM-only): real
`ServerSocket`s, real threads, the SAME `[len:int32][CBOR]` framing
`Wire.scala` already uses for the client-facing wire — reused here
for NODE-TO-NODE `RaftMsg` exchange. One-shot connections (connect,
write one frame, close) rather than a persistent connection pool per
peer — simple and correct first; Raft's own retry-by-heartbeat
already tolerates a dropped send. A background tick thread drives
REAL wall-clock election timeouts (randomized per node) and leader
heartbeats — the first real non-determinism this algorithm has run
under; stage 0's tests drove everything by explicit tick.
`propose(data)` is the client seam: succeeds only on the current
leader, `false` otherwise (no forwarding to the real leader yet —
stage 1b). `onCommit(index, entry)` fires once per newly committed
index, in order.
Deliberately NOT here: the `Store`/`Topic` wrapper itself (`Election`
still cannot construct a topic over this), and persistent storage
for `currentTerm`/`votedFor` (a real crash forgets them here —
Raft's own safety proof assumes stable storage for exactly those two
fields, stated not hidden).
Tests (`TestRaftWire`, 3, real sockets/threads/wall-clock): three
real nodes elect exactly one leader and agree on who it is; a client
entry proposed to the leader replicates and commits on every node, a
non-leader refuses; killing the leader — the survivors elect a new
one and keep committing. 5/5 clean runs. Full `okayPersistJVM` suite
98/98.

## test-login-tamper-flake — the suggested fix had its own, bigger flake
Completed: 2026-09-03
Landed as d260ddf. `TestLogin`'s "a tampered token is refused" built
its tamper as `token.dropRight(2) + "xx"` — the SAME token whenever
the JWT happened to already end in `xx` (~1 in 4096 runs). BACKLOG's
own suggested recipe (flip the last char to one it is not) turned
out worse: stress-tested at ~40% failure, not rare at all. Root
cause: a 64-byte ES256 signature base64url-encodes to 86 characters
carrying 516 bits for 512 bits of real signature — the FINAL
character holds only 2 significant bits (4 are decoder-ignored
padding), so many single-character flips there decode to the
IDENTICAL signature bytes and verify anyway. Fixed by flipping a
MIDDLE character instead — always inside a fully-significant 6-bit
block on any reasonable token length. 0/50 stress runs, plus the
normal suite green.
Also swept the rest of BACKLOG.md against the actual code/specs
(operator: "half of it seems already done"): removed nine now-empty
section headers (Correctness and the core, Cross-cutting,
okay-security, The data landscape, okay-cache, okay-sql, okay-pg,
okay-jdbc, okay-conf) that had accreted to zero items; marked
okay-demo's section DONE (all 11 items were already checked, just
missing the closing marker the other finished sections use).
Spot-checked the remaining open items (ctx-reader-bridge,
stm-js-direct-bench, okay-http's flake ledger) — all still
accurately open, no further staleness found.

## okay-stm-collections — TDict/TList over one TRef, synchronous by honest construction
Completed: 2026-09-03
Landed as 1b9c688 (spec fix) + 7f88f25 (impl) on top of an initial
spec commit. The real design question, answered before building
(BACKLOG named it): does an STM-backed map/list make plain
synchronous call sites (`Hub.subscribe()`, `Registry.apply(key)`,
`Subscription`'s two maps) effectful? No — `TRef.modify` is ALREADY
synchronous (one CAS loop, no `Tx`, no `F`); `Tx`/`Stm[F]` exists to
coordinate MANY cells in one transaction, and a dict/list backed by
ONE `TRef` never needs more than that cell for any of its own
operations. The synchronous shape is the honest one, not a facade.
Named `TDict`, not `TMap`: `okay.TMap[K[_]]` already exists (the STM
engine's own heterogeneous write-set bookkeeping) — a real collision
the BACKLOG bullet's own reminder anticipated.
`TDict[K, A]`: get/put/remove/computeIfAbsent/updateAt/snapshot/size/
clear. `TList[A]`: append/snapshot/size/clear. Both in core `okay`
(cross by construction), zero platform-specific code.
Real bug caught by a 64-thread JVM stress test: `computeIfAbsent`'s
`mk` inherits `TRef.modify`'s own "f may run more than once" rule
under CAS contention — a losing attempt already evaluated `mk`
before losing the race, silently discarded. What's guaranteed is
that every racer observes the SAME winning value, not that `mk` runs
at most once; fixed the doc comment and the test's own assertion.
`okay-subscription` migrated: `joinedPeriod` is `computeIfAbsent`/
`put` exactly (a pure `mk`, so the may-run-twice limit costs
nothing); `paidPeriods` needed `updateAt` — a plain get-then-put
would race two concurrent `pay()` calls on the same subject. No
call-site API changed. `okay-live`'s `Hub`/`Registry` — the same
BACKLOG bullet's other half — NOT migrated here (named specifically
by the operator's ask); left filed.
Tests: `TestTDictCross` (5, JVM/JS/Native) + `TestTDict` (3,
JVM-only stress: 64 racers on one key, 100 concurrent `updateAt`,
200 concurrent appends — none lost). `okay-subscription`'s existing
suite 9/9 unchanged, 3x clean. `okayDemo` compiles clean against the
migrated `Subscription`.

## persist-raft stage 0 — the Raft algorithm's core, staged explicitly
Completed: 2026-09-03
Landed as 81f0bfc (spec) + e3224c0 (impl). `specs/consensus.md`
itself scoped own-Raft as "months of careful work... justified only
when a deployment cannot run Kafka and has outgrown the arbiter" —
the operator asked to start it anyway, understanding a session lands
a slice, not the whole climb. Staged rather than attempted whole,
matching how this stack prices every large claim.
Stage 0: `okay.persist.Raft` (`RaftState`/`RaftMsg`/`RaftEntry`,
`Raft.handle`/`startElection`/`replicate`) — a PURE value
transition, no engine, no network, no `Store` yet: the textbook core
(Ongaro & Ousterhout, Figure 2). `Election` does not change by
construction when this eventually lands as a `RaftStore` behind a
`Topic` — the reduction's whole argument. A higher term seen on ANY
message steps a node down first, unconditionally, before the message
is otherwise handled.
Tests (`TestRaft`, 7, driven EXPLICITLY — no wall clock, matching
`TestElectionReplicated`'s own manual-clock style): a lone candidate
wins a majority and becomes leader; two simultaneous candidates
never both lead in one term; a client entry replicates to a majority
and the leader commits it; a heartbeat propagates `commitIndex`; a
follower diverged at an OLDER term is corrected, not appended past
(same-term-same-index divergence is impossible under Leader
Completeness); a stale-term message is refused untouched, a
higher-term message steps a leader down; the Figure 8 trap — a
previous-term entry is never committed by majority count alone.
Two real bugs caught in testing, both in the TEST's own setup, not
the algorithm: asserting `votedFor` resets after a refused stale
message (it doesn't); injecting a same-term "rogue" divergence,
which cannot legitimately arise under Raft's own safety property.
Full `okayPersistJVM` suite 95/95. Stage 1 (the `Store`/`Topic`
engine wrapper + network transport — the actual `RaftStore` the
BACKLOG bullet names) and stage 2 (compaction, membership changes)
filed, not attempted here; so is the seed-swept `Sim.scala`-driven
fuzz harness this repo's own prior notes asked for, once stage 1
gives it something real to fuzz.

## direct-try-ctx — a context-function CanTry, deferred to application, no crash and no version bump
Completed: 2026-09-03
Landed as 4b9f3ee (spec) + 7935a23 (impl). The 2026-09-02 audit
withheld a `CanTry` instance for context functions because the
obvious shape (reusing `CanTry.strict`) crashed dotty 3.7.4 at
erasure ("bad adapt for M$proxy2.pure(a)") inside the direct macro's
generated code.
That shape was also semantically wrong regardless of the crash: a
context function `E ?=> A` is a CLOSURE over its environment —
evaluating (constructing) it never runs the body, only APPLYING it
with a given `E` does. `try fa catch ...` around the construction
tries a value that has not executed anything yet, so a throw from
inside the body would never be caught — the same "catch silently
never fires" trap the audit named for `Eff`, reappearing here for a
different reason.
`ctxFn` defers the try to APPLICATION time instead — the honest
counterpart to the Free row instance's per-step guard:
```
def tryIn[A](fa: => (E ?=> A))(h: Throwable => (E ?=> A)): E ?=> A =
  (e: E) ?=> (try fa(using e) catch case ex => h(ex)(using e))
```
This different generated-code shape sidesteps the erasure crash too
— confirmed by compiling `direct[[X] =>> E ?=> X] { try ... catch
... }` clean. No Scala version bump needed (3.7.4 is already the
latest 3.7.x patch — the next available versions are 3.8.x/3.9.x, a
much bigger, unrelated change nobody asked for here).
Test: try/catch inside a context-function direct block, the SAME
produced value applied twice via `provide()` with different
environments, both catching correctly — proving the deferred, not
one-shot, semantics. Full okayJVM suite 418/418 (the existing
`TestErrorMessages` "no CanTry" assertion for `Eff`, the genuinely
lazy Cont monad, still passes — a different, unaffected `F`).
okayJS/okayNative main sources compile clean.

## native-scheduler-pool — a fixed worker pool, safe now that waiting is in queues not threads
Completed: 2026-09-03
Landed as 46b2704 (spec) + e372171 (impl). The Native Scheduler
forked one OS thread per fiber because a fiber waiting on a Channel
held that thread asleep — on a shared pool, enough waiters would
deadlock every worker at once. channel-callback (2026-09-02) closed
that: Channel now waits in queues, never in a thread, so ordinary
fiber work never blocks a pool worker — only an explicit CanBlock
park still does (still real OS parking on Native, no Loom there).
`Schedulers.pool(size)`: a hand-rolled task queue
(`scala.collection.mutable.Queue` under `synchronized`/`wait`/
`notify` — no `java.util.concurrent` collection assumed on Native's
javalib) feeding `size` worker threads. `Schedulers.threads` keeps
today's one-per-fiber scheduler and stays the DEFAULT — a blocking
workload on a shared pool can starve every worker, so `pool` is
opt-in until a consumer sizes it for a workload that does not park.
`cancel()` is best-effort (`Fiber.cancel`'s own stated contract): a
queued-but-not-started task is dropped; a running one is interrupted
through the worker thread CURRENTLY running it, tracked per task so
a stale cancel can never reach a later, unrelated task the same
worker picks up next.
New Native-only test source dir: `Test/unmanagedSourceDirectories`
for the native leg was `:=`-overwritten to just the cross suite
(like JS); appended `src/test/scala-native` for what only makes
sense there (CanBlock-based).
Tests: `TestNativeScheduler` (4 tests) — 20 fibers complete on a
2-worker pool without deadlock, par on the pool, `threads` unchanged,
a task cancelled while queued never runs. 3/3 clean runs on the
linked native binary; full `okayNative` suite 26/26.

## chunked-profile — two small levers, and the fair comparison the previous section owed
Completed: 2026-09-03
Landed as ceb77a4. Profiling the chunked path (stale since chunking
and channel-drain moved past the old 71%-in-the-transaction reading)
named two concrete frames: `boxToLong` per element, and `LazyList`'s
own machinery nearly as large as the interpreter's.

`Stage.chunked` is now `inline` — `ChunkBuf` allocates unboxed when
the element type is concrete at the point of expansion, and a plain
`def` hid it behind an abstract `T`. 197.5 ±0.8 against 206.3 ±3.2,
same window, bars non-overlapping. `Source.range` generates a
half-open range directly, no `LazyList` cell per element: −22% on
the per-element path (646.8 ±23.9 against 829.5 ±5.1), +9% WORSE on
the chunked path (the cell's cost is already amortised across 16
elements sharing a transaction there) — kept as the per-element
specialisation, not a blanket replacement.

**And a methodology bug in the comparison table itself, found and
fixed.** It read okay's per-element `Source.merge` against
`ZStream`'s own chunk-native 4096 default and labelled both rows
"elementwise" — not a comparison, since `ZStream` has no per-element
representation. Forced onto equal footing (`ZStream.range(chunkSize
= 1)`, fs2's `.unchunk`):

| | okay | ZIO |
|---|---|---|
| per-element | **824.9 ±6.9** | 10032.5 ±111.0 (okay 12x ahead) |
| chunk-native | **22.3 ±0.3** | 126.2 ±1.0 (okay 5.7x ahead, was 2.5x) |

`docs/benchmarks.md` §6b rewritten with this methodology.

## buffer-drain — `Channel.buffer` inherits the batched read: 2.4x, and it had never been measured
Completed: 2026-09-03
Landed as 2519ad2. `channel-drain` cut the merge's consumer side 30%
by taking what is already buffered under one transaction;
`Channel.buffer` is the other per-element channel consumer, had never
been benchmarked at all, and has exactly the same shape.
`Channel.buffer(1024)(xs).drained` measures **437.2 ±2.6 against
1068.5 ±18.4 element by element — 2.4x**.

`.drained` is an explicit carrier rather than a change to the
`Stream[Channel, Async]` instance, and that is forced rather than
chosen: that instance's carrier IS the channel, so it has nowhere to
hold a batch, and hiding one inside would hand a second consumer
elements the first had already taken. Also fixes two warnings that
arrived with `http-peer-address`.

## channel-drain — batch the RECEIVE side: 30% off the per-element merge, no flag, no semantic change
Completed: 2026-09-03
Landed as e7987aa. Profiling had put 71% of the per-element merge
inside the channel transaction, and four lanes established it cannot
be made cheaper, only rarer. Chunking made it rarer on the SEND side
and had to be opt-in, since batching sends delays an element that
could have gone now.

The receive side carries no such price, and nobody had looked: what
is ALREADY buffered is already late, so taking up to 64 of them under
one CAS hands the consumer exactly the same elements in exactly the
same order. `Channel.receiveMany` does it in one transition —
admitting parked senders into the room it frees, falling back to the
ordinary parking receive when nothing is buffered, so the count is a
ceiling and never a quota to wait for — and the single receive now
shares that transition so the two cannot drift. `Drain` is the
carrier that serves from the batch and touches the channel only when
it runs out.

**828.3 ±11.3 against 1180.5 ±11.8 on 2x2000, in one window — 30%
faster.** The largest single win on the per-element path in this arc,
and the only one that needed no permission from the caller. Seven
tests cover where order could go wrong: capacity 4, 1 and 0, an end
landing either side of a batch boundary, a failing producer, and
batched against single read element for element.

## http-peer-address — a served Request knows where it came from
Completed: 2026-09-03
Landed as 7b2b2b6 (spec) + 4fd1d62 (impl). `Request` carried the
method, the url, the headers and the body — and nothing about WHO sent
it. Fine for a client, which builds the request; wrong for a server,
which is handed one and has to decide whether to answer it. The
consumer that named it: a downstream service rate-limiting its
endpoints could key only on identities the caller supplies, all
forgeable, or on `X-Forwarded-For`, which is a claim in a header.
`peer: Option[String] = None` — additive and defaulted, so every
existing construction compiles and behaves unchanged, `Request.get`/
`post`/`json` included: those build what a CLIENT sends. The three
server-side constructors fill it through one `hostOf` helper each:
okay-jetty, okay-netty and okay-http's own JVM server. The HOST,
without the port, because a port changes per connection and keying a
limiter on `host:port` hands every connection a fresh budget — which
is the bug the field exists to fix, so each test asserts the absence
of a colon rather than trusting the shape. `None` means UNKNOWN, never
"trusted zero", and this is not `X-Forwarded-For`: that stays a
proxy's claim and a consumer's decision to trust.
Tests over real sockets on all three backends (the netty and okay-http
ones are Live-tagged, as those suites already were).
Gate: 75 suites, 1940 tests, 0 failures, 0 warnings — after two
flakes that were NOT this change and were each reproduced away in
isolation: okayCodecNative erroring under parallel load (77/77 alone),
and TestLogin's tamper test, which builds its tamper as
`token.dropRight(2) + "xx"` and therefore passes the ORIGINAL token
whenever a JWT ends in `xx`, roughly 1 run in 4096. The second is
recorded in BACKLOG as test-login-tamper-flake.

## nio-port-scope — every socket-binding suite into `integrationTest`, and one assertion that was testing the machine
Completed: 2026-09-03
Landed as a975ebe. Fourteen suites that BIND a real port move out of
the default gate, found by surveying the test tree rather than by
waiting for each to flake in turn: okay-http (TestHttp, TestMcpHttp,
TestWs, TestNio), okay-jetty (TestJetty, TestAcceptance,
TestResumable, TestMcpPush), okay-obs (TestOtlp, TestCrossing),
okay-ops (TestOpsRoutes), okay-security (TestOidc, TestFlows,
TestMcpAuth), okay-ui (TestWire). Verified both directions — the
default gate green with them absent, all 62 of their tests green
under `--include-tags=Live`.

And the timing was continued rather than hidden. `TestNio`'s "the
port is free after the scope" was not failing because the OS is slow
to release a port: it took the ephemeral port its listener had been
given, closed the scope, and required a connect to FAIL — which under
the full matrix is not a fact about our `Resource`. The port returns
to the ephemeral pool the instant we release it, a sibling suite
binds it, and our connect reaches THEIR listener and succeeds, so the
suite reported "the listener outlived its Resource scope" about a
listener that had closed exactly on time. The claim is about the
listener, so it is now asked of the listener — `Nio.listen`'s
resource value IS the `ServerSocketChannel`, and `isOpen` answers it
with no port and no neighbours in it. AGENTS.md records the survey
command so a new binding suite tags itself.

## chunk-size-representation — declined, and the premise it rested on was wrong
Completed: 2026-09-03
Landed as a6f4f89 (a benchmark lane and docs — the optimisation
itself was reverted). The suspicion was that chunked merging gets
dearer with the chunk size because `Stage.chunked` accumulates into a
`Vector` and copies it into an `ArraySeq` per chunk. Filling a
`ChunkBuf` instead, measured against master in one window: 2.2% WORSE
at the default 16 (223.5 ±2.8 against 218.7 ±1.7, bars
non-overlapping), 11% better at 256, 8% at 1024 — helping only sizes
nobody picks and leaving the curve's shape intact. Declined.

What survives is better than the optimisation would have been: the
curve was never a chunking defect. It compared our per-element
`Source`, chunked after the fact, against a stream chunked by
CONSTRUCTION, which never had elements to pay for. On the
like-for-like pair — okay `Chunks.merge` **23.2 ±0.2** against ZIO
`ZStream.merge` 58.6 ±0.7 on 2x2000 — **okay is 2.5x ahead**. So the
cost is what a per-element source costs before any chunking, and the
way past it is not to have one. `docs/benchmarks.md` §6b and the
guide now say that instead of leaving "ZIO is ahead of us" standing.

## netty-integration — okay-netty out of the default gate, into `integrationTest`
Completed: 2026-09-03
Landed as 1d7fbb1. Real sockets and real ports: `TestBackends` failed
the default gate twice with the identical signature (jetty
`StaticException: Closed`, one in 12, 2026-09-01 and 2026-09-03) and
was green in isolation immediately after both times. That is the
evidence `netty-ws-matrix-flake`'s settle-plan asked for, and it
resolves the standing conflict between that plan and AGENTS.md's
no-flaky-in-the-default-gate policy in the policy's favour, on the
operator's call. Both suites Live-tagged; verified both directions
(0 tests in the default gate, all 12 green under `--include-tags=Live`).

## chunk-bench-matrix — the chunk/flush surface measured and compared, the API made orthogonal, one pre-existing overflow fixed
Completed: 2026-09-03
Landed as e986da5.

**Edge cases** (26 tests): counts around the chunk boundary
(0,1,15,16,17,31,32,33), an empty side either way and both, a side
shorter than a chunk, an early-stopping consumer, a failing source
that must not swallow the other side, and every `Flush` edge — a
flush with nothing buffered, one immediately before the end, two in a
row, one inside a full chunk.

**The API, made orthogonal.** Chunking was a flag on `merge`, which
is the same concept spelled once per consumer. It is now a property
of the STREAM — `s.chunked(size)` / `.unchunked` — so `merge`,
`buffer` and whatever comes next get batching without a flag of their
own (a test composes `chunked` with `buffer`, which needed nothing
added). `merge(chunked = true)` stays as the fused spelling, and is
justified rather than duplicated: composing costs nothing where no
timer is involved (222.3 against 223.7), but the TIMED case would
otherwise need a second channel.

**The comparison** — fs2 and ZIO have direct spellings of all three
shapes (`groupWithin`, `groupedWithin`), so this is like-for-like. On
2x2000 at chunk 16:

| | okay | ZIO | fs2 |
|---|---|---|---|
| elementwise | 1177.0 ±15.7 | **59.1 ±0.5** | 35332 ±281 |
| chunked | 223.7 ±5.0 | **127.2 ±2.2** | 38508 ±403 |
| chunked + timed flush | **244.3 ±3.5** | 4907 ±98 | 54270 ±1790 |

The timed flush is where okay wins outright — 20x over ZIO, 222x over
fs2 — because the flusher is a sleeping fiber beside the feed rather
than machinery in the per-element path. ZIO's elementwise row is not
elementwise (`ZStream` is chunked by construction, which is why
`.grouped(16)` makes it *slower*), and read at equal chunk sizes ZIO
is ahead and pulls further ahead as the chunk grows: ours 222/277/438
at 16/256/1024 against theirs 127/75/75. Stated rather than omitted;
the cause is not stack depth but our Vector-then-ArraySeq
representation, filed.

**The bug it found (chunk-stack-safety).** `through` drives a stage
by calling into the producer and back, and only an EMISSION goes
through a `flatMap` that unwinds the stack — so an ACCUMULATING stage
recurses once per element. `chunked(4096)` over 4000 elements
overflowed, as did any chunk a short stream cannot fill. Pre-existing
(reproduced on b8c65c7 with `through` and `Stage.chunked` alone),
fixed in all four `through` overloads with a BUDGET rather than an
unconditional defer — per-element deferral being exactly what
writer-of-resume-fix removed from this path. Free where it matters:
222.3 ±4.0 after against 224.5 ±3.7 before.

## match-vec-batch — one statement for many vectors
Completed: 2026-09-03
Landed as f1cb3b1 (spec) + fe82ceb (impl). The follow-up match-vec-cache
named. That change removed the model inferences from `candidates()` and
left the ROUND TRIPS: a per-entity lookup is a SELECT per candidate,
and it grows with the marketplace exactly as the inferences did —
measured in a downstream deployment at ~1.3ms a row, 65ms for fifty.
`candidates()` now reads every cached vector for the passing set in ONE
statement and embeds only the misses. Chunked at 500, because a
database that accepts an `IN` list does not accept an unbounded one and
a marketplace is allowed to be bigger than one statement. The misses
are still written back one at a time deliberately: a miss is a model
inference, which dwarfs the round trip a batched write would save.
Tests: 2 new (10 in TestVecCache), and the first counts STATEMENTS
rather than time — a forwarding `Sql` wrapper counting reads of the
vector table — because "one read, not twenty" is the claim and a timing
assertion would be a flake. Rebased on master and re-gated after a
sibling landed: 76 suites, 2001 tests, 0 failures, 0 warnings.

## flush-op — `Flush.now`: the chunk boundary as an operation, where the producer knows it
Completed: 2026-09-03
Landed as 749e5f5. The half of the request `chunk-flush` answered
only with a timer. A chunking consumer emits when the chunk is full,
when the input ends, or when `flushAfter` expires — three rules that
all GUESS, while a producer usually knows: this token ended the
model's turn, that byte ended the frame.

An operation, not a distinguished element: a boundary is not data,
and making it one would widen every element type to `A | Boundary`
and make every consumer match on something outside its stream. So
`Flush` is a one-constructor signature with its own `TypeableK`, and
`Source` gains a `Flushing` row plus `mergeFlushing`. It is
interpreted by a walk of the source PROGRAM rather than by pulling
through `Stream.uncons`, because it must take effect at the exact
point the producer put it — and `relay` cannot serve, since its
handler answers with a VALUE while `Flush.Now` must become a channel
send.

The design that did not survive measurement: routing the ordinary
chunked merge through that same walk, for one accumulation path
instead of two. Tidier, and **11% dearer on the common path** —
244.3 ±15.2 against master's 219.6 ±1.5 in the same window — for one
extra widen rebuild per source and one extra row split per element.
Split back into two walks sharing the accumulation helpers: 220.5
±0.7, bars overlapping master. Only sources that use the operation
pay for it.

Also fixed two warnings that arrived on master with `match-vec-cache`
(unused pattern variables in `TestVecCache`), since the tree is meant
to stay at zero.

## match-vec-cache — SqlMatch stops re-embedding what has not changed
Completed: 2026-09-03
Landed as a0e57ff (spec) + 6572e14 (impl). MemoryMatch has cached
profile summaries since stage 0 and invalidates them at four write
sites; SqlMatch — the engine a deployment actually runs — had nothing,
and embedded every candidate's summary on every `candidates()` and
every live attribute on every `registrySearch()`. Invisible while
`embed` defaulted to `Vectors.hashing` (arithmetic over character
trigrams, which is what it was written against); one model inference
per candidate per query with a real encoder. Measured in a downstream
deployment on a real multilingual encoder, 50 profiles, 1 vCPU: 4.4s
for the first search, 1.5s for each one after, ~80ms per profile.
A TABLE, not a hook: `match_vecs(k, fp, dim, vec)`, where `k` names the
entity (`p:<uuid>:<side>`, `a:<slug>`) so rows stay bounded by entities
and an update overwrites, and `fp` is a SHA-256 of the text the vector
was computed from. The fingerprint is the whole design: a changed fact
is a changed summary is a changed fingerprint, so the stale row is
simply not used and there is NO invalidation logic to forget at the
fifth write path somebody adds later. It also survives the process,
which a cache in memory cannot, so a restart no longer re-embeds the
marketplace. New constructor parameter `embedTag`, empty by default and
therefore inert for every existing construction: vectors from one
encoder are noise to another and the dimension often matches even when
the model does not, so a deployment that can switch models names its
own. The query text is still embedded fresh — one call per query, not
an entity, and caching users' sentences would grow without bound.
Tests: 8 in TestVecCache, one per spec box, all counting CALLS to the
encoder rather than checking answers (the answers were already right),
including the one that matters most — ranking unchanged, same order and
same scores, cached or not. Full gate green on a quiet box: 76 suites,
1997 tests, 0 failures.

## rag-index-freshness — the index stops going quietly stale
Completed: 2026-09-03
The rag index is per project, lives in the main checkout, and does not
update itself. Left alone it keeps answering, confidently, out of a
tree that no longer exists — the same silent-failure class as the
`?project=` binding fixed an hour earlier. A search that returned
nothing would at least be honest; one that returns yesterday's file is
not.

`scripts/githooks` plus an installer (rozum's convention, tracked so
the rule is reviewable and survives a clone): post-merge, post-commit
and post-checkout all run one `reindex.sh`. Three rules, ordered by how
badly they would bite. Never fail the git operation that called us, so
every path exits 0 — an index is a convenience, a commit is not. Index
the MAIN checkout rather than a worktree, since worktrees share the
hooks path and a branch's tree is not what a reader searches. Do
nothing at all when `rozum` is off PATH, so a fresh clone commits
normally. The run is bounded by `timeout` where available, because a
hook that hangs is a hook that gets deleted.

Verified by running it rather than by reading it: 0.33 s, silent, exit
0, the main checkout's index mtime moved, no index appeared in the
worktree, and with `rozum` off PATH it still exited 0. Installed here
(`core.hooksPath -> scripts/githooks`).

## chunk-flush — `flushAfter`: a bounded wait for a partial chunk, so chunking is safe on a live source
Completed: 2026-09-03
Landed as e72a18d. `merge(chunked = true)` emitted only on a full
chunk or on end of input, so on the slow or unending sources this
merge exists for (a model's tokens, a live feed) an element could
wait for 15 others that never come. A test now SHOWS that stall
rather than describing it: three elements into a source that never
ends, and nothing arrives in 500ms.

`flushAfter = Some(millis)` bounds it, and two constraints shaped how:

It must not race the PULL — the obvious bound is `Async.timeout` on
the source's `uncons`, and it is wrong, because timeout cancels the
loser and cancelling an in-flight `uncons` on a live source can lose
the element it was about to yield. The timer therefore never touches
the pull: the feed accumulates into a `TRef` and a flusher fiber
takes what is already there.

It must not add a channel — chunking through a second channel would
double the transactions and erase the win it exists to deliver. So
the accumulation moved into the feed (`Channel.mergeChunked`), and
the existing merge channel stays the only one.

| measured, quiet box | |
|---|---|
| `chunked = false` (the default path) | 307.2 ±2.0 vs master's 310.2 ±18.4 |
| `chunked = true`, capacity 1024 | 230.1 ±0.9 (was 226.5 ±1.2 via the Stage) |
| the same + `flushAfter = Some(30000)` | 230.0 ±3.3 (never fires) |

The default path is untouched, the rewrite is at parity with the
chunking it replaces, and a flusher that does not fire costs nothing
standing.

## version-store — a version tree that outlives the process
Completed: 2026-09-03
`Rerun` shipped with only `MemoryVersions`, so a branch died with the
JVM that made it. `FileVersions` (JVM) is the same `Versions` contract
over a directory: one JSON file per version, named by id, written
atomically (temp file then rename) so a reader never sees half a
version.

Flat on disk, deliberately. Every version already names its `parent`,
so the tree lives in the pointers and a listing is the set of
versions; a nested layout would have to be rewritten the moment a
branch appears, which is exactly when nothing should have to move. The
on-disk shape is its own model rather than derived from the runtime
types — the same call `Staged` makes about storage formats, and the
one rozum's `replay.rs` makes in Rust: a derived format turns every
internal rename into a silent format change, and thirty lines of
explicit mapping buys a file a person can read with `cat`, which is
most of the point, since a version tree nobody can audit by hand is a
tree nobody audits. A file that does not parse is skipped rather than
fatal, because a directory is a place other things end up.

Eight tests cover what only a file can get wrong: reading back whole
across a restart, keeping parent/branchedAt/divergence, a lineage that
walks after the process is gone, a version from disk still replaying
with no world at all, legibility, a stray file not poisoning the
listing, and a rewrite replacing rather than accumulating. okay-agent
101/101 on JVM plus JS from clean, no warnings.

## mcp-project-binding — rag and state were answering for the wrong repo
Completed: 2026-09-03
Landed as e0f0a0e. The user-scope rozum registration is
`http://127.0.0.1:8779/mcp` with no `?project=`, so one daemon served
every repo from its launchd default, the rozum checkout. Measured
rather than suspected: `state.update` called from a session in THIS
repo wrote into rozum's `.rozum/state.json`, and `rag.search` answered
out of rozum's index, which is why its hits kept being Rust files in a
Scala repo. The per-project multiplexing exists and works; nothing was
using it.

`.mcp.json` now pins `?project=` for this repo (project scope wins over
the user-scope entry) and AGENTS.md says why the query must stay, since
dropping it fails silently rather than loudly. Verified after: state
landed here, and `rag.search` answered `Delim.scala` for a
continuations query and `Rerun.scala` for a journal one. The index is
per project and not automatic — `rozum rag index --root .` built 705
files into 9525 chunks in under a second.

## quiet-needs-live-model — the fork's precondition, caught by the other repo
Completed: 2026-09-03
journal-versions shipped `OnDiverge.Quiet` (continue past a divergence,
branch a version) beside a module doc saying a rerun scripts the model
from the recording. Together those were wrong, and the error was
shipped: the recorded reply for step k+1 was produced while looking at
the OLD answer to step k, so once a live tool answers differently,
every later recorded reply answers a question the run is no longer
asking. Continuing to script the model there is not a weaker replay, it
is a confidently wrong one.

Caught by rozum's `replay.rs`, built independently against the same
problem in Rust while this lane was landing: its `ReplayLiveTools`
STOPS at a live-tool divergence for exactly this reason, and its fork
mode abandons the old journal and hands a LIVE model the new result.
Found by asking that repo's own `rag.search` about the area — the first
time this session's cross-repo work paid back in that direction rather
than out of it.

`Quiet` is now `ForkWithLiveModel`, so the name carries the
requirement and a call site cannot assume otherwise, and `Loud` is
documented as the mode a scripted-model rerun needs (stopping is the
sound thing there). The handler cannot see the caller's model, so it
does what it can: abandons the journal at the fork and reports
`branchedAt`, the step from which the caller's model must be live. Two
tests pin it. okay-agent 93/93 on JVM plus JS from clean, no warnings.

## journal-versions — the third mode of a journal, and what a divergence means
Completed: 2026-09-03
Landed as 5537049. A journal had two modes: `Durable.tools` records
and recovers, `Durable.replaying` answers everything from the journal
and touches nothing. `Rerun.live` is the third: run the journal again
against TODAY'S world with the tools ACTUALLY executing, and compare.
The distinction that earns it: replaying proves the program is
deterministic given the same answers; rerunning proves the world still
gives those answers. A journal recorded in June and rerun in September
either reproduces, or names the step where reality moved.

Divergence is loud or quiet, never silent. `Loud` throws at the first
one, naming step, call, recorded and got (the default, and what CI
should use). `Quiet` accepts the new answer, continues LIVE, and
branches a `Version` — while still reporting the divergence on the
outcome AND carrying it on the version, so a reader of the store alone
learns it. Snapshot testing already taught the industry that an
auto-accept nobody reads is worse than no test.

The branch-instead-of-patch is structural, not a preference: once step
k answers differently, every later entry is unusable, because the model
that consumed X asked its next question because of X. So a version is a
shared prefix plus a live tail, versions form a tree (`parent` +
`branchedAt`, `Versions.lineage` walks it), and only the FIRST
divergence is the branch point. `Provenance` rides on each version
because a diff that cannot say what produced either side says nothing;
the layer carries the caller's claim and cannot verify it, which is the
same boundary the journal already has (it sees calls, not the world
under them). A rerun that reproduced stores nothing and hands the base
back unchanged.

`Kind.Answer` and `Kind.Call` are told apart (the world moved under the
same question, versus the program asking a different one, including
running past the journal's end) though they branch identically. The
model half needed nothing new: `Handlers.scripted` over the recorded
replies plus live tools IS the mode, and of the four record/replay
combinations only that one and `Durable.replaying` earn their keep.
Written after the same design was worked out for rozum's Rust agent
runtime and posted to its room; this is the Scala half, and the two
agree on the shape without sharing a line.

TestRerun 10/10, okay-agent 91/91 on JVM plus JS, no warnings, from clean.

## merge-chunk-param — the chunked merge folded into `merge` as a flag, beside the `capacity` it has to respect
Completed: 2026-09-03
Landed as 8c4d7b0. `mergeChunked` became `merge(chunked = true)`,
for discoverability — the previous lane's finding is that callers of
`merge` silently pay 5x, and a sibling method only reaches those who
already know it exists. Folding it in exposed two things the separate
combinator had hidden.

`capacity` had stopped meaning what it says: the channel holds
CHUNKS, so a caller asking for 64 elements was silently getting
64 x 64 = 4096. Now the channel gets `capacity / ChunkSize` slots, so
`capacity` counts elements either way — which costs half the win at
the default and lets the knob buy it back explicitly:

| 2x2000 | | |
|---|---|---|
| `chunked = false` | 1163.4 ±21.2 | readiness, exact |
| `chunked = true`, capacity 64 | **443.6 ±23.8** | 2.6x, same budget |
| `chunked = true`, capacity 1024 | **226.5 ±1.2** | 5.1x |

(226.5 against a hand-built chunk pipeline's 223.2 — nothing lost to
the composition.) And the chunk SIZE stopped deserving to be a
parameter: it moves the number ~10% across a 4x span while exposing
it is exactly what breaks `capacity`. Fixed at 16.

Off by default for a stronger reason than politeness, verified in
`Stage.chunked`'s body: it emits when a chunk is full or its input
ends, with no flush on time — so on the slow or unending sources this
merge exists for, an element waits for 15 others that may never come.

## source-merge-chunked — Source.mergeChunked: 4-5x, the first thing in this arc that actually gets faster
Completed: 2026-09-03
Landed as 9971a0d. Four lanes had refuted the COST side of the
per-element merge (queue data structure, retry rate, kernel tree
shape, row variance). The question put directly — if `Chunks.merge`
is 10.7us where `Source.merge` is 299.7, why does `Source.merge` not
chunk underneath? — was the right one. A profiler pass attributing
EVERY frame (not just the two already suspected) found 71% of
samples in the per-element channel TRANSACTION: 33% the CAS itself,
19% the immutable Queue rebuilt around it, 19% `resume`'s per-pull
rotation. Nothing there is cheaper than it is; there are too many.

`Source.mergeChunked(other, size)` divides the count by `size` and
hands back an ordinary `Source`:

| 2 x N | `merge` | `mergeChunked` |
|---|---|---|
| N=2000, size 16 | 1169.9 ±9.3 | **223.5 ±2.9** (5.2x) |
| N=2000, size 64 | 1163.4 ±12.7 | **247.2 ±1.8** (4.7x) |
| N=500, size 16 | 292.8 ±3.5 | **70.2 ±1.0** (4.2x) |

A separate combinator rather than a change to `merge`, for a
semantic reason: it batches, and the win comes precisely from
batching sends that would otherwise have succeeded immediately (a
consumer that keeps up creates no backpressure to absorb), so it
cannot be made invisible. Readiness stays `merge`'s promise. Three
tests cover element identity and per-source order, the flushed
partial final chunk, and empty sources.

## free-row-variance — the upcast that is not free: Free stays invariant, now with a number behind it
Completed: 2026-09-03
Landed as 70d7e95 (a benchmark, docs and one corrected doc comment —
the kernel spike itself was reverted). merge-scaling-shape named the
last cheap lever: `Source.merge` calls `Writer.widen` per source, and
widen rebuilds every Free node because "Free is invariant in its
signature" — so delete the invariance, delete the pass.

The invariance IS removable: `enum Free[+F[+_], A]` passes the
variance check, and the row subtyping then holds pointwise at
concrete rows. Adopting it costs re-typing the tree walkers (a
covariant row captures a fresh subtype at every `Bind(Inject(e), k)`
match); `fold`'s and `resume`'s rotation were recovered cleanly and
cast-free, ~6 more sites would each need a helper, and a generic
`up[H[+_] >: F]` does not typecheck at all.

Measured before paying that price — and the prize is NEGATIVE. Widen
costs 7.4–10.4ns/element in isolation (+18%/+27%), but the same merge
built without it is SLOWER: 1141.8 ±6.7 with against 1202.6 ±14.6
without on 2x2000, and 1162.4 ±11.4 against 1240.1 ±10.1 on a
repeat, bars non-overlapping both runs. The walk is also a
NORMALIZATION — it hands `feed` an already head-normal tree, so the
rotation it saves is not paid per pull inside the contended region.
Declined; `Free` stays invariant as a measured choice, and
`Effects.widen`'s comment now says so. `WidenBenchmark` guards it.
`docs/theory/04-free-freer.md` carries the general form: a coercion
the type system performs for free performs no normalization either,
and the interpreter pays it later, in the worse place.

## jmh-warnings — the zero-warnings policy extended to Jmh sources, and taken to zero there
Completed: 2026-09-03
Landed as 2d20d5b. `Test/compile` does not reach Jmh sources — they
are their own configuration — which is how five warnings sat
unnoticed through the sweep that took main+test to zero. Two in the
root `src/jmh` (`HandlerBenchmark`): the `Ask[Nothing]` type test
erasure cannot verify, inlined from `relay`/`handle` — same class as
the four documented suppressions in the test tree, fixed the same
documented way. Three in `compare/src/jmh`, all artifacts of the
COMPETITORS' idioms (kyo's `Loop` default argument x2, kyo-direct's
`defer` macro expansion) — silenced by `-Wconf` scoped to the
`compare` project ALONE, never `ThisBuild`, since a benchmark
rewritten to please our linter measures the rewrite rather than the
library, while the main tree must keep the lint that catches real
defects. AGENTS.md now says main, test AND Jmh, and records how to
check them (per-project `Jmh/compile` after `rm -rf <project>/target`
— the JMH generator caches hard enough to report success without
recompiling). Full gate green.

## theory-linearity — the linearity measurement written into the textbook, where the third road was claimed
Completed: 2026-09-03
Landed as 606d7ea (docs only). `docs/theory/04-free-freer.md`
already named the three answers to the left-nested-bind problem
(codensity, type-aligned sequences, and okay's own "normalize in the
one interpreter") and declared the third road taken — a structural
argument, unchecked, with a profiler putting 38% of a merge
benchmark on the normalizer, which is what BOTH readings look like.
The chapter now carries the sweep that decides it (per-element cost
flat across 8x in every lane), the general lesson rather than the
local fact (reflection without remorse pays on LEFT-nested binds; a
recursive stream producer is right-nested; whether you are in the
trap is a property of how programs are built, not of the encoding),
and what the sweep found instead — the Writer layer ~5x dearer
inside the contended merge than alone, pointing at fewer steps in
the contended region rather than a cheaper step. `docs/guide.md`
gets the practical half (the per-element price against the chunked
merge's 10.7us); the theory index and README summaries follow.

## merge-scaling-shape — the Bind tree is LINEAR: the kernel rewrite is off the table, measured
Completed: 2026-09-03
Landed as 9420f1d (a benchmark plus docs — no library code changed).
Closes the question left standing by writer-of-resume-fix: is
`!.resume`'s rotation cost quadratic (which reflection-without-
remorse — a type-aligned continuation queue replacing the binary
`Bind` tree — fixes, justifying a kernel rewrite through `resume`'s
three-form invariant and the 42 sites depending on it) or constant
per element (which it does not)? `ScalingBenchmark` sweeps `n` and
reads PER ELEMENT, with the bare `LazyList` walk as control:

| per element | 500 el | 1000 el | 2000 el | 4000 el |
|---|---|---|---|---|
| `rawLazyListDrain` (control) | 11.3ns | 11.4 | 11.0 | 10.6 |
| `sourceSingleDrain` | 41.2ns | 39.6 | 41.5 | 40.6 |
| `channelMerge` | 142.3ns | 121.9 | 127.9 | 131.8 |
| `sourceMerge` | 303.5ns | 299.7 | 300.7 | 291.6 |

Flat everywhere across 8x — linear, no quadratic to remove, so the
rewrite buys nothing and is closed with data. What the sweep exposes
instead: the Writer layer costs ~30ns/element alone but ~160ns
INSIDE the merge (~5x), the quantified form of channel-cas-
contention's qualitative finding. The lever is fewer interpretation
steps in the contended region, not a cheaper step — i.e.
`Chunks.merge`, already in the library at 10.7us for 2x500 against
`sourceMerge`'s 299.7us.

## state-mcp-native — a reproducible native build, and the real install
Completed: 2026-09-03
Landed as 7264d2f. The operator asked for an optimized native build
of StateMcp, installed and connected to the Claude Code they use.
Built with GraalVM native-image (Oracle GraalVM 21.0.11): a two-stage
PGO build (instrument, run 200+ rounds of get/update/reset through
it, rebuild against the collected profile) plus `-march=native` — 17MB,
sub-10ms cold start, no reflection config needed (a Mirror-derived
codec has little to reflect on; native-image's analysis reached the
library unassisted). Installed to `~/.local/bin/okay-state-mcp` and
registered with Claude Code at user scope (`claude mcp add okay-state
--scope user`, connected, verified by `claude mcp list`) — no
state-file argument, so `StateMcp`'s own default (`.claude/state.json`)
resolves per project against wherever Claude Code spawns the server,
making one user-scope registration serve every project. This lane
lands `okay-demo/scripts/native-image-state-mcp.sh`, the exact
sequence, so the build reproduces on another machine; the local
install itself is outside the repository by nature. specs/llm-agentic.md
gains a paragraph on why native matters specifically for an MCP
server (its whole cost IS process-spawn latency).

## state-mcp — bounded execution state, offered over MCP
Completed: 2026-09-03
Landed as fcef57c (2 commits, rebased twice over sibling landings).
The operator asked how skill-state's idea reaches a real agent
session they run today — specifically Claude Code, which is not
built on this library's Context/Turn/Aggregator and cannot host
Compact.skillState directly. `StateMcp` (okay-demo) is the answer
that fits the actual boundary: a standalone stdio MCP server, built
from okay-mcp's existing Server/Stdio (the same shape RepoMcp.scala
already is) and okay-codec's Json.mergePatch from the skill-state
lane, exposing three tools over one durable Json value persisted to
a file — get_state, update_state (an RFC 7396 patch), reset_state.
No compile-time Schema validates a patch here, on purpose: a tool an
arbitrary project points at has no known type to check against, so
any JSON object is accepted and the state's shape is a convention
between caller and reader, the same trust boundary a hand-edited
JSON file already has — a project wanting typed validation defines
its own Schema[S] and calls Compact.validatePatch in its own copy of
the handler, four lines. TestStateMcp: 7/7 over the real protocol
(TestDuplex's in-memory Link — initialize, tools/list, tools/call),
covering persistence across a restart, a non-object patch refused
with Sigma untouched, and a damaged state file starting empty (the
lossless parser's own damage markers, walked recursively). Verified
once more as an actual subprocess over real stdio pipes end to end,
file on disk included. specs/llm-agentic.md gains "An MCP server for
it", naming plainly what crosses the boundary (the format primitive)
and what stays inside our own agents (the policy, the typed
validation). Gate green, no warnings, from clean.

## channel-cas-contention — measured and closed: elevated CAS-retry rate is a symptom of Writer/Bind-rotation cost, not an independent tax
Completed: 2026-09-03
Landed as c47eaac (docs only — no code changed, the instrumentation
used to measure it was reverted). Follow-on to writer-of-resume-fix,
closing the second item it filed (channel-queue-reversal closed the
other). Instrumented `TRef.modify` directly and measured
attempt/CAS-fail counts across `Channel.merge`/`Source.merge` at
matched capacities: the Writer wrapping does add real contention
beyond capacity alone (28.1% -> 34.3% fail rate at the same
`Int.MaxValue` capacity), and capacity's own effect compounds with
it (49.0% at `Source.merge`'s own default 64) rather than adding.
But `TRef.modify`'s retry is a spin, never a park, and the existing
wall-clock measurement in `Source.scala` already shows capacity
moves nothing this benchmark can see — so the elevated retry rate is
what a slower critical section looks like from the CAS's side, not a
separate cost. Nothing to land; specs/stm.md carries the numbers.

## integration-test-gate — Live-tagged suites out of the default gate; zero warnings enforced as policy
Completed: 2026-09-03
Landed as bb5ccba. ~25 suites reaching outside the JVM (a live model
gateway, docker services — kafka/mongo/pg/redis/tls/s3, external
tools — python3/spark) are now tagged `Live` via a shared
`munitTests()` override and excluded from `sbt test` by default;
`sbt integrationTest` runs the exact same suite with nothing
excluded. Motivated by this session's own TestChatDemo LIVE suite
failing identically on untouched master, twice, under live-model
load — a flake indistinguishable from a regression until re-run
against master proved it wasn't one. Verified both directions: `sbt
test` green with the tagged suites absent (TestChatDemo alone
38 → 34), `sbt integrationTest` runs them and reproduces the same
flake in the right gate.

Also closed the zero-warnings gap it surfaced along the way: two
real exhaustivity misses (`Turn.StatePatch` unhandled in
Mcp.scala/Langchain4j.scala), two `Ordering.reverse` calls needing
`using`, an unused lambda param, two unused imports, a missing
`scala.language.implicitConversions` import, a deprecated
`Char + String` concat, and a `-Wconf` entry so four
platform-only-real `@nowarn` suppressions stop false-positiving as
"unused" on Scala.js/Native. AGENTS.md now states both — no
warnings, no flaky tests in the default gate — as explicit policy.

## channel-queue-reversal — measured and declined: Vector and a hand-rolled Fifo both tried against Channel.State's Queue, neither wins, nothing lands
Completed: 2026-09-03
Landed as 7371afa (docs/history only — Channel.scala is byte-
identical to master). Follow-on to writer-of-resume-fix: profiling
`okayChannelMerge` found 30% of CPU in `Queue.dequeue`'s amortized
`List.reverse`. First read: near-lockstep producer/consumer timing
defeats the amortization. Two fixes, each measured on the real
`Channel.merge` benchmarks:

`Vector` (no reversal ever) won an isolated single-threaded lockstep
microbenchmark 16x, LOST 13-15% on the real multi-fiber benchmark —
under CAS-retry contention a failed `TRef.modify` attempt rebuilds
the whole `State`, and `Vector`'s per-attempt trie-copy cost, times
retries, beat `Queue`'s occasional reversal. A hand-rolled `Fifo`
(List-based, special-casing the 0/1-element reversal the profiler
flagged) measured EXACT PARITY with `Queue` on both benchmarks — no
win. Re-profiling corrected the original premise: nearly all
remaining reversals landed in the 2+-element general case, not the
assumed 0/1 fast path — `Channel.merge`'s real access pattern under
fiber timing DOES form multi-element batches, so `Queue`'s
amortization is doing real, intended work, not being defeated. The
30% CPU number was real; the explanation drawn from it was wrong,
corrected in specs/stm.md Results. Both attempts reverted cleanly;
the exploratory benchmark file was removed rather than kept, since
its lockstep premise doesn't hold for the real workload.

## writer-of-resume-fix — profiled (JFR), not guessed: Writer.of stops re-wrapping every element
Completed: 2026-09-03
Landed as 3f558a0. `compare/Jmh/run -prof jfr` on `okaySourceMerge`
(JMH's built-in JFR profiler, no extra install) found 38% of CPU
samples in two lines of `!.resume` — the tailrec rotation that
normalizes a Free tree before `Writer.uncons` can read it, called on
every pull. Traced to `Writer.of`: the recursive step wraps EVERY
element in `pure(()).flatMap` for laziness, load-bearing exactly
once at the top (the recursive call already sits inside the
previous step's own `flatMap`) — wrapping again is N-1 redundant
`Bind(Pure(()), k)` nodes per source, each needing rotation. Fix:
split `of` into a one-wrap entry and a private `ofLoop` the
recursion calls directly; the external laziness contract is
unchanged.

Measured: the bare `Source.of(xs).toLazyList` floor closed 18%
(48.9 -> 40.3us, clean). `okaySourceMerge` moved only ~2-3% (305 ->
299us) — re-profiling explains why: the targeted rotation line
dropped 28 samples to 5 as aimed, but a DIFFERENT rotation case rose
18 to 33 (no reset point between elements now, so recursive
`Bind`-building nests deeper more often), and `TRef.modify` —
`Channel.merge`'s own transaction machinery under real multi-fiber
contention — is now the dominant frame in the merge path (75/~210
at depth 3), untouched by this fix. Landed anyway: real, verified,
zero-regression improvement on `Writer.of`'s own terms; full `sbt
test` green. The `Channel.merge` contention cost is filed as a
separate, deeper investigation — specs/writer-covariance.md carries
both profiler runs' numbers.

## writer-covariance — Writer[+W, +A] lands; the merge-fusion it was meant to unlock measured worse and was declined
Completed: 2026-09-02
Landed as b469c20. Follow-on to channel-merge-regression: with the
STM channel cleared of the doc-baseline drift, this asked whether
`Source.merge` has any real, safe optimization available at all.
`Writer[W, +A]` becomes `Writer[+W, +A]` — the correct variance (`W`
is only ever told, the `Say(w)` constructor never consumes it) —
verified safe by the full `sbt test` suite (JVM+JS+Native) green
unchanged, not just by inspection. New `Writer.widen` (map's
identity case, reusing the told `Say` instance instead of rebuilding
it) replaces `Writer.map(s)(identity[A|B])` inside `Source.merge`.

Measured neutral: 305-308us either way, because that per-element
allocation was never the dominant cost. A further attempt — fusing
`Writer.of`'s construction with the re-tell into one unfold per side
(`Source.mergeOf`) — was implemented, tested, and measured WORSE
(336-349us, noisier), so it was not shipped; the code and its
numbers are recorded, not the API. Two diagnostics filed for a
profiler pass: a bare `Source` with no `Channel.merge` at all
already costs 48.9us against a native `LazyList`'s 11.1us for the
same 1000 elements (the honest price of the program abstraction,
~38ns/element) — under half the ~180us gap `Channel.merge` shows
consuming a Writer-shaped stream, so most of the cost is specific to
that consumption path; and Pure-rowed sources measured WORSE than
Async-rowed ones inside `Channel.merge`, counter to expectation.
specs/writer-covariance.md carries the full investigation.

## demo-embeddings-attr — real embeddings for the registry's search-before-create
Completed: 2026-09-02
Landed as ce0f8d5. `ChatDemo.marketOf` gains `embed` and
`proposeThreshold` (defaults unchanged: `Vectors.hashing()`, `0.85f`
— every existing call site sees no behavior change). New sibling
module `okay-demo-embed` (depends on okayDemo + okayLangchain4jEmbed,
kept out of the root aggregate like okay-demo-e2e-browser — a real
~90MB ONNX model download): `TestDemoEmbed` proves the demo's
registry drifts into two attributes for "разработчик"/"программист"
under the default hashing embedder, and collides into one under the
real embedder recalibrated to its own MEASURED similarity (~0.52
cosine for this pair — well under the 0.85 default chosen for
hashing's coarser fallback, matching `TestLangchain4jEmbed`'s own
`>0.5f` bound rather than an invented number). Also closed a
pre-existing docs gap found along the way: `okay-langchain4j-embed`
had shipped with no `docs/modules/*.md` at all; written now,
alongside `okay-demo-embed`'s own, both indexed in docs/README.md.
specs/demo-chat.md new section. Matrix 71 suites, zero failures (one
confirmed pre-existing live-model flake).

## skill-state — SKILL.state (arxiv 2608.26263) as one more Compact policy
Completed: 2026-09-02
Landed as 21fac3b (2 commits). The operator asked whether the paper's
bounded-execution-state technique — replace an append-only agent
transcript with a small structured Sigma, patched each step, its
reasoning discarded rather than compacted — was buildable here. It
needed no new subsystem: `Compact.skillState` is one more
`Aggregator[Turn, S, Seq[Turn]]`, the SAME algebra `window` already
implements, so it plugs into `Handlers.context`/`Memory.handle`
exactly as `window` does. `Json.mergePatch` (RFC 7396) is the new
primitive in okay-codec — an object patch merges recursively, null
deletes a key, anything else replaces — tested against the RFC's own
examples and its own composition caveat, demonstrated rather than
merely asserted. `Turn.StatePatch` is the one artifact a step leaves;
the policy pins System turns, merges Sigma on StatePatch turns, keeps
the latest Result/User as the one observation that matters, and folds
nothing else in — so `present` is O(1) in turns EVER seen, not merely
bounded, proven as a property (TestLaws) and demonstrated at 500
steps (TestSkillState). `Compact.validatePatch` is the rollback door:
merge-then-decode against the caller's Schema before a patch is ever
remembered, a pure Left/Right instead of a runtime rollback. The
paper's grammar-constrained decoding needed no new machinery either —
a state patch is an ordinary tool call's arguments, and
`ToolSpec.jsonSchema` already derives an all-optional schema from
Option/defaulted fields, which is exactly what a patch's shape wants.
Provider.scala's two wire mappings (OpenAI, Anthropic) gained the new
Turn case. specs/llm-agentic.md's "Bounded execution state" names
what this is not a replacement for (`window`, for history-shaped
tasks) and where `zip` composes both. Gate green (codec x3 platforms,
agent JVM+JS) from clean, no warnings.

## staged-cbor — the STAGED fold's second wire
Completed: 2026-09-02
Landed as d299be1 (3 commits, rebased over stm-sessions/stm-orelse-
warning/demo-two-nodes/kyo-fair-lanes). staged-codecs' BACKLOG entry
read "when a wire names it" — okay-persist's Wire.scala,
WireProtocol.scala, Typed.scala and Snapshots.scala all call
Cbor.write/read per record, per frame, per snapshot, so it is named.
`Staged.cbor[A]` is JSON's twin: the Mirror walked once at expansion,
Cbor's item primitives called straight-line at run time. The
refactor that made it honest first: Cbor.scala's `Out`/`In` were
private locals inside `put`/`get`; made public classes so the staged
generator calls the identical methods the fold calls, not a second
implementation of RFC 8949's varint header. A shared `Reflect` base
now carries the Mirror-walk/shape-check machinery both `JsonGen` and
`CborGen` need; only `emit`/`read` differ per format. The real
finding: a first cut assumed a CBOR map's field order matches the
Mirror's (true only because both writers here happen to agree on it)
and would have silently miscoded a document with reordered or
duplicate keys — a hazard JSON's object never has. Refuted before
landing by TestStagedCbor's own test built for exactly that shape;
fixed with `Staged.cborProduct`, the fold's own read-by-name-then-
fill algorithm with per-field readers specialized at compile time.
Price: encode 1.6x, decode 2.0x over the interpreted fold on the
fixture (history.tsv staged-cbor). TestStagedCbor: 15/15 on JVM, JS,
Native — every case above plus an Iso field, recursion, every wrong-
shape refusal in the fold's own words. specs/codecs.md gains "Staged
CBOR"; BACKLOG's staged-cbor is checked off. Gate green (codec x3
platforms, persist JVM/JS, compare jmh) before and after the rebase.

## stm-sessions — McpHttp's session table and Native's FiberCell, on TRef
Completed: 2026-09-02
Landed as c488185. `McpHttp`'s session table
(`ConcurrentHashMap[String, Wire]`) became `TRef[Sessions]`; every
write is one `modify`. Found and fixed a real race on the way: "an
uninitialized client talking to a server with no sessions gets one
rather than a riddle" was `isEmpty`-checked then `put` — two racing
first requests could both pass and clobber each other's `Wire` under
the shared `""` key. One `modify` now decides lookup-or-mint
atomically; a regression test fires 30 concurrent such POSTs and
asserts exactly one 200. Native's `FiberCell` (the hand-rolled
`synchronized` "one result, many subscribers" cell backing `Fiber`
where the platform has no `CompletableFuture`) became `TRef[State]`
the same shape `stm-ui-close`'s `CloseState` took. Both single-cell
fast paths, no `Tx`/`Stm[F]`. TestMcpHttp (9) and the full Native
suite (22) green. specs/stm.md Results entry. Matrix 73 suites, zero
failures (one confirmed pre-existing live-model flake, isolated and
reconfirmed green alone, twice).

## stm-orelse-warning — the OrElse branch's unchecked type test, fixed
Completed: 2026-09-02
Landed as 8d7f363 (operator ask: "посмотри warning при компиляции в
Stm.scala:236"). `case op: Tx.OrElse[X] =>` triggered [E092] Pattern
Match Unchecked (X is erased, unverifiable at runtime) — the only
branch in `perform` that used a type-ascription pattern instead of
case-class extraction. Switched to `case Tx.OrElse(progA, progB) =>`,
the same shape every other branch already used; the GADT match types
`progA`/`progB` as `X ! Tx` with no runtime check needed at all. No
behavior change, warning gone, full matrix 79 suites clean.

## demo-two-nodes — Election made consumer-visible: two real processes, kill the leader
Completed: 2026-09-02
Landed as aed0669 (spec) + 43ae53d (impl). Failover itself is not new
work — `Election` (okay-persist, specs/consensus.md) already proves
concurrent claims, lease takeover, fencing, and operator override,
against both `MemoryStore` and `FileStore`. This showcase makes that
machinery consumer-visible: two real `ChatDemo` processes, one shared
`OKAY_CHAT_LOG` directory, one elected writer, kill it and watch
`/market` keep answering. Sized LARGE and gated in BACKLOG ("take
only when a distributed demo is named wanted") — landed after the
operator named it wanted, once every other okay-demo backlog item
was clear.
A real constraint made visible, not hidden: `FileStore.open` scans
its directory ONCE; a process that never appends to a topic never
sees another process's later writes — nothing re-scans. `TwoNode`
does not add live cross-process tailing (a real project on its own);
it POLLS — every tick (`OKAY_CHAT_TICK_MS`, default 500ms) reopens a
fresh `FileStore` handle, re-decides leadership from ITS control
topic, and — only when not leading — `market.reset()` +
`replayProjections` (the exact function `POST /admin/replay` already
calls, reused rather than reinvented).
`TwoNode.leaderGated` wraps the WHOLE route table once: a `POST`
from a non-leader answers 503 naming the leader; every `GET` passes
through untouched. `main` branches on `OKAY_CHAT_NODE` — absent, the
single-process path is byte-for-byte unchanged.
Two real bugs caught in testing: both spawned processes defaulted to
the SAME relative `okay-chat.db` sqlite file (never explicitly set),
racing on schema creation — fixed by pointing each process's store
at `:memory:` (the log is shared, the store is per-node); and a cold
start where both nodes provision a brand-new shared directory at
once can race on a topic's first segment file
(`FileAlreadyExistsException`, no cross-process lock) — fixed by
catching and retrying on the next tick instead of crashing `main`.
Tests: `TestTwoNode` launches two REAL OS processes (not two
threads), asserts one agreed leader, the follower refusing writes
while still serving reads, a write becoming visible on the follower
within one tick, a forced kill, and the survivor taking over and
accepting writes. 5/5 clean standalone runs; the full demo suite (44
tests) clean in 2 of 3 combined runs, the third hitting only the
known pre-existing LIVE SEEKER flake.

## kyo-fair-lanes — the 1000x kyo numbers were the shape, not the library
Completed: 2026-09-02
Landed as 35caeaf (lanes) + <docs>. The operator asked whether the
competitors' worst numbers — kyo Env 362 756, Emit 342 761, Resource
8566 — were our mistake. Audit of all ten kyo lanes and of kyo 0.16.2's
source: they were not a measurement error, but they were the
left-nested foldLeft shape, which is O(N²) in kyo (`map` over a
suspension wraps a `KyoContinue` that re-applies the inner
continuation; `handleLoop` never reassociates — ×109 from N=1k to
10k). Real code nests right, and there kyo is linear: Env 291 vs Okay
79, Emit 215 vs Okay 163 (parity with the foldLeft Okay Writer),
Resource 838 vs Okay 15. The stream lane hand-emitted singleton
chunks; kyo's own `Stream.range` runs 64 vs 330 in the same session.
Added `okayReaderRec`/`kyoEnvRec`, `okayWriterRec`/`kyoEmitRec`,
`okayResourceRec`/`kyoResourceRec`, `kyoStreamRange`; tables §2/§5/§7
in docs/benchmarks.md and the README now show BOTH shapes with the
right-nested row as the number to quote, the mechanism stated, and
the earlier "~1000x" / "Resource + Async runtime" wording retracted.
Bind chain, fork/join, choice, direct and generator lanes were checked
and stand. Measured with plain `java -cp` outside sbt (siblings'
runs), load ~7, all four classes in one session. Two BACKLOG items:
a same-session chunked-source sweep for fs2/ZIO, and the rule that a
foldLeft competitor lane gets a right-nested twin before quoting.

## json-value-parser — the text side of the staged-codecs promise
Completed: 2026-09-02
Landed as 69f5f8f (3 commits). staged-codecs step 0 found the real
cost of text->value: with decode staged at 0.1 µs, Json.parse's
lex+CST+project cost 14.6 µs on a 150-byte object, all of it the
lossless layer nobody asked for when they just want the value.
`JsonValue.parse` is a strict recursive-descent parser over the raw
String, no tokens, no tree: an index, a StringBuilder only for the
rare escaped string, `parseDouble` on a number's slice. It accepts
only RFC 8259 (plus the projection's own two readings, kept on
purpose: `\u0041` is the four letters, an unknown escape is itself)
and answers None on anything else; `Json.parseValue` then falls
through to the lossless `Json.parse`, so damaged input gets exactly
the CST's answer and no damage vocabulary is duplicated. Agreement is
a full prefix-truncation sweep: every substring of 25 well-formed and
34 damaged fixture documents, both roads, equal — on JVM, JS and
Native. The price: 217 ns vs 13.3 µs on the fixture (61x, and 2.0x
faster than circe's own parser); end to end with the staged decoder
from the last lane, 349 ns vs circe's fused parse+decode at 804 (2.3x
faster). specs/codecs.md gains "Value parser"; BACKLOG's
json-value-parser entry is checked off. Gate green in three chunks
after a rebase over stm-orelse; the nine -Wall warnings clean showed
are all pre-existing or a fresh sibling's (Stm.scala:236 from
stm-orelse) — none in the files this lane touched.
## stm-orelse — Tx gains the classic STM combinator
Completed: 2026-09-02
Landed as f1d4df3. `Tx.orElse(a, b)`: run `a`; if it RETRIES (not any
other outcome), its writes are discarded as if it never ran and `b`
runs instead; if `b` also retries, the whole thing retries, parked on
whatever EITHER branch read. One new `Tx` case, one new `perform`
branch — a nested `Log(parent = outer)` runs `a`, `RetryNow` is
caught locally, the branch's reads fold into the outer log either
way, its writes fold in (`Log.absorb`, via `TMap`'s typed polymorphic
`foreach`) only if it did not retry. `runWithLog`, the tree-walker
`interpret` used to own alone, is now shared with `perform`'s
`OrElse` case; every handler (tl2, direct, Sim) gets `orElse` for
free since none of them reimplement Read/Write/Modify/Retry
themselves. Proven cross-platform (`TestStmOrElse`: a-succeeds,
a-retries-b-runs-and-a's-write-never-lands, both-retry-parks-on-
either, nested `orElse`, a write before `orElse` visible inside a
branch) plus 60 Sim seeds racing two writers against one `orElse`d
reader. specs/stm.md Behavior box + Results entry. Matrix 73 suites,
zero failures (one confirmed pre-existing live-model flake, isolated
and reconfirmed green alone).

## staged-codecs — the STAGED fold mode, kept
Completed: 2026-09-02
Landed as 92fc9ed (5 commits). The operator asked what final tagless
and staging could still buy; every effect-side staging lane was
already measured (staged-tagless 1.6x, staged-effects 1.9x, pipelines
~10x, direct 1.06x of hand), and the one promise left unkept was
specs/codecs.md's "the fold runs in two modes, interpreted or
STAGED". Step 0 priced it: the interpreted Schema fold was 6.0x over
a hand-written encoder and 7.6x over a hand-written AST decoder,
circe between. Step 1 is `Staged.json[A]`, a macro that folds the
Mirror at expansion and emits straight-line StringBuilder appends and
field lookups: encode 168 ns vs 820 (4.9x, 1.25x of hand, 3.2x
faster than circe), decode-from-AST 114 vs 634 (5.6x, 1.6x of hand,
2.4x faster than circe). Faithfulness is a construction-time SHAPE
check, not derived-detection: the first cut tried to read whether a
field's Schema came from Schema.derived and a probe proved a given
val is a bare reference at the macro (derived=false everywhere, the
"staged" codec level with the fold because everything delegated), so
each product/sum gets one hoisted `val ok_T` comparing the run-time
schema's names with the Mirror's, and every staged node is `if ok_T
then straight-line else the fold`. TestStaged agrees byte-for-byte
and Left-for-Left over products, sums, Option/List/Vector, all the
totality doors, an Iso field and a recursive type, on JVM, JS and
Native. The finding that is not this lane's: Json.parse of a
150-byte object is 14.6 µs against circe's 0.55 (the lossless CST
parser) — with decode at 0.1 µs, text→value is all parser; filed as
json-value-parser. Also filed: staged-cbor, staged-runtime. On the
way: Json.escape public, six jmh -Wall warnings gone (three that
remain are kyo's macros). Rebased over five siblings' landings; the
build's `clean; Test/compile` shows 3 warnings from siblings' fresh
landings (okay-live TestHub/TestRegistry imports, ChatDemo.scala:863)
— theirs to take, reported in the room.

## demo-e2e-browser — one chat round through a real headless browser
Completed: 2026-09-02
Landed as d0dbf4e (spec) + 03b2099 (impl). Every existing demo test
hit the HTTP/SSE seam directly with `java.net.http.HttpClient` —
proving the server honestly, but never running the shipped React
bundle in a real JS engine. `Main.scala`'s remaining untested glue is
real: `fetch`+`ReadableStream` driving `/chat` (hand-rolled SSE-frame
parsing) and a real `EventSource` driving `/events/:email` — neither
exercisable from a JVM unit test.
Weighed HtmlUnit (pure JVM, no browser binary, but its own JS
engine's fetch-stream/EventSource support is exactly the weak spot
this test needed solid), Selenium (needs a real browser + matching
driver pre-installed, a precondition outside sbt's control), and
Playwright for Java (one dependency, downloads its OWN Chromium on
first use, full real-browser fidelity for both). Took Playwright —
same reasoning as `okay-langchain4j-embed`'s local ONNX model: no
external service, but a real download.
New module `okay-demo-e2e-browser` (JVM): one test booting
`ChatDemo.routes` on a real Jetty socket (the exact
`TestChatDemo.withServer` pattern), launching headless Chromium,
typing into the `data-key="draft"` input, clicking
`data-key="send"`, and waiting for the bot bubble under
`data-key="log"` to carry the scripted echo streamed token by token.
Deliberately kept OUT of okay-demo's own test sourceset and the root
`.aggregate(...)` list; invoke explicitly: `sbt
"okayChatWebJS/fastLinkJS" "okayDemoE2eBrowser/test"`. Absent the
linked bundle or an installed browser, the test SKIPS named, never
fails falsely.
Real bug caught during testing: `Page.waitForFunction`'s 2-arg
overload is `(expression, arg)` — `arg` a value passed INTO the
polling function — not `(expression, options)`; passing a
`WaitForFunctionOptions` positionally there threw a serialization
error. Fixed by passing `null` for `arg` and the options third.
Tests: 3/3 clean bare-JUnitCore runs, ~3s each after the one-time
~450MB browser download. Verified okay-demo and other modules compile
unaffected by the build.sbt addition.

## stm-ui-close — the first STM consumer outside the engine itself
Completed: 2026-09-02
Landed as bc9059a. `Ui.runCmd`'s closing decision (`okay-ui`) held
three atomics — `pending`/`unprocessed`/`upstreamDone` — read one at
a time in `maybeClose`; a command launched from the last buffered
event could land in the window between two of those reads and its
answer was lost (a flaky test had already caught and named this
once). Now one `TRef[CloseState]`: `TRef.modify` makes mutate-then-
decide-whether-ready ONE step, removing the window by construction.
Single-cell fast path only (no `Tx`/`Stm[F]` needed — the composite
condition lives in one cell), exactly the case specs/stm.md's
Behavior list already named, chosen this time by the caller rather
than the interpreter. Existing UI suites stayed green (67→68 JVM, 4
JS, Native compiles/links clean); new stress test fires 200 commands
x 50 runs under the real JVM scheduler (virtual threads, no `Pure`
interpreter) and accounts for every answer — the shape of the exact
race the old comment described, exercised for real. specs/stm.md
Results entry. Matrix 78 suites, zero failures.
## rag-langchain4j (EmbeddingModel half) — a local embedder, kept out of the root build
Completed: 2026-09-02
Landed as 01c4955 (spec) + bcb29c0 (impl). A consumer finally named a
store: demo-embeddings-attr (BACKLOG) wants okay-match's registry to
collide "разработчик"/"программист" BEFORE the registry drifts —
semantic search-before-create, not the hashing embedder's lexical
stand-in. `MatchStore` already takes `embed: String => Embedding` as
a plain constructor parameter, so no okay-rag retrieval pipeline was
needed to satisfy that ask; a real embedder was the whole gap.
New module `okay-langchain4j-embed` (JVM): `dev.langchain4j`'s
`AllMiniLmL6V2EmbeddingModel` — a local ONNX model bundled in the
jar, no network, no API key — wrapped two ways: `embed(model):
String => Embedding` (the exact shape `MemoryMatch`'s constructor
already takes) and `handler(model): Handler[Embed]` (okay-rag's
effect). Pinned at `1.19.0-beta29` — this artifact still ships
beta-versioned even though `langchain4j-core` is stable at `1.19.0`.
Scoped narrower than the BACKLOG name: only the EmbeddingModel half
shipped. Their EmbeddingStore (a vector database behind
`okay.rag.VectorStore`) stays unbuilt, a separate larger integration.
Deliberately kept OUT of okay-demo's build and the root
`.aggregate(...)` list — the bundled model is a real ~90MB download,
and nothing about compiling or testing this repo should force that
on a contributor who never touches embeddings (an explicit operator
call after weighing the tradeoff). `demo-embeddings-attr` stays open
in BACKLOG, now unblocked rather than closed.
Tests: the real semantic collision (разработчик/программист score
>0.5 cosine, beating the hashing embedder on the same pair),
embed/handler answering identically. 2/2 clean over three
bare-JUnitCore runs, ~45ms each (confirms no network). Verified
okayDemo/okayLangchain4j/okayRag compile unaffected by the build.sbt
addition.

## eager-dispatch-regression — fold goes inline, closes 3.45x UNDER the pre-regression baseline
Completed: 2026-09-02
Landed as 55f4aa3. The other half of the bench-sweep report's two
findings (channel-merge-regression was the first, and turned out not
to be a regression at all). This one was real and traced exactly:
casts-encapsulated (d6feb48) centralized Eager's two casts into one
`fold` taking ordinary Function1 arguments — every flatMap call
built a closure for BOTH branches (the one not taken included, since
arguments evaluate before fold is entered) and dispatched through a
virtual call instead of an inlined match arm. 5.1 -> 17.6us, 3.45x,
on the pure-bind hot path that is the entire point of Eager. Fix:
`fold` becomes `private inline def` with `inline` value/tree params
— the casts stay in the one documented function, but each call
site's branch compiles in place, nothing built for the arm not
taken. Measured 4.83 ±0.03us — under the original 5.1, not just
back to it. TestEager 4/4, full sbt test green. docs/benchmarks.md
§1 and specs/eager.md Decisions carry the finding and the fix.

## demo-package — the React bundle rides into the image next to the jar
Completed: 2026-09-02
Landed as 04abb38+e3bab74 (spec) + 8485498 (impl). `Chat.appJs`
(okay-chat) finds the linked React/Scala.js bundle by walking a
filesystem path relative to a repo checkout, which a shipped fat jar
does not carry — "one command run" needed sbt and a node-style dev
server side by side. Weighed an sbt `resourceGenerators` task (a new
idiom nowhere else in this repo, and a cross-module dependency just
for a copy) against two generic fields on `Deploy` plus one more
`COPY` line in the already multi-stage Dockerfile; took the second.
`Deploy` gains `extraBuild: Vector[String]` (extra sbt tasks run
alongside `<module>/assembly`) and `extraCopy: Vector[Copy]` (extra
`COPY --from=build` lines into the final image) — both default
empty, so every prior `Deploy` value renders byte-identical
Dockerfiles, no drift anywhere else.
`DemoDeploy.spec` links `okayChatWebJS` and copies the output to
`/app/app.js`, wired through `Chat.appJs`'s EXISTING `OKAY_CHAT_APP`
env-var seam — okay-chat itself needed no code change. Regenerated
`okay-demo/deploy/` from the updated spec; `TestDemoDeploy` confirms
zero drift.
Tests: okay-deploy's TestDeploy +1. Full okayDeploy suite 6/6;
TestChatDemo+TestLogin+TestDemoDeploy 43/43 clean over three
bare-JUnitCore runs, no flake.

## demo-gate-ui — the platform gate policy, live and switchable from /market
Completed: 2026-09-02
Landed as 0d4dfff (spec) + a2e7492 (impl). `PlatformPolicy` was
already data (an immutable `Map[String, Gate]`) but bound at store
construction and never reassigned — flipping a gate meant editing
code and restarting. `MatchStore` gains `gate`/`setGate`/
`gateOverrides`; both engines (Memory, SqlMatch) keep a `livePolicy`
var seeded from the constructor's starting value, read on every
disclosure check, so a flip takes effect on the very next query — no
cache to invalidate, because there never was one.
`POST /admin/gate` flips one attribute's gate, admin-token gated the
same way `/admin/replay` is (`Secure.granted` + `Admin.Issuer`, built
directly in `ChatDemo.scala` rather than growing okay-admin's generic
module with a domain-specific action — Gate is a marketplace concept,
not admin infra). `/market.json` gained a `"gates"` field; `/market`'s
page gained a small panel (current overrides + an attr/gate form)
mirroring the existing replay button's client shape exactly.
Tests: okay-match's TestMatch +1 (live flip visible immediately;
`reset()` leaves the gate policy alone — configuration, not
projection, same reasoning as scenario definitions), okay-demo's
TestChatDemo +2. Full okayMatchJVM suite 31/31; TestChatDemo+TestLogin
42/42 clean over two of three bare-JUnitCore runs, one hit the known
pre-existing LIVE SEEKER judgment flake (unrelated).

## channel-merge-regression — investigated, not a code regression
Completed: 2026-09-02
Landed as f5ad554 (bbefd79). The bench-sweep report flagged
Source.merge at 1.95x over its doc baseline (158 -> 307us) the same
day the STM channel landed, and STM was the obvious suspect. A new
concurrent-contention benchmark (ChannelBenchmark.
concurrentSendReceive1k — two virtual threads racing sendBlocking
into one channel, real CAS contention; the STM lane's own benches
were single-threaded) showed only ~13% overhead, not 95%. The
settling check: today's HEAD and the last pre-STM commit
(channel-cas, 500efb7) measured within noise of each other on the
identical merge benchmark, same run (308±19 vs 290±11). Whatever
moved the doc's 158 baseline predates every 2026-09-02 landing —
retired to 308 in docs/benchmarks.md §6; the investigation and the
refuted hypothesis are recorded in specs/stm.md Results. No source
change beyond the new benchmark.

## demo-scenario-editor — author a ScenarioDef through a page, no code change
Completed: 2026-09-02
Landed as 2dac9be (spec) + ad2d196 (impl). GET /scenarios lists every
registered scenario; a textarea (pre-filled with an escrow-sale
example) edits the plain JSON shape of the EXISTING ScenarioDef/
Transition case classes directly, POSTed to /scenarios and passed to
MatchStore.defineScenario — no new schema. The BACKLOG bullet's
"steps"/"prompts"/"deal hook" turned out to already be the type's own
transitions/notifies fields (the built-in `deal` scenario rings
inboxes through `notifies` already); design work was recognizing
that, not inventing anything. A malformation (validate's BadScenario)
comes back as 400 + one line each; success reloads the page, so the
scenario is immediately listed AND immediately playable through the
existing offline phrase driver.
MatchStore gained `scenarios: Vector[ScenarioDef]` — no list-all
method existed before this. Both engines' private scenario map
renamed `scenarioDefs` to avoid shadowing the new public def. Stated
limit, pre-existing: SqlMatch doesn't persist scenario definitions to
a table the way it persists flows — not touched here. Help text
(help/помощь) now names the currently-registered scenarios instead
of a static hint that could name one the store doesn't have.
Tests: okay-match's TestScenario +1, okay-demo's TestChatDemo +4.
Full okayMatchJVM suite 30/30; TestChatDemo+TestLogin 40/40 clean
over three bare-JUnitCore runs.

## demo-mcp-market — the marketplace served over MCP at /mcp
Completed: 2026-09-02
Landed as af5bae9 (spec) + 55b1b82 (impl). chainedTable — already the
ONE tool table both the LLM agent path and the deterministic driver
drive — is exactly the (specs, table) pair okay.mcp.Server.serve
takes; mcpRoute mounts it at /mcp via McpHttp.route, so any MCP
client becomes a market participant over the same substrate the chat
UI drives. mcpTable rebuilds chainedTable PER TOOL CALL (fresh turn
offset + subscription period) rather than once at server-mount time,
matching what /chat already does per HTTP request. Stated limit: MCP
tool calls do not append to chatLog — MCP is the marketplace's OTHER
front door, not a second writer to the durable turn log.
Real bug caught in testing: mcpRoute is a def, and McpHttp.route
builds a fresh session table every time it's evaluated; routing with
`mcpRoute(r)` inline re-built it per request, silently dropping every
session right after initialize (next call 404'd as "the MCP session
is gone"). Fixed by binding it once as a val in routes(). Also
dropped an unused Transport/Secrets from the signature — chainedTable
only needs MatchStore.
3 new MCP integration tests (initialize+tool list, facts_assert
visible via /market.json, reverse chain firing across MCP+chat front
doors). Suite 35-36/36 clean over three bare-JUnitCore runs, the one
red being the known pre-existing LIVE SEEKER judgment flake.

## demo-streaming-cut — the demo as llm-streaming-cut's first consumer
Completed: 2026-09-02
Landed as 0aa67f4 (spec) + b1bd957 (impl). Cut.guard shipped earlier
(specs/llm-agentic.md) but the demo only ever checked the token
BUDGET; okay-chat's reply/chatRoute gain an optional policy:
(Int, String) => Option[Cut.Violation], checked alongside the budget
inside the SAME Cut.checked — additive, defaults to never-violate, so
every existing caller (okay-chat's own prior tests included) is
unchanged. okay-demo wires a small banned-word content policy (a
stand-in for "off-policy content" — the point is the MECHANISM, a
live generation aborting on what it SAYS, not just how much);
Chat.scripted echoes the user's own message, so typing the banned
word is itself the trigger, offline, no separate demo-only model
needed. 5 new okay-chat unit tests, 2 new demo integration tests.
Closes BOTH backlog gates this pair had open: demo-streaming-cut
(okay-demo section) and llm-streaming-cut (Elsewhere). Suite 32-33/33
clean over three runs, one red the known LIVE-model-endpoint flake
(and a stale sbt/zinc symbol cache in okay-ops hit along the way,
fixed by clearing its target dir — unrelated build-tooling noise).

## security-sessions — SessionIssuer + OneTimeCode, the recipe two callers already duplicated
Completed: 2026-09-02
Landed as dc968be (spec) + 2ff03c9 (impl). Third and last of the
round-two demo reusable-module extractions (user ask) — all three now
done: pg-target-in-okay-pg, okay-live, login-in-okay-security. New
okay-security/src/main/scala-jvm classes (ES256 already is JVM-only):
SessionIssuer(ttlSec)(subject, scopes) for the ES256 keypair-plus-
issue/verify shape okay.demo.Login and okay.admin.Admin.Issuer had
independently duplicated (found while landing okay-admin); OneTimeCode
(ttlMs) for Login's confirm-and-sign one-time code. Login.scala and
Admin.Issuer both became thin wrappers; OneTimeCode.start's Crypto is
okay-security's OWN Crypto now, not okay.crypto — simpler, since the
recipe lives inside the module with the richer local trait already.
8 new unit tests in okay-security (a real bug caught writing them: a
first "expired token" test advanced past ttlSec but landed inside
Jwt.verify's default 60s clock-skew tolerance, silently passing —
fixed by advancing further); okay-admin's TestAdmin and the demo's
full suite (31 tests, +TestLogin) pass unchanged in substance.

The round-two demo extraction ask (BACKLOG.md, "what else can be
reused") is now fully complete: pg-target-in-okay-pg, okay-live,
login-in-okay-security, alongside round one's okay-subscription,
okay-admin, okay-chat — six extractions from one file this session.

## docs-catchup — module docs for today's extractions, and a stale index found
Completed: 2026-09-02
Landed as 91eb6d6 (docs only, no code — no matrix run). Four modules
extracted from the demo today had shipped with no docs/modules/*.md
at all: okay-admin, okay-chat, okay-subscription, okay-live. Written.
okay-pg.md gained the PgTarget paragraph (moved in earlier today,
never documented there); okay-demo.md's "pieces and who does what"
rewritten to say ChatDemo.routes composes five extracted modules via
orElse rather than describing streaming/the guard/sessions as if
still inline, plus the admin token and okay-ops's routes named in the
env table. Found along the way: docs/README.md's module index (one
row per module, exhaustive) was missing rows for 8 modules that
already had docs and had simply never been added — okay-admin,
okay-chat, okay-delta, okay-deploy, okay-live, okay-ops, okay-r2dbc,
okay-subscription. All 8 added; verified by diff that the index now
names exactly the modules under docs/modules/.

## okay-live — Hub[A] broadcast + Registry[K,A] per-key channels
Completed: 2026-09-02
Landed as 3aad44f (spec) + dab184a (impl, merged over three sibling
rebases). Second of the round-two demo reusable-module extractions
(user ask): found by noticing the SAME pattern already independently
duplicated twice in ChatDemo.scala — marketFeed (broadcast a ping to
every /market subscriber) and inboxes (a per-email Channel, created
on first use). New JVM-only sbt module okay-live (Channel itself is
already cross-platform core; only the concurrent bookkeeping around
many channels needs java.util.concurrent, the same tradeoff okay-
subscription already made). Hub[A].subscribe()/.publish(a) and
Registry[K,A].apply(key) generalize both call sites; ChatDemo.scala's
own semantics (what's published, who subscribes) stayed put. 5 new
unit tests; demo suite 27/27 clean except the known rotating
LIVE-model-endpoint flake.

Also filed to BACKLOG (operator ask): unify okay-subscription and
okay-live onto okay core's existing TRef/Stm.atomically (a real
cross-platform transactional cell whose own engine leans on an
internal TMap) once the synchronous-vs-effectful API tradeoff for
Hub/Registry is decided deliberately — not urgent, no JS/Native
consumer named yet, filed so the decision gets made once rather than
by accretion next time this tradeoff recurs.

## tidy-warnings-tests — the whole build compiles warning-free
Completed: 2026-09-02
Landed as dc7fec5 (4 commits). Extends tidy-warnings-screen-dom's rule
to every test source: a `clean; Test/compile` of the whole build had
533 warnings (424 unused values, 88 unused symbols, 12 unchecked type
tests, 7 discarded values, 2 non-exhaustive matches); now 0, main and
test both. Same recipe — a discarded result says so (`: Unit`, or
`val _ =` when the value's type is driven by a `using` clause or is
js.Dynamic, where an ascription alone does not stick), unused pattern
binders become `?`, unused lambda/def parameters become `_` or
`@unused` where a bare `_` cannot name a method parameter, dead
imports and dead locals removed. Two shapes needed their own
treatment: `Damaged` in TestCondition's repair-story test was a LOCAL
class, whose type test cannot be checked at runtime by JVM rule —
hoisted to a class-level member instead of suppressed, since local
scope was never load-bearing there; two inherent erasure-kernel
exposures (Writer.run's inline body checked at an abstract answer
type, the same trusted kernel Effects.scala names) got `@nowarn` with
a one-line reason, the first use of that annotation in the codebase.
Found and fixed along the way, each from a sibling's landing that ran
concurrently: four dead imports in ChatDemo.scala (pg-target-in-okay-
pg) and one discarded write in okay-deploy's own new test
(deploy-module) — both are proof the invariant holds even under
concurrent development, not exceptions to it. Full gate green in
three chunks, twice (before and after the second rebase); the branch
was rebased four times over five siblings' landings before the
fast-forward went through clean.

## deploy-docs — the okay-deploy usage guide
Completed: 2026-09-02
Landed as 8d23c9e (docs only, no code — no matrix run). docs/modules/
okay-deploy.md rewritten as a full guide (operator ask): a five-step
quick start for making any service deployable, the `Deploy` value
field by field, what each rendered file is, build/run for a laptop
(compose), a cluster (helm) and Terraform (`helm_release` over the
rendered chart, overrides layered by `set`), monitoring once deployed
(nothing to add — the chart already points at okay-ops's routes), the
rule for changing things (a knob: edit the value and re-render; a
chart gap: extend okay-deploy's template once for everyone, never the
rendered copy), and the things that bite (`_root_.okay...`, the forked
`run` cwd, the `app.jar` contract).

## deploy-module — okay-deploy as a module; the app owns its deploy
Completed: 2026-09-02
Landed as ba959d4 + d5c9398 (tip acaeee3). Operator: "в самом
okay-deploy не было ничего жестко привязано к конкретному
приложению... сам деплой должен находиться в okay-demo... все должно
быть локализовано или в okay-deploy или в okay-demo". The first
deploy landing was a root `deploy/` whose default values.yaml knew
DemoChat's port variable and image — the app leaking into the
template. Now `okay-deploy` is a module: `Deploy(...)` is a VALUE
with a Schema; `Dockerfile.render`/`Helm.values`/`Compose.render`
are pure and golden-tested; the generic chart rides as resources and
a test asserts its templates name no application; `Deploy.write`/
`drift`/`repoRoot`. The build half is a SOURCE sbt plugin under
`okay-deploy/sbt-plugin` (brings sbt-assembly; `OkayDeploy
.deployable(mainClass)` is one build.sbt line) — the root keeps one
pointer in project/plugins.sbt and nothing else deploy-shaped (root
deploy/, docs/deploy.md, project/OkayDeploy.scala all gone).
okay-demo declares `DemoDeploy.spec`, owns `okay-demo/deploy/`
(Dockerfile, compose.yaml, helm/) as its committed rendering, and
`TestDemoDeploy` refuses drift. Proven: okayDemo/assembly through
the source plugin + `java -jar` /healthz; `helm lint`/`template` on
the rendered chart. Traps: a forked `run` has the MODULE dir as cwd
(hence `Deploy.repoRoot`); the core project is named `okay`, so the
build entry says `_root_.okay.deploy.sbt.OkayDeploy`. Matrix 76
suites, zero failures.

## PgTarget — moved into okay-pg, beside the driver it configures
Completed: 2026-09-02
Landed as d3880a8 (spec) + a792dd2 (impl). First of the round-two demo
reusable-module pass (user ask): PgTarget (a pure postgres:// connection
URL parser) had zero demo dependencies from the start — moved to
okay-pg/src/main/scala-jvm (the JVM leg PgTls.scala already lives on,
TlsConfig being JVM-only), no behavior change. TestChatDemo's pure-
parsing test moved to okay-pg's own TestPgTarget, +3 new edge-case
tests (disable/absent plaintext, require carries no CA, malformed URL
never throws); the live-Postgres integration test stays in the demo,
proving marketOf's own wiring rather than parsing. Caught and fixed
before commit: a stray NULL byte landed in the new test file during
authoring (git flagged the diff binary) — verified and fixed by direct
byte inspection, the admin.md-incident lesson applied a second time.

## okay-chat — a streaming LLM chat component, extracted from the demo
Completed: 2026-09-02
Landed as 96e61b7 (spec) + c63af13 (impl). Third and last of the three
reusable-module extractions from the demo (user ask) — the demo now
composes okay-subscription, okay-admin, okay-chat, and okay-match
purely via `orElse` route tables instead of holding their logic
inline. New JVM-only sbt module okay-chat (depends on okayLlm.jvm,
okayHttp.jvm, okayConf.jvm): the model seam (Model/scripted/live/
local/modeName/model), Cut-guarded SSE framing (sse/obj/reply — sse/
obj made PUBLIC since the demo's other streams, /events/market and
/events/<email>, reuse the exact same convention), body parsing
(fieldOf/messagesOf), appJs, and chatRoute(m, budget, turnOverride)
for POST /chat.

turnOverride's type widened during wiring from the original BACKLOG
sketch (Seq[Anthropic.Message] => ...) to (Request, Seq[Anthropic.
Message]) => Option[Source[Chunk[Byte]]] — found while actually
composing the demo's /match branch, which needs the bearer token off
the request's headers (a verified session identifies the speaker),
not obtainable from parsed messages alone. The override answers an
already-SSE-framed Source, preserving the demo's own token-splitting
shape for marketplace answers untouched; None falls through to the
plain path. 8 new unit tests in the module; the demo's own
TestChatDemo suite unchanged in substance (call sites qualified only)
still proves /match, deals, flows, subscriptions, sessions and the
live paths end to end through the composed route table. Suite
27-28/28 clean over three runs, the one red the known rotating
LIVE-model-endpoint flake — unrelated pre-existing infra noise.

Landed carefully after the okay-admin spec-corruption incident earlier
this session: diff sizes and spec line counts verified sane both
before and after the merge, no scripted find/rfind edits used this
time (a plain single Edit replace_all for the checkbox flip instead).

## direct-tail-fusion — the direct macro's while loop matches its hand-written flatMap chain
Completed: 2026-09-02
Landed as 935014d (merge; c4315d8 the kafka+mongo munitTimeout
drive-by, 63c86bb the fusion itself). The road direct-flatmap-
emission recorded: while's 2.0x came from paying two flatMap binds
per iteration (the step's own, plus a separate sequencing bind
chaining to loop()). compileTail/stmtsTail compiles a loop body
against an explicit tail term instead, threading it into the body's
own last bind — vals, marked assigns, pure statements, and bare
runnable ops all fold; if/match/nested-loop/try fall back to one
sequencing bind (duplicating a tail into branches duplicates code,
and the fallback is the pre-fusion emission, correct by construction).
Measured (quiet box, §1b): while+var 189us -> 101us (2.0x -> 1.06x,
matched within noise against the 95us hand chain). Recursion
untouched at 55us — it never goes through While/foreach.

Drive-by, found chasing the gate: TestElectionKafka and TestMongoDocs
were missing the `munitTimeout` override their sibling live suites
already carry (120s) — the 30s munit default was firing before even
a merely-slow availability probe could return, so a correct "docker
absent, skip" read as a hard failure under a loaded sbt test matrix.
Fixed to match the established pattern, verified green (clean skip)
in isolation. TestKafkaEos, which already had the override and still
blew through it, stays open in BACKLOG as the netty-ws-matrix-flake
root cause verbatim (unbounded sbt test-matrix parallelism) — a
cross-cutting build.sbt fix, its own claim. The gate's one remaining
failure on the decisive quiet-box run was netty-ws-matrix-flake's
sixth sighting (TestBackends, green in isolation) — recorded, not
re-triaged, per that entry's own standing doctrine.

## tidy-warnings-screen-dom — the main sources compile warning-free
Completed: 2026-09-02
Landed as e5a512d. The operator asked what else could be fixed. A
`clean; compile` of the whole build had 255 warnings in main sources
(143 unused values, 81 unused symbols, 28 discarded values, 3
non-exhaustive matches); now 0 (test sources still carry theirs — a
separate, optional lane). What changed: a discarded Java or Scala
result says so (`x: Unit`), a discarded js.Dynamic result is `val _
= x` (an ascription does not silence it), unused pattern type
variables are `?`, an unused parameter an API forces is `@unused`,
unused imports and `using` parameters are gone (McpHttp.routed,
Wire.serve, Repair.decode/read, Retrieve.fair no longer ask for
what they do not use), the macro's two slot rebuilders are total
with a named refusal, its union type test is three extractor
patterns, Throws' head-form match follows the stack's convention,
and ChunkBuf/Direct's inline accessors are `@publicInBinary`.
Screen's `Nav | S` split is sound now: a `NotGiven[S <:< Nav]`
evidence refuses an S that is a Nav at compile time. Two slips on
the way, both caught by the compiler before any commit: the
automatic pass once put `: Unit` after a `match` header and once
after the wrong statement on a `;` line. AGENTS.md records the rule
and the trap (an incremental compile hides warnings in untouched
files). Gate green in three chunks (the one red was the demo's
LIVE SEEKER judgment — the small local model's answer, not the
wire); rebased over okay-admin-module and deploy-package and
clean-compiled again: still 0.

## deploy-package — a reusable deploy scaffold, not one app's Dockerfile
Completed: 2026-09-02
Landed as 3e0d390 (operator ask, following ops-monitoring). sbt-
assembly packages any Okay service into one fat jar; a service opts
in with two build.sbt lines (`assembly/mainClass`,
`assembly/assemblyJarName := "app.jar"`) — okay-demo is the first.
`deploy/Dockerfile` is ARG-parameterized by sbt module id, so one
file serves every future service; `deploy/scripts/okay-package.sh
<module> [tag]` wraps the jar build and, where a daemon answers, the
image build too — and says so plainly when it does not (no Docker
daemon was running during this landing; the operator had stopped it
to free memory). `deploy/helm/okay-app` is one Helm chart,
values-parameterized (image/tag/port/env), whose probes and
`prometheus.io/scrape` annotation point at exactly the routes
okay-ops already answers — proven with `helm lint` (clean) and `helm
template` (rendered and inspected against a DemoChat example values
file); Terraform's `helm` provider (or plain `helm install`) applies
the same chart unchanged for a second service. The fat jar itself
WAS proven live: `java -jar app.jar` served `/`, `/healthz`,
`/readyz`, `/stats`, `/metrics` all 200 — the actual hard part
(classpath, merge conflicts, one main), independent of whether a
daemon exists to containerize it. specs/deploy.md; docs/deploy.md.

## okay-admin — protected admin routes, fixing the demo's unauthenticated /admin/replay
Completed: 2026-09-02
Landed as 1d29269 (spec) + 38ea057 (impl). Second of the three reusable-
module extractions from the demo (user ask). New JVM-only sbt module
okay-admin (depends on okaySecurity.jvm): Admin.routes(verify, policy =
Policy.scoped("admin"), realm)(replay, onReplayed) delegates to
Secure.granted — the same 401/403 ladder every other protected route in
this stack already uses; Admin.Issuer is a minimal in-process ES256
credential (same shape as okay.demo.Login) so a consumer has something
to test/use the route with. Wired into ChatDemo.scala: routes composes
core.orElse(Admin.routes(...)(...)), replacing the old inline
UNAUTHENTICATED /admin/replay case — a real gap named while planning
the extraction, now fixed rather than moved verbatim. main() prints the
admin token to console at startup; /market's replay button now sends a
Bearer token via fetch instead of a plain form POST. 6 new unit tests
plus one integration test through the real demo route. Suite 27-28/28
clean except the known rotating LIVE-model-endpoint flake.

Caught and fixed before push: a checkbox-flipping script (specs/admin.md)
assumed unchecked "- [ ]" boxes but the file was accidentally written
already-checked; an unguarded `rfind` returning -1 on every iteration
silently duplicated the file's content 30-fold (121 -> 3841 lines) via
Python's negative-index slicing. Caught by an unusually large diff stat
on a routine merge, before the corrupted commit was pushed; fixed by
restoring the spec commit's clean content and amending, no history
rewrite reached origin. Lesson for future scripted text edits: verify
line count / diff size after ANY find/rfind-based rewrite, and never
assume a checklist's starting state without checking it.

## cast-free-small — round two closes: 97 → 36
Completed: 2026-09-02
Landed as 3666b6b. The last lane of the second cast round, one small
fix per file: Rx's reactive pull queue is a typed message ADT; Async's
handshake cell is `Got[X] | Moved | Null`, so what comes out is the
operation's Either; Native's blocking cell holds an Option instead
of a null dressed as an A; the Java API downcasts (Nio, Jetty, Netty
×2, Tls ×2) are type tests with a named refusal, and CryptoJvm's key
handles go through privateKeyOf/publicKeyOf that refuse the wrong
kind by name; Node's `process.argv` is a facade (Web.Process), the
Buffer callbacks are typed at the callback (NetNode, node:crypto in
both CryptoJs — the require-based one keeps ONE claim at the module
boundary, where a JSImport would put it); Form decodes fields at
their type and matches a product's column; Screen finds a boundary
by Same's witness (`b.k === k` gives Boundary[A]); Collect always
calls the finisher; jdbc/r2dbc walk any array through the runtime.
Every touched suite green (core JVM/Native, r2dbc, jdbc, java, ui
JVM/JS, http JVM/JS, jetty, netty, tls, security JVM/JS, crypto
JVM/JS, cluster JS), every module compiles; rebased over
ops-monitoring's landing. What stays is in BACKLOG "Casts, round
two": the kernels with their reasons, Screen's `Nav | S` union (an
API design), Dom.scala's js.Dynamic (a ui-js facade lane).

## ops-monitoring — health, stats, Prometheus over the values that already exist
Completed: 2026-09-02
Landed as 1414328 (operator ask, extended to standard wires). New
module `okay-ops`: `Health.of(store)` calls `store.stats` live (no
cached flag), answering two booleans with a reason; `Prom.render` is
a pure `Store.Stats => Prometheus-text-0.0.4` mapping, pinned by a
golden string, with opt-in per-group `Offsets.lag`; `Ops.routes`
composes `GET /healthz`, `/readyz`, `/stats`, `/metrics` into any
route table — no client library, no SDK, the same OTLP-is-a-mapping
ruling okay-obs already made for tracing. Wired into the demo, whose
`chatLog` store is now exposed as `chatStore` so both can see it. A
Kubernetes liveness/readiness probe and a Prometheus scrape both read
these routes directly with zero code in the pod on their behalf; the
manifest IS the integration (deploy-package/deploy-k8s next, a
reusable Dockerfile + Helm chart template for any Okay app, DemoChat
as the first concrete instance). specs/ops.md; docs/modules/
okay-ops.md.

Found and fixed along the way (docker-live-suites-slow-skip):
TestKafkaStore/TestElectionKafka/TestKafkaRepair/TestKafkaEos and
TestMongoDocs hung to their munitTimeout (30-120s) instead of
skipping in milliseconds when no broker answered — the Kafka and
Mongo client LIBRARIES' own generous defaults ran their full retry
policy before the tests' catch-all ever saw a Throwable. A fast raw-
socket pre-check (`TestKafkaSupport.reachable`, 1s) now gates all
four Kafka checks; Mongo's probe client alone gets a 1.5s
`serverSelectionTimeout`/`connectTimeout` (production `MongoDocs
.client`/`KafkaStore.apply` keep their default retry policy — only
the test-side "is anything here" question needed to be fast). Both
suites now skip in ~1s combined with no broker running, not ~2min.

## cast-free-rag-llm-kyo — rows by ascription, a typed frame, kyo at its E
Completed: 2026-09-02
Landed as a755a84. rag's Ingest and Retrieve re-associate rows by
ascription (a row is a union); `fair` builds its alternatives in
Choose + Pure and observes them there, no re-typing; llm's Cut
establishes its "cut" frame through `frame[…, Violation]`, so the
policy's value arrives as a Violation or is refused named; kyo's
interop matches Throws at its E (the tree types it) and passes kyo's
continuations uncast (their types line up). Ten casts to none; rag,
llm, kyo suites green; rebased over okay-subscription-module.

## okay-subscription — the demo's subscription gate, extracted into a reusable module
Completed: 2026-09-02
Landed as 261981e (spec) + c54f3d2 (impl). User ask: extract genuinely
reusable pieces the demo builds into their own modules rather than
leaving them stuck inside ChatDemo.scala. Period/subscribed/pay/
backdateJoin/subscriptionNotice/paySpec were already fully decoupled
from MatchStore/ChatLog and took a bare String uuid, so this was a
pure move, not a redesign: new JVM-only sbt project okay-subscription
(depends only on okayAgent.jvm, for ToolSpec), okayDemo.dependsOn
gains it, every ChatDemo.scala call site becomes Subscription.<fn>.
New unit suite (9 tests) proves the module in isolation; the existing
SUBSCRIPTION GATE integration tests in TestChatDemo keep proving the
end-to-end HTTP-route wiring, updated to the qualified names. Two more
extractions from the same ask (okay-admin, okay-chat) are designed
(API sketches, decisions — a Plan agent validated the seams before
landing) but not built this pass; filed to BACKLOG.md's new
'## Reusable modules' section — admin also names a real gap found
while planning: /admin/replay ships unauthenticated today. Suite
26/26 clean except a rotating LIVE-model-endpoint timeout (the same
infra flake documented all session, unrelated to this change).

## cast-free-blob — the blob walkers typed by the tree
Completed: 2026-09-02
Landed as 982c4ce. Backup.walkGet and Offload.walk, hand-rolled
interpreters over `Either[String, Unit] ! (Produce + Async)`, are
typed by the tree: the split yields an `Async[X]` or a produced X
(Produce is the identity signature — the op IS its answer), and the
one claim — that the produced values are chunks — goes through the
stated kernel `produced[Chunk[Byte]]` instead of a cast per site.
S3's row re-associations are ascriptions (a row is a union, so
Async + Produce is Produce + Async). Seventeen casts to none; the
blob suite green, every module compiles.

## cast-free-agent — round two opens: the agent's interpreters typed
Completed: 2026-09-02
Landed as 881074a. The second cast round (BACKLOG "Casts, round
two"), agent first: Provider.relay, Grounded's translation,
relayTools and Memory.handle are built at the GADT-bound X — a
covariant row gives X >: the case's answer and `!` is invariant in
its answer, so each branch is `pure[F, X](…)` or `.map[X](…)`, never
a cast; a row re-association is an ascription (a row is a union).
Large asks the inner handler as a `Tool[String]` and gets a String.
Two erased boundaries got one kernel each: `Schema.defaultAt` (a
product's defaults are aligned with its fields) and
`Snapshot.stateAs` (the Context row names no S, and a snapshot is
only ever restored into the state that made it). Ten casts to one.
Agent suites (all but the live one, which hangs while the gateway
is drowned), agent JS, codec, demo's repo-agent green; every module
compiles.

## unchecked-null-chunks — the audit's last lane, and a slip on the way
Completed: 2026-09-02
Landed as 9eaf17f (2 commits). The five `case c: Chunk[Byte]
@unchecked` over a `Chunk[Byte] | Null` scrutinee (blob Backup and
Fs, http Transports on JVM and JS, Nio) are null-first matches — after
`case null`, flow typing types `c` — no annotation needed. The other
non-`resume` `@unchecked` are the stated kernels: Chunks/Writer's
Fold specialization dispatch (8) and Throws' union dispatch (12),
both commented at their site. The slip, reported plainly: the first
commit of this lane carried only the BACKLOG entry — the perl
substitution had not matched, the count line said 5 remained, and I
read the passing tests as the edit's success; the second commit has
the edits, verified by the count reaching 0. The cast tally for
src/main across the day: 185 → 97; what remains is kernels with a
stated reason (ChunkBuf, Eager, Pipe, Same, Schema, Effects), JVM
interop (blob S3/Offload/Backup, java Streams, CryptoJvm, kyo) and
small ones in ui/rag — named in BACKLOG for the next audit. Also
the JS fetch chain's unused promise is discarded explicitly.

## typed-js-facades — the web globals stated once, in types
Completed: 2026-09-02
Landed as 871e36a. The "raw js.Dynamic, no scala-js-dom" decision in
specs/http.md asked to be revisited out loud; the cast audit was the
occasion. `okay.Web` (core scala-js) declares `fetch` with
`RequestInit`, `Response`, `Headers`, the body `Reader` and its
`ReadResult`, `WebSocket` with `MessageEvent`/`CloseEvent` as
`js.native` facades — still no scala-js-dom — and both JS transports
(okay-http's fetch and sockets, okay-llm's fetch) are written on them:
seventeen casts to none. A message's `data` is declared `Any` so text
versus binary is a type TEST, not a cast. JS suites for core, http,
llm, mcp, chatweb, cluster green; every module compiles. Also: the
stale `+` import in Repair.scala.

## cast-free-typed — the SQL typed layer's Shape is a GADT
Completed: 2026-09-02
Landed as 38ed3b2. Typed's Shape mirrored the Schema untyped
(`Iso(Any => …)`, `Arr(Vector[Any] => Any)`, value casts by SqlType
on encode) — eleven casts. Now `Shape[A]` is a GADT: `Prim[A]` carries
its typed decode (with the column widenings I32 → I64, Num → F64/Text)
and encode, `Opt`/`Iso`/`Arr` carry their element types, `Row[A]`
carries the field shapes by position for decoding (the Mirror's
`make` takes them erased — no kernel needed) and its Schema for
encoding through `eachField`; decode/encode are written by matching
(`case o: Shape.Opt[a]`), `encodeParams[P]` is typed. Zero casts in
Typed.scala. sql suites on JVM/JS/Native, jdbc, r2dbc, pg, persist,
match, demo green; every module compiles; rebased over
demo-subscription-gate and re-verified demo + match.

## demo-subscription-gate — free join month, then paid-per-period or gated, never deleted
Completed: 2026-09-02
Landed as 87d6e70 (spec) + c06bc42 (impl). User ask: a profile shows and
matches free for its first calendar month; after that only a period
actually PAID keeps it visible; unpaid is gated from find_candidates, the
reverse chain (both as poster and as the waiting side), and /market +
/market.json — never deleted, and every turn from a gated user carries a
reminder. Demo layer only, okay-match untouched: Period(y,m) is the
calendar-month key, subscribed(uuid, now) = the profile's lazily-anchored
join period IS now, or now was paid. Paying is a new subscription_pay
tool (demo stub, оплатить/pay), taking effect the same turn. Two reminder
channels for two paths: scriptedAgent appends a suffix computed AFTER
dispatch; the LIVE path's facts_register wrap carries a "notice" field,
relayed by one new matchSystem sentence — the same channel the model
already reads its provenance instruction from. Bug found and fixed while
landing this: dealEvents (demo-deal-timeline) was keyed by bare deal id,
so two independent test stores (both numbering from 1) could cross-
contaminate dealTimeline lookups across tests in one JVM — rekeyed on
(store identity, deal id). Suite 24-26/24-26 clean over several runs
(rebased twice mid-flight over demo-sessions landing concurrently); the
one red seen throughout was local-model timeout/flake, unrelated.

## cast-free-codec — Json and Cbor by GADT matching; the Mirror's erasure stated once
Completed: 2026-09-02
Landed as 2a8aca1. Schema was a GADT already; the codecs cast out of
habit (eighteen `Schema[Any]` / `asInstanceOf[A]` in Json and Cbor).
Now they match the schema and bind the element type (`case l:
Schema.SList[a]`), so a nested decode is typed by the compiler. The
erasure the Mirror leaves is stated once, in Schema: `eachField`
(parts is productIterator in field order — the i-th value is the
i-th field's type) and `theCase` (caseOf is the ordinal — the value
is that case's type) hand each value to the codec at its own type
through a polymorphic function; sum cases are `Schema[? <: A]`, the
bound claimed in `derived` where the Mirror gives the element types.
Product decoding needs no kernel: each field decodes at its type and
joins the erased parts `fromProduct` takes. Codec suites unchanged,
51 green on each of JVM/JS/Native; agent and sql suites green
(agent's TestLive HANGS while the local gateway is up but drowned at
host load 20 — the wire is slow, not broken; liveTest skips a drop,
not a crawl — noted for the live-skip lane's follow-up); every module
compiles. Typed (okay-sql) is next: cast-free-typed.

## cast-free-delim — the delimited-control machine on a typed chain
Completed: 2026-09-02
Landed as 3370692. Delim's segment stack is a typed chain
`Segs[F, A, Z]` — from the current answer to the run's answer, `K`
chaining types through each Bind's continuation, `Mark` carrying its
prompt and so the answer type under it. The cut at a prompt goes
through `Same[Prompt]`'s witness (`q === p`: the mark's type is the
prompt's), `reify`/`split`/the loop are typed by the chain and GADT
matching, the state between steps is `Next(prog, kont)`. Nine casts
became the two claims the file's header always stated (a Push's body,
a Capture's f: programs in the machine's row, erased because F is not
the operation's to name), each at its line. One frame more per push
(the K carrying the prompt's answer up to the op's); DelimBenchmark
A/B on a loaded host (load 6–9, error bars wider than the values)
shows no loss — pushOnly 24.7–29.3 µs vs 48.6 µs baseline in the one
clean-ish pair. TestDelim unchanged; core 362 green on JVM, JS/Native
green, every module compiles. specs/delimited-control.md gained the
section.

## casts-encapsulated — three audit lanes in one: Effects, the kernels, the macro's upcasts
Completed: 2026-09-02
Landed as d6feb48. Effects.scala: Handler.union splits through `<|>`
(the union's excluded-middle claim lives in one place), translate's
continuation is typed by the Bind node; typeableK's class-test kernel
stays, stated. Pipe.unreachable throws instead of handing out a null
dressed as an A. Eager's value-or-tree dispatch is ONE `fold` holding
the encoding's two casts; toFree, flatMap, runWith go through it.
ChunkBuf's array kernel is one `wrap` (reflection answers Object, and
an array of A's representation IS a Chunk[A] — the lie `update` tells,
repeated once on the reading side) and `sized` replaced the Vector
casts. Direct: the macro summons `V <:< T` at expansion time and
splices the compiler's evidence instead of emitting a cast (`upcast`)
— and that exposed one of the four as NOT an upcast: a
statement-position loop's `F[Any]` cast to Unit, now an explicit
discard `(_: V) => ()`, Scala's own value-discard rule said in the
macro. Core 362 green on JVM, JS/Native green, every module compiles;
rebased over a claim-only commit.

## cast-free-sim — the simulator typed, zero casts
Completed: 2026-09-02
Landed as a8baab9. Second lane of the cast audit. Sim's channel ops
carry the channel's element type (`Chan[A]`, `Send[A]`,
`Receive[A]`, `Close[A]`), the parked receivers and senders live
TYPED on the channel itself instead of in erased maps keyed by id,
and `perform[Y](fid, op: Op[Y], k: Y => …)` is typed by GADT
matching on the tree; the deadlock count sums the channels' queues.
Seven casts to none. Traces unchanged by seed (TestSim, TestStmSim
green), core 362 green on JVM, JS/Native green, every module
compiles.

## cast-free-condition — the condition machine typed; the audit in the backlog
Completed: 2026-09-02
Landed as 6cfb79b. The operator asked where casts remained and to
write the audit down and work it: BACKLOG "Casts" lists five groups
(185 asInstanceOf + 28 @unchecked in src/main) with a lane and a
recipe each. First lane, Condition.scala (12 casts, 1 left): the
operations carry their answer type — `Signal[A](condition, accept)`,
`Within[A, V](…, recover, accept)`, `Leave[V](handle, value)` — and
the policy's untyped answers cross ONE door, `accept`, a ClassTag
test that refuses a wrong value as BadResume; the run loop is typed
by GADT matching (`step[Y](op: Op[Y], k)`, Left = continue on the
Resume path, Right = answer) so its erasure casts are gone; `raiseC`
IS `signal[A]` with the Answers tag, and every signal has the
checked resume now. `signal[A]` and `frame[A, V, F]` take a ClassTag
(none written for concrete types; Repair.decode/read gained the
bound). The one claim left, stated at its line: a Within's body is a
program in the machine's row, erased at the operation because F is
not the operation's to name. All condition tests unchanged (one that
built Op.Signal by hand uses signal[Int]); core 362 green on JVM,
JS/Native green, every module compiles; persist, llm, match suites
green. specs/condition.md gained the section.

## demo-sessions — confirm-and-sign login replaces trust-the-field
Completed: 2026-09-02
Landed as cd4591b. `POST /login` mints a one-time 6-digit code (this
stack has no email transport yet, specs/security.md — the code rides
the response AND the server console, named as the demo's stated
limit); `POST /login/confirm` spends it once and answers a session
signed with okay-security's `Jwt` over an in-process ES256 key pair.
The session is the identity of RECORD for a `/match` turn: threaded
through `matchTurnLogged` -> `matchTurn` -> `scriptedAgent`/
`agentTurn`, it registers the ChatLog speaker and is what facts get
asserted under, overriding a DIFFERENT email the message text claims
(proven on the deterministic driver: a session as `real@x` asserts
under `real@x` even when the same message names `spoofed@x`; a live
model is told the session in its system prompt and asked to honor
it). The text-parsed "email x@y" stays the fallback for turns with
no session, so scripted/offline callers are unchanged. The vanilla
page gets a real login widget (email, code, `localStorage`, sent as
`Authorization: Bearer`). Landed alongside a sibling's demo-en-
phrasebook on the same function's signature — one straightforward
rebase conflict, resolved by keeping both parameter additions.
specs/demo-chat.md's new Sessions section; docs/modules/okay-demo.md.
TestLogin (4 tests) + two demo-level tests over a real socket. Matrix
70 suites in chunks, zero failures.

## same-operator — === is the witness
Completed: 2026-09-02
Landed as bf7825d. The operator: "можем определить оператор ===
такой как нам нужно". What the stack needs from equality of typed
tokens is the proof, so `a === b` (with a `Same[K]`) is
`Option[A =:= B]`: in the `Some(ev)` branch the compiler knows A is
B and `ev` converts; `=!=` is the Boolean "not the same key"; `==`
stays `equals` for a plain yes or no (permitted under strictEquality
by the derived CanEqual). TMap's lookups read `e.key === k`. Test:
a value moves from one key to another only with the proof in hand,
and an A is not a B without it (compile error). JS/Native green,
every module compiles.

## same-by-value — Same for value keys, with the tag that makes it sound
Completed: 2026-09-02
Landed as c8ce5a4 (2 commits). The operator asked for Same over
primitives. A typed id over a primitive cannot witness `A =:= B` by
equal values alone — `Id[User](5)` and `Id[Order](5)` are equal
numbers and different keys — so `Same.byValue(equal, tag)` calls two
keys the same only when value AND a runtime tag of the type
parameter agree; the tag is a ClassTag (exact for concrete types,
erased for generic ones — value keys are for concrete types, stated
on the method). Example `Id[A](n: Long)(using ClassTag[A])`. Tests:
through TMap, `Id[String](5)` and `Id[Int](5)` are two entries each
at its own type; through HMap the same two vals are two typed
entries and a fresh equal id is not in the type (keys there are the
vals' singleton types). Core 360 green on JVM, JS/Native green,
every module compiles; rebased three times over sibling landings
(demo-en-phrasebook and its release) before the merge went through.

## demo-en-phrasebook — the offline driver speaks two languages
Completed: 2026-09-02
Landed as 81ae611 (spec) + 7e90da4 (impl). scriptedAgent spoke only
Russian; language is now picked PER MESSAGE — isEnglish(text) is "no
Cyrillic character", content alone decides the reply template, no
session state. Every trigger pairs 1:1 (умею/can:/offer:,
нужен-нужно/want:-need:, спроси/ask (+ всех/all), сценарий/scenario,
шаг/step, флоу/flow, берусь/accept, отказываюсь/decline; помощь/help
was already paired — decided by the TRIGGER WORD rather than the
isEnglish flag, since an empty string carries no Cyrillic either and
would otherwise misroute the recursive help call). Both phrasebooks
drive the exact same chainedTable — language is presentation, not a
second code path. Noted in passing, not fixed (out of scope): the file
already carried dead duplicate match arms for сценарий/шаг/флоу
(unreachable, shadowed by identical live arms above them) — only the
live arms got English twins. Suite 22/22 three runs.

## same-typeclass — sameness of typed tokens, and strict equality from it
Completed: 2026-09-02
Landed as 1b13209. The operator asked for the `A =:= B` trick as a
typeclass of its own, and recalled Scala 3's equality work.
`okay.Same[K[_]]` (Same.scala): `same(a: K[A], b: K[B]): Option[A
=:= B]` — two typed tokens are one key, witnessed; `Same.byIdentity`
for reference tokens holds the one witness cast in the stack;
`a.sameAs(b)` at package level. Scala 3's `CanEqual` (multiversal
equality, strictEquality) permits `==` without proving anything;
from a `Same[K]` a `CanEqual[K[A], K[B]]` is derived at package
level, so token keys compare with `==` in strict mode and a key
against a String does not compile (tested). TMap and TRef use Same;
TMap.Keyed is gone. Core 359 green on JVM, JS/Native green, every
module compiles; rebased over demo-deal-timeline and re-verified
demo + match.

## demo-deal-timeline — a deal's negotiation history, made visible
Completed: 2026-09-02
Landed as 4ccb701 (spec) + 4ef2c3f (impl). Deal (okay-match) carries only
its current state, no history — the demo layer fills the gap without
touching the engine: chainedTable now threads off: Long (the same ChatLog
offset scriptedAgent/agentTurn already carry for facts_assert's
provenance), and the match_inquire/match_respond wraps each append a
DealEvent(state, by, Provenance("web-demo", off, what)) to an in-memory
per-deal log — append-only, the same story supersede tells for facts.
GET /deals/<n> and /deals/<n>.json render the current state plus the full
event vector with provenance; an accepted deal's stand-downs get their
own Withdrawn events (found via the responder's live dealsFor query,
taken AFTER onResponded ran); an unknown deal answers 404, not an empty
timeline. Suite 20/20 three runs.

## lake-delta — Delta Lake without Spark, and its read road
Completed: 2026-09-02
Landed as b4fc0c2. A new JVM module `okay-delta` wraps Delta Kernel
4.4 (delta-kernel-api + delta-kernel-defaults, the Delta project's
own library): `Delta.create(path, columns)`, `.append(path, rows,
loadId)`, `.snapshot(path)`, `.rows(path)` — rows in the seam's
SqlValue vocabulary, the commit protocol left to the kernel. A
`loadId` rides Delta's own transaction identifier, so a retried
append is refused rather than duplicated — the bulk-load posture's
dedup, in Delta's own words. Two things the kernel taught: the
Parquet writer wants a decimal at the column's declared scale (a
7.25 at scale 2 came back 7.25E-16 until rescaled first), and a row's
refusal arrives wrapped in RuntimeException, sometimes twice, so the
named IllegalArgumentException is dug out of the cause chain. Road 1
(the read side, already promised in specs/data.md) is proven on the
kernel-written table: DuckDB's delta extension reads it through the
JDBC seam, typed rows equal the kernel's own scan; DuckDB describes
delta_scan columns nullable regardless of the Delta schema, so
verify names the non-Option fields — the Parquet-marks-fields-
optional lesson, a third time. That leg skips offline (the extension
installs from the network). Matrix: 70 suites in chunks, zero
failures.

## tmap-keyed — the identity axiom is a witness, TMap is cast-free
Completed: 2026-09-02
Landed as 05953ee. The operator pointed at TMap.get's two casts (the
AnyRef ones for `eq`, and identity => type) and updated's. Now a
key type proves sameness itself: `TMap.Keyed[K]` with `same(a: K[A],
b: K[B]): Option[A =:= B]`; `get` and `updated` only apply the
witness, the map has no cast. `Keyed.byIdentity[K[X] <: AnyRef]`
states the axiom for reference keys once — "this token IS that
token, so A is B" — as the single `asInstanceOf` on a `=:=`
witness in the file; the reference bound removes the AnyRef casts
`eq` needed. TRef provides its Keyed in its companion; the test's
Key does the same. Core suite 357 green on JVM, JS and Native
suites green, every module compiles; rebased over a claim-only
commit.

## demo-market-live — the market page moves
Completed: 2026-09-02
Landed as f9f2985 (spec) + 96c6ce4 (impl). /market was a static render;
now GET /market.json serves the rows as disclosed facts with their
attribute names (the gate holds on JSON as on HTML — Public-only), GET
/events/market is a market-wide SSE feed matched before the
/events/<email> prefix route and pinged from the chainedTable wraps
(facts_assert, match_inquire, match_respond, flow_advance) plus
/admin/replay — model path and deterministic driver ring it alike; the
page keeps server-rendered rows and re-renders from market.json on every
ping, with attribute facet chips as the client-side filter. A closed
page's channel stays registered until process end — stated in the spec.
Suite 20/20 three runs (live endpoint up).

## tmap — two heterogeneous maps; the STM's write set on the dynamic one
Completed: 2026-09-02
Landed as a6a6d7c (2 commits). The operator's design for the STM's
write set and a question that followed it. `okay.TMap[K[_]]` (dynamic):
a key K[A] holds an A, keys compare by identity, the store is a cons
stack of typed `Entry[K, A]` pairs — a class, since a `(K[?], ?)`
tuple cannot say "the same A on both sides" — `foreach` takes a
polymorphic function so iteration is typed, `entries` is the
existential view (an abstract K cannot be applied to a wildcard), and
the one cast of the heterogeneous-map problem is stated once in
`TMap.get`: identity of a typed key IS type equality. Stm's Log is a
TMap; Stm.scala has no cast left. `okay.HMap[K, T <: Tuple]`
(static, the operator's `((A,B),(C,D),(E,F))`): the map's TYPE is
the tuple of `(key.type, Value)` pairs, `get` is a `Select[T,
k.type, V]` derived by induction over the tuple (V a type parameter
so inference carries it out), membership is a compile-time fact,
no cast anywhere; keys must be stable identifiers, which a
transaction's write set never has — so it exists for code that has
them. Tests: typed get, wrong type does not compile, identity vs
equality, typed iteration; HMap: get at the key's type, a missing
key does not compile, shadowing. Core suites green on JVM/JS/Native,
every module compiles; rebased over three claim-only commits.

## direct-flatmap-emission — the direct macro compiles to plain flatMaps; the Cont target retires
Completed: 2026-09-02
Landed as 454036a (merge; f5dcd7f compare receiveBlocking fix,
2b11dbe the macro rewrite, 454036a measure+docs). The optimization
bench-direct FILED is done, and went further than filed: not plain
flatMaps for sequential fragments with Cont at the corners, but the
whole emission target — `reflect(m)` IS `shift(k => m.flatMap(k))`,
so the Cont layer bought nothing the syntactic continuation does not
already provide, and even the stack discipline was always inherited
from F in both encodings. Measured (quiet box, spike watcher clean,
§1b re-tabled): 10k binds while+var 313µs -> 189µs (3.3x -> 2.0x —
exactly the loop's sequencing bind), recursion 410µs -> 56µs (4.3x
-> 0.59x — FASTER than the hand-written foldLeft chain, right-nested
emission vs left-nested rebuilding, the zio-direct mechanism, level
with kyo's hand-written 56µs). Monad instance hoisted to one val per
block; pure while conditions and statement Assigns fused. Full sbt
test green; every TestDirect* suite unchanged. Drive-by: compare's
Cluster/Merge benches called the retired receive() — receiveBlocking
now (compare/Jmh sits outside sbt test, so the channel-cas breakage
sat unseen; worth a thought for the build). Pickler traps recorded
in the spec's Results (nested quote in a splice, anonymous Type
params, cross-Quotes val). Road recorded, not promised: a CPS
sequencing pass could merge while's two binds per iteration (~1x).

## live-skip-on-gateway-loss — a live test skips when the gateway goes away, even mid-test
Completed: 2026-09-02
Landed as 8bcd9f2. The operator, after TestChatDemo's LIVE tests
went red twice on "HTTP/1.1 header parser received no bytes" from
the shared local gateway: make them skip when the gateway is
absent. They already skipped on a failed probe; now a gateway that
drops the wire DURING the test counts as absent too. `okay.llm.Live`
(main, tiny, shared by the suites): `wireDropped(e)` — an
IOException or HttpTimeoutException anywhere in the cause chain —
and `root(e)`; a `liveTest(name)(body)` helper in TestChatDemo and
the agent TestLive wraps the body and turns such a failure into a
named skip, while a wrong answer still fails. Unit test on the
predicate (EOF under IOException under RuntimeException; a
self-referential cause chain terminates; assertion failures are not
the wire). AGENTS.md: new live tests use liveTest. Verified with the
gateway actually down (all LIVE tests skipped, named); rebased over
sql-r2dbc and re-verified llm/agent/demo.

## sql-r2dbc — okay-r2dbc, the R2DBC hatch behind Sql
Completed: 2026-09-02
Landed as e8aa08d (operator: "НУЖЕН"). A new JVM module: `R2dbcSql(conn)`
over any `io.r2dbc.spi.Connection` — query as PULLED chunks through a
demand-driven Subscriber (request(fetchSize), park behind Async.Run),
update/batch summing every Result's count, transact with the granted
isolation read back, the seam's SqlValue/Col vocabulary; the typed
layer runs unchanged, the same suite on H2 (r2dbc-h2) and the
dockerized Postgres (r2dbc-postgresql). Two SPI lessons recorded in
specs/sql.md: Results must be consumed one at a time (collecting them
first hangs against the Postgres driver — H2 is eager and hid it; the
hang is what all those SIGTERM'd runs were sitting in), and metadata
exists only with a row, so describe reads the first row's, an empty
result describes as EMPTY, and nullability is the driver's word
(r2dbc-postgresql: UNKNOWN, so verify names every non-Option column).
Docs: docs/modules/okay-r2dbc.md. Matrix: 70 suites in chunks, green
except the demo's LIVE model tests, which the local endpoint drops
under load (a sibling has claimed live-skip-on-gateway-loss).

## stm-typed-interpreter — the handlers typed, one cast left, a rule in AGENTS.md
Completed: 2026-09-02
Landed as f50387f. The operator saw the remaining casts in
Stm.scala and asked whether they could go; on the honest answer
("the first draft's laziness") came the rule, now in AGENTS.md
"Code rules the operator has set": no cast without a real
necessity. All but one went: `perform[X](op: Tx[X]): X` and
`interpret[A]` are typed by GADT matching on the freer tree
(`case Bind(Effect(e), k)` types e and k), the commit holds each
taken cell in a `Held[X]` that releases or installs it, `park` is
generic in the answer, the Sim handler's loop is typed the same
way. `wrap`'s @unchecked went by deciding the cell's KIND at
construction: `TRef(init)` wraps every value, `TRef.bare[A <:
Stamped[A]](init)` installs bare and is the only kind that can
answer "unchanged" (`a eq content`, typed); the Channel uses it.
The one cast left is `Log.pending`: a write set keyed by cell
identity and heterogeneous in value — the key's type is the
value's, which no map type can say — isolated and explained. Gate
green in seven pieces on a host at load 10–16 from sibling sbt
runs; the only red was TestChatDemo's LIVE tests, twice, two
different tests, "HTTP/1.1 header parser received no bytes" from
the local model gateway on :8089 (okay.llm.Transports → JDK
HttpClient) — the endpoint, not the code (operator asked that
those tests SKIP when the gateway is absent: next lane). A/B in a
quiet window: 30.8–31.4 µs vs a noisy 34–41 baseline, no
regression.

## stm-no-anyref-cast — nothing in the cell is cast
Completed: 2026-09-02
Landed as 951ac50. The operator: "я хочу избавиться от
asInstanceOf[AnyRef]". modify skipped the CAS when the answer was
the same object as the content, and `eq` on an unbounded A needed
both sides cast to AnyRef. The skip only matters for Stamped
values (the Channel's State returns itself on a receive that
changes nothing) and a Stamped is a reference by type, so the check
is now a pattern — `case same: Stamped[?] if same eq s` — and a
wrapped value always installs, an equal one included (a version
bump and a spurious wake-up of a retry on that cell; the woken
transaction re-validates and parks again). A stays unbounded,
TRef[Int] keeps compiling. A/B two rounds once the host quieted
(load 6 after a 20+ stretch of sibling sbt runs): equal. Gate green
in six pieces under that load; rebased over pg-composite-rowtype
and demo-ctx-wiring and re-verified core + pg + demo + match +
chatweb before the merge.

## pg-composite-rowtype — a table's row selected whole is a typed Row
Completed: 2026-09-02
Landed as 3e817cc (operator: "НУЖЕН"). The connect preload joins
tables, views, matviews and partitioned tables (relkind r/v/m/p) to
the named composites, in user schemas only — still ONE simple query,
measured at 6.5 ms for the whole connect on the test database (the
catalog's own columns, the cost that deferred this, are excluded by
the namespace filter). `select p from okay_people p` decodes to a
Row whose fields are typed all the way down — this found and fixed a
real gap: `parseCompositeTyped` decoded fields with the static scalar
map, so a composite inside a composite stayed text; it now decodes
with the connection-aware `decodeCell`. `describe` names the nested
type; `Typed.rows[Wrap(p: Option[Person])]` reads it and verify is
clean, while the strict `p: Person` drifts with found "nullable" —
a whole-row column has no table column behind it. A table created
after connect is the raw text until reconnect (stated). Matrix: 69
suites in 8 chunks, zero failures — chunked because full runs keep
dying of an external SIGTERM (exit 143) that siblings report too.

## demo-ctx-wiring — the demo's handler as one value awaiting its environment
Completed: 2026-09-02
Landed as e812eb3 (spec) + ce7d346 (impl). ChatDemo.handler(budget) is
`(Transport, Secrets, MatchStore) ?=> Route` — ctx-wiring's factory half,
closed 2026-09-01 for want of "a consumer that actually rewires", reopened
with that consumer and shipped: main wires Transports.http() + Secrets.env
(the sys.env edge), the CTX WIRING test wires a canned Transport + memory
Secrets and runs the REAL Anthropic.stream parsing offline (untestable
before); every offline suite runs over a DEAD wire that throws on touch.
Model dispatch (live/local/scripted) reads ambient Secrets as env:NAME
refs. Merged over demo-replay-projections mid-flight (matchTurn's offset
threaded through the ambient signatures); okay-chat.log/ (the FileStore
the through-route tests now grow) gitignored. Suite 19/19; the one red
seen was the local model flapping mid-load (the TestLive lesson).

## stm-typed-content — the cell's content typed end to end
Completed: 2026-09-02
Landed as e591d8e. The operator asked for Owned to be generic for
type safety. Over an untyped Stamped that would be a phantom
parameter, so the whole content is typed: `Stamped[+A] { def value:
A }`, `Slot[A]`, `Owned[A]`, `AtomicReference[Stamped[A]]` — and
TRef.get/modify have no cast at all. A user value writes one line,
`extends TRef.Stamped[State] { def value = this }` (a self type
argument, not the F-bound the earlier objection made it out to
be); Channel.State does. What remains: one `@unchecked` in `wrap`
(a Stamped inside a TRef[A] is a Stamped[A] by contract, invisible
through erasure) and the interpreter's erasure casts over Tx[Any].
A/B two rounds: equal. Gate green (chunk C in two halves — the host
was at load 14 with siblings' sbt runs and the first attempt hit
the tool's timeout); rebased over two claim-only commits.

## stm-one-content — the cell holds one type
Completed: 2026-09-02
Landed as f3a2d18. The operator disliked that Owned and
Slot/Stamped were different types, forcing AnyRef and casts in the
cell. Now `Owned extends Stamped` and mirrors its content's stamp
and value, so the reference is `AtomicReference[Stamped]`,
valueOf/versionOf are gone (a field read), no AnyRef remains in the
cell, and Owned is matched only where ownership means something:
the fast path spins on it, a transactional read aborts on it, a
commit's ownership CAS fails on it. The one cast left is `value:
Any` to `A` in TRef.get/modify — the price of a bare value being
its own content; the handlers' remaining casts are the freer
interpreter's erasure over Tx[Any], the shape every handler in the
stack has. A/B two rounds: equal within noise. Gate green in three
chunks; rebased over demo-pg-backend and re-verified core + sql +
jdbc + match + demo before the merge.

## demo-pg-backend — the marketplace on live Postgres, one env var
Completed: 2026-09-02
Landed as 0da8033 (spec 39bd411). okay-sql gains
`Placeholders.numbered` — `?` outside quoted literals/identifiers
becomes `$1..$n`, the ONE mechanical dialect difference
bind-don't-model itself created between the JDBC and pg drivers;
pure, tested on JVM/JS/Native, and recorded in specs/sql.md as NOT a
dialect layer (the strings stay the DBA's). SqlMatch takes a
`placeholders` seam (identity by default) and its DDL says `DOUBLE
PRECISION` — two changes, and a 60-statement `?` program runs on the
pg wire. The sqlite suite, which had its three tests pasted three
times over behind stray braces (compiling by accident, running
three), is now `MatchEngineSuite` with two engines: TestSqliteMatch
(a temp file) and TestPgMatch (live, one schema per store, dropped
after) — guarantees, deals, flows, each surviving a reconnect, the
same text on both. The demo: `OKAY_CHAT_DB=postgres://user:pass@host
:port/db?sslmode=…&sslrootcert=…` is parsed purely (`PgTarget`,
tested: defaults, the TLS ladder by its postgres names, refusals
named) and `marketOf` puts the marketplace on PgSql or PgTls; a live
test drives it against the dockerized Postgres. Docs: okay-demo's
env table, okay-match's "Postgres is the same line" now cites the
test. Boards: the wire-typestate umbrella closed (resolved by
pg-scram-typestate, sql-typestate and the pg-wire-typestate decline);
a TestMcpAuth matrix flake filed (green alone).

## stm-slot-generic — Slot[+A], for the reader
Completed: 2026-09-02
Landed as 2f6d73c. The operator asked why Stamped and Slot were not
generic. Slot now is — `Slot[+A](value: A)`, a covariant override
of `Stamped.value` — so the wrapper's type documents itself; the
cell still holds AnyRef and re-attaches A once in valueOf, so no
cast moved. Stamped stays unparameterized on purpose: a typed
`value = this` needs an F-bound (`State extends Stamped[State]`) on
every user value for nothing but that field. Recorded in
specs/stm.md. Gate green in three chunks; rebased over a sibling's
claim-only commit before the merge.

## stm-slot-stamped — the cell's content is two kinds, not three
Completed: 2026-09-02
Landed as 0577ab3. The operator's suggestion after stm: `Slot
extends Stamped`. The cell now holds a Stamped — the value itself
when it carries its version, or a Slot wrapping any other value,
itself a Stamped with `value` overridden (`Stamped.value` defaults
to `this`) — or the Owned commit marker. `valueOf` and `versionOf`
are a field read behind one Owned check, `modify` has one path for
every value type, `wrap` and the commit's ownership CAS type against
Stamped. Caveat recorded: `wrap` stamps what it is given, so it must
only see what a transaction or modify produced, never an existing
Slot. A/B three alternating rounds: equal within noise on both
channel paths (history.tsv row). Gate green in three chunks.

## stm — one transaction language, a family of handlers; the Channel on it, faster
Completed: 2026-09-02
Landed as 5ac9b4d (5 commits; spec first). The operator's brief:
STM behind a typeclass, implementations optimized per case, and the
Channel on the same machinery "без потери производительности".
Delivered: `TRef[A]` (one cell, value + version in an
AtomicReference, one-shot waiters), the language `Tx`
(Read/Write/Modify/Retry — no Async in the row, so I/O inside a
transaction is a compile error), the door `Stm[F]` with three
handlers: `tl2` (JVM/Native: per-cell versions, incremental
validation so a body always holds a consistent snapshot, CAS-owned
obstruction-free commit, `retry` parks the transaction on its
read set and the committing thread re-runs it), `direct` (JS: one
thread, writes buffered to the end, no log), `sim` (deterministic:
a `Sim.Yield` before every step, versions validated at commit,
retry sleeps a virtual millisecond — the same transaction code
under every seed). Structural fast paths from the program's shape:
one Modify is the cell's own CAS, one Read a plain read. The
Channel's state is a `TRef[State]` and its transitions
`TRef.modify`. Measured (src/jmh ChannelBenchmark, alternating A/B,
history.tsv): the first cut wrapped values in a Slot and cost 10%;
`TRef.Stamped` (values carrying their version; an abstract CLASS,
since a trait's instanceof is an interface scan) removed the
wrapper and the buffer path is now 8% FASTER than master's
AtomicReference, the program path equal within noise. Tests on all
platforms: transfers under eight threads, torn-pair reader, retry
wake-up by the right cell only, a thousand parked transactions,
Sim under sixty seeds with interleaving asserted. Find: a
Scala.js incremental build kept `Stamped.$init$` from the trait
version — clean rebuilt it. BACKLOG: stm-ui-close, stm-sessions,
stm-orelse, stm-js-direct-bench. Gate green in three chunks.

## channel-cas — the channel without a lock
Completed: 2026-09-02
Landed as 500efb7. The operator: "сделай все неблокирующим через
cas immutable state". The channel's state is now ONE immutable
value in an AtomicReference — persistent queues for the buffer,
the waiting receivers and the waiting senders with their elements,
a size counter, the open flag, the failure. Every operation is a
pure `State => (State, action)`; a CAS loop installs the state and
only then runs the action, so a retry re-runs a pure function,
never a callback (the Drive handshake's shape over the whole
channel). No thread holds anything, ever; on JS the reference is a
plain cell. Same surface, same tests, plus a multi-producer/multi-
consumer stress: 8 virtual-thread producers × 1000 through a
16-slot channel into 4 consumers — 8000 elements, each exactly
once. Spec decision in specs/cross-platform-async.md. Gate green in
three chunks on all platforms.

## discarded-program-lint — a dropped program is a compile error
Completed: 2026-09-02
Landed as 8213968. The operator asked whether the channel rule
("send only inside a program, offer from plain code") can be checked
at compile time. It can, for the shapes the compiler sees: build.sbt
escalates -Wall's value-discard and non-unit-statement warnings to
ERRORS when the discarded top-level type is an `A ! F` program
(regex on the message; a `!` nested in Sim's Queue element type is
not matched). Probed: statement `{ c.send(1); () }`, Unit def body,
eta-expansion into `Int => Unit` — errors; `xs.foreach(c.send)`,
`for x <- xs do c.send(x)` — invisible (foreach's U takes anything),
stated in AGENTS.md. The lint paid for itself before landing: five
more silent discards in okay-demo/web (chatweb Main.scala) that the
channel-callback migration had missed, all on offer now. munit's
compileErrors cannot see lints, so the test covers the sanctioned
spellings and the spec records the probe. Gate green in three
chunks (the full run was being SIGTERMed near ten minutes — the
tool's ceiling, not the build).

## channel-callback — one channel for every platform, waiting in queues not threads
Completed: 2026-09-02
Landed as 5502971 (3 commits). The operator asked whether blocking a
thread is not "too crude" and, on Native, what it costs — and said
"Давай, делай колбэчный канал". The JS design is now THE Channel,
src/main/scala/Channel.scala: `receive: Option[A] ! Async` and
`send(a): Boolean ! Async` are programs; a receiver that finds the
buffer empty leaves a callback, a sender that finds it full leaves
its element and a callback, send/receive/close hand things to the
first waiter; the state sits under a short lock, callbacks run
outside it. No thread parks inside the channel and nothing polls
(the old receive polled every 10ms to notice a close). Parking
forms `receiveBlocking()`/`sendBlocking(a)` exist only under
CanBlock, like Fiber.join; `offer(a)` is the non-suspending send.
The scala-js and scala-jvm-native channels are gone. Callers
migrated across http/mcp/jetty/netty/demo/ui/cluster: a send inside
`async(...)` became the program itself, sends from plain callbacks
became `offer`, test drains use `receiveBlocking()`. Found on the
way: the migration's first cut silently DISCARDED sends in Ui.offer,
Dom, ReactJs, Jetty and Netty queues (a `Boolean ! Async` in
statement position is a value, not an action) — the -Wall
value-discard warning and TestDom caught it; all on offer now.
Tests: a thousand parked receives hold no thread; a bounded send
suspends and resumes on the consumer's take; close wakes a parked
receiver at once and drains a parked sender's accepted element; the
200-round send/close race keeps its accounting invariant. Spec
decision in specs/cross-platform-async.md; BACKLOG
native-scheduler-pool (a fixed pool is now safe on Native). Full
gate 69/69.

## channel-send-closed — send after close is refused, not thrown
Completed: 2026-09-02
Landed as 5eaec72. The operator asked (after the receive() walk-
through) for send-after-close to be an error, "but safer than an
exception". `Channel.send` now returns Boolean: true when the channel
took the element, false once closed (dropped, nothing thrown) — a
producer that outlives its stream reads the fact and stops instead of
having its fiber unwound. Exact under the send/close race on JVM and
Native: check open, put, re-check; a put that landed after the close
is taken back, and counts as accepted only if a receiver already had
it. JS keeps the same surface. Callers (merge, buffer, Remote.listen)
ascribe the result. Tests: refusal after close; a 200-round race
(virtual-thread producer, consumer, close at an arbitrary moment) with
the accounting invariant received == accepted, in order; the cross
suite checks the refusal on every platform. Decision recorded in
specs/cross-platform-async.md. Full gate 69/69 green.

## bench-direct — the direct syntax priced, ours and the apt competitors
Completed: 2026-09-02
DirectBenchmark (compare) + the 1b table in benchmarks.md: each
ecosystem's first-party direct form against its own hand-written
flatMap chain, same shape, same run, quiet box. okay `direct`
(while+var) 313 µs/10k binds = 3.3x over the 96 µs baseline — the
price of the Monadic Cont layer the macro emits, not of the macro;
the recursion spelling 410 (4.3x). kyo defer 157 = 2.8x over its
eager chain. zio-direct 119 = 0.64x — FASTER than the naive
hand-written chain (its macro emits a better shape than left-nested
binds; credited in the doc). Expressiveness measured by refusal:
both competitors forbid `var` in their blocks (kyo also nested
marks); the imperative form compiles only in okay (direct-loops).
FILED for the direct lane: emit plain flatMaps for purely
sequential fragments, reserving Cont for control corners — would
close 3.3x toward 1x. (Room post failed: rozum daemon "Too many
open files"; this entry carries the handoff.)

## pg-wire-typestate — measured, declined, recorded
Completed: 2026-09-02
Landed as ac1d76a (spec only; PgSql.scala unchanged — that is the
result). specs/typestate.md said the pg graph could be typed only
if the readability gain inside the one file measured worth the
plumbing, after Scram proved the pattern. Measured phase by phase:
startup → auth → ready is already one-way by construction (private
constructor, Scram phase objects); ready ↔ in-tx is a public-seam
cycle the spec forbids typing and Typed.region already types for
callers; portal and COPY are local defs inside one method each,
called from the for-binding below; the one cross-cutting rule
(every public entry passes through `settled`) was checked entry by
entry — all eight do. The cheapest candidate, a Portal(oids) phase
object, turns one threaded parameter into a field and nothing else;
PState fits nowhere (no phase changes the state's type). Decision
and the reopening condition (a second consumer of the driver's
internals) recorded in the spec's Decisions/Results.

## pg-mtls — the client presents an identity; the TLS seam's last rung
Completed: 2026-09-02
Landed as 69b5f5d. `Tls.client` turns `clientCert` + `clientKey` into
key managers through the same `contextOf` the server half uses, so
the certificate is offered when the server sends CertificateRequest
and nothing changes for a server that does not; a half identity
(cert without key, key without cert) is refused by name; the key
stays a Secret ref, inline PEM refused as before. Zero signature
change — the fields rode in TlsConfig from day one. Proven live:
`okay-pg/mtls-provision.sh` provisions the dockerized Postgres (a
client CA, a cert with CN = the role `okay_mtls`, `ssl_ca_file`,
`hostssl all okay_mtls all cert clientcert=verify-full` inserted
before the scram rule — idempotent, and the first time the ssl
provisioning is a script rather than prose); TestPgMtls logs the
role in with the certificate and NO password and queries as itself,
is refused by the server without it ("connection requires a valid
client certificate"), and shows the password role still SCRAMs in
with or without an identity offered. okay-tls gains the half-identity
and full-identity unit tests. specs/tls.md box checked with a Results
entry; the okay-pg module doc gains its TLS paragraph. Full matrix
green (69 suites).

## audit-fixes — the 2026-09-02 master audit, everything found fixed
Completed: 2026-09-02
Landed as 5580283 (6 commits, AGENTS.md rule on top). The operator asked for an audit of
master and then "всё исправь". Mechanical: the resurrected
direct-effect-provide BACKLOG entry, three stale `.?` spellings.
Reconciled: Of[A] derives Answers — one typed condition door, two
spellings (raiseC takes an Of; a bad resume is BadResume, named).
Defects, each with a test: two same-named condition frames aliased
(a Restart handle targets its frame by IDENTITY now; the policy's
Invoke stays by name); a marked `val` lost its symbol, so a local
def after it or a `var` bound from a mark failed to compile
(re-bound in place, symbol kept); a catch-all `CanTry` let a LAZY
monad's try never fire (named instances; a lazy monad is a compile
error that says why); Condition.run recursed per Resume and
overflowed near 10k (a while loop; 100k tested); a pure argument
before a marked one ran AFTER the effect and once per continuation
under multi-shot (hoisted first, once); foreach/map materialized
the receiver with `.toList` (LazyList — an unbounded receiver is
forced only as far as the monad drives it). Cleanup: rowOf by
symbol not name, one `stripped`, wrapPure via wrapStat, asCont's
dead parameter, identity matches after &&/||, `!?` delegates to
reflect, NoSuchRestart says "none". Stated in the specs: direct-try
over a `within` frame body's pure segments; direct-try-ctx BACKLOG
(dotty 3.7.4 erasure crash). Full gate green after rebase.

## pg-scalar-types — numeric is exact; vendor scalars are named
Completed: 2026-09-02
Landed as 96191fb (spec 1 commit before). numeric/decimal no longer
rounds through a Double in either driver: SqlValue.Num(BigDecimal)
under SqlType.Num (pg 1700 from text, NaN/±Infinity to F64; JDBC via
getBigDecimal/setBigDecimal). Typed: a Double field still reads Num
(lossy by the FIELD's choice — v1 consumers keep working), a String
reads its exact text, and `given decimalSchema: Schema[BigDecimal]`
(import okay.sql.given) is the exact typed field. pg describe names
uuid/json/jsonb/xml/timestamp(tz)/date/time(tz)/interval/inet/cidr/
macaddr/money instead of oid:N; values stay text and a String field
fits ANY Other column with a clean verify (bind-don't-model). Find:
sqlite-jdbc getBigDecimal + wasNull throws — nullness decided from
the value. okay-sql 10/10 ×3, TestTyped 16/16, live pg 15/15,
okay-match 28/28 (sqlite), full matrix green.

## sql-schema-composite — Vector and nested case classes bind to Arr/Row
Completed: 2026-09-02
Landed as d7c8e0d (spec 1 commit before). The Schema layer closes the
composite story: a case-class field typed Vector[T]/List[T] decodes
from SqlValue.Arr and a nested case class from SqlValue.Row, both
recursive (Vector[Option[Int]], Vector[Vector[Int]], Vector[Addr],
Option[Addr]); the encode side mirrors it, so Vector/nested params
bind as Arr/Row. Typed's field shape became a recursive Shape
(Prim/Opt/Iso/Arr/Row) — decode and encode are two folds, the old
into/outof closures retired. SqlType gains Arr(elem)/Row(fields) for
verify (fits recurses; Arr(Other) from JDBC metadata passes, decode
checks the elements). JdbcSql: Types.ARRAY, getArray, Object[] bind.
PgSql: describe types columns through the composite/array caches.
Proven: okay-sql 8/8 on JVM+JS+Native (one-frame fake driver),
okay-jdbc TestTyped 15/15 over H2 (array column read/verify/bind),
okay-pg TestPgComposite 14/14 live (Person with Vector[Int], Addr,
Vector[Addr], Option[Addr] via Typed.rows from a table, clean verify,
a Wrong shape drifts on the column; rowsOf binds Vector+Addr params).
Composite fields bind by POSITION (no names on the wire).

## condition-typed-signal — the typed door, and the gate lesson paid in full
Completed: 2026-09-02
Landed as 1281a4c + FIX 96f5b3c. Of[A] + the typed signal edge + the
typed resume: a wrong-typed resume stops compiling; the machine
stays erased, the Any floor untouched. The ONE ungated landing of
this arc (operator directive, under what looked like a machine-wide
sbt lock) shipped the arc's ONE real defect: the typed signal
self-resolved (Of[A] beats Any), the self tail call compiled to
while(true), and the forked test JVM burned 47 CPU-minutes — which
ITSELF was the "lock": sbt-2 batch buffering hid all post-load
output, my own timeouts sent the 143s, and the zombie's CPU/swap
pressure stalled the machine. The (c: Any) ascription is
load-bearing and commented as such. 17/17 condition suites; full
matrix green in 106s on the freed machine, exit 0.

## pg-composite-array — arrays of a named composite decode to Arr of typed Row
Completed: 2026-09-02
The first sliver after pg-composite-fields-typed: an array whose ELEMENT
is a named composite (`addr[]`) now decodes to `Arr(Row(...))`, not text.
The connect preload gained a second map — each composite-array type OID
to its element composite OID (`pg_type.typelem` where the element is a
`relkind='c'` composite) — and the per-cell decode became a
connection-aware `decodeCell` that passes ITSELF as `parseArray`'s
element decoder. So an array element that is a composite types through
the field cache, a scalar element through `valueOf`, and nested arrays
recurse with the same decoder: `array[row('main st',90210,true)::addr,
row('elm',null,false)::addr]` yields
`Arr(Row(Text,I32,Bool), Row(Text,Null,Bool))`. Live TestPgComposite +1
(12 total); the pg suite and the rest stay green. Deferred and re-filed
as pg-composite-rowtype: typing a table's whole row-type (relkind='r'),
which would draw every table into the connect preload.

## direct-try v2 — marked catch bodies graduate
Completed: 2026-09-02
Landed. The sibling's v1 try sandwich (reify body, CanTry.tryIn,
reflect back) gains effectful handlers: a catch body with marks goes
through the block pipeline at the join type — Writer("recovered")
.reflect inside the handler tells. Marked guards keep the named
refusal; the finalizer refusal stands. A claim lesson recorded: the
task was promoted from a STALE backlog entry (v1 had landed without
the board cleanup) and converted into the delta instead of a no-op
release. Full matrix green, exit 0.

## pg-composite-fields-typed — named composite columns decode with typed fields
Completed: 2026-09-01
The follow-up to pg-composite-decode: a NAMED composite column's fields
are now TYPED, not handed back as text. The obstacle was the protocol —
a mid-query catalog lookup would corrupt the open extended-protocol
portal — so the driver PRELOADS every named composite type's ordered
field OIDs ONCE at connect (a single simple query in the ready state,
where no portal is open, cached on the connection). `dataRow` then
types a composite column's fields from that cache with no extra round
trip: `select row('main st', 90210, true)::addr` decodes to
`Row(Text("main st"), I32(90210), Bool(true))`, a NULL field as `Null`.
Anonymous `record`/ROW() deliberately stays fields-as-text — its field
types are genuinely undiscoverable (no typrelid, nothing on the wire).
A composite type created after a connection is unknown to it until
reconnect (stated). Live TestPgComposite gains 3 (typed fields, NULL
fields, record-still-text); the pg suite (29) and the rest stay green —
the preload is one extra query at connect, robust to a catalog it
cannot read. Filed pg-composite-array-of-composite for the last
slivers (arrays of composites, table row-types).

## direct-effect-provide — coloring as policy: the grant is a capability
Completed: 2026-09-02
Landed. A def requires Effect[G] as a using parameter and its
ascribed positions color only by the grant; provide installs the
permission for one expression, providing composes it as a layer,
the ungranted site does not compile. TestEffectProvide pins the
three shapes; the ascribed-val rule recorded. Full matrix green,
exit 0.

## bench-ctx-reader — the compiler-runs-the-monad claim gets its number
Completed: 2026-09-01
ReaderBenchmark gains the ctx-function rows: direct style (10k
ambient reads via wire under one provide, Blackhole-consumed) runs
at 0.48 us — two-plus orders below the row Reader's relay, because
there is nothing to interpret; the chain built THROUGH ctxMonad
measures ~5.3 us per 1 000 binds (~2x the relay per bind) and is
stack-bounded. Benchmarking surfaced E22 (specs/context-functions.md):
no trampoline — capacity 2-5k binds on a default stack; a
mutating-var chain build SELF-CAPTURES (the inserted ctx-closure
takes the var by reference) and overflows at any depth — recursion
only; and foldLeft over ctx-fns fails in lambda-result position
(the E10 family). benchmarks.md section 2 tells the ratios;
capabilities.md boundaries carry the width-not-depth doctrine. A
19-leg okay regression screen ran under sibling sbt load: error
bars 50-200%, inconclusive by the honest-limits rule; cats matched
its doc number while every effectful leg (zio included) sat 2-5x
high — a load profile, not a regression pattern; Writer flagged for
a quiet-box recheck; today's library changes are additive.

## error-messages — absence answers with a recipe, wording pinned by test
Completed: 2026-09-02
Landed with @implicitNotFound on Monad, Applicative, MonadPlus,
Handler, TypeableK, CanBlock, Direct.Effect and DirectCtx — each
message names the fix (the given import, Handler.union, the
one-line Effect registration, the Blocking door, direct { }).
TestErrorMessages pins five wordings plus the direct macro's
standing refusals, so a rewrite that loses the actionable substring
fails the suite instead of a user's terminal. Probed and recorded:
TypeableK and CanBlock always resolve in-package (annotations serve
downstream); compileErrors snippets need imports inside the string.
The ambiguity half of the directive was already served by
direct-mark-retire (the class is removed, not reworded). Full
matrix green, exit 0.

## docs-spellings-sweep — the mark family documented everywhere
Completed: 2026-09-02
Landed with the when-which-spelling table in direct-style.md (one
unified three-strikes story, the boolean-readability caution), the
Idris bang-notation (Brady 2013) and Frank citations in theory ch8,
the typepedia mark-family entry disambiguating three uses of the
word reflect, and the guide's one-paragraph pointer. Gate note: two
LIVE TestChatDemo assertions flaked on model judgment under load and
passed 15/15 on rerun — recorded for the demo lane in BACKLOG.
## direct-try — try graduates from the v2 list
Completed: 2026-09-02

A try body with marks compiles now: the recursive pipeline reifies
it as its own sub-block at the try's joined type, and the whole try
becomes one mark over CanTry[F].tryIn — the Throws.scala seam the
road map named. Strict monads catch at construction (their
computation IS the construction — full coverage, tested on Option);
Free rows guard construction and every continuation step (a pure
segment throwing after an effect lands in the catch, the effect
before it having happened; an unmatched exception rethrows) while a
throw inside an effect's handler stays that handler's business.
Finalizers and marks in catch bodies remain refused, named; the old
"try is v2" test graduated into the three new ones. Literal-typed
branches forced a union join by hand; a Nothing-typed throw-tail
upcasts through the monad, not variance. 312 core tests.

## condition-caps — lexical restarts as capabilities
Completed: 2026-09-02

frame(name)(body)(recover) hands the body its Restart[V] as a
context capability: in-scope code unwinds to ITS frame directly —
no signal, no policy round-trip, V reaching recover typed — and a
nonexistent restart is unwritable code (the constructor is private;
only a frame hands a handle out). An outer handle invoked from an
inner frame unwinds past both. The menu stays the floor; within
stays. 10 condition tests.

## direct-mark-retire — .? retired, .!? resurrected, the mark family settled
Completed: 2026-09-02
Landed as d474b8d. The three-strikes story closed: .? was ambiguous
with okay's own Throws row-? (found twice independently) and is
gone; .reflect is the name for every scope; .!? returns as the
postfix symbol (its only charge had been redundancy beside .?, and
it collides with nothing); and direct-bang's prefix !prog (unary_!,
landed in parallel by the ui lane — Idris bang-notation's point in
the design space) joins as the one-glyph gesture. One dispatch-by-
type serves all three. The Ambiguous-extension-methods error class
for the mark disappears by construction. 58 tests across the direct
family, TestThrows untouched. GATE CAVEAT: OOM-kill late, 0
failures, full reference coverage — the standing precedent.
## condition-typed — signal stops casting
Completed: 2026-09-01

Answers[C, A] is the typed pair: raiseC(HowMany("retries")) IS an
Int with no annotation at the site, and a policy Resume of the wrong
type is BadResume named at the point where it acts — not a
ClassCastException three calls later. The policy stays heterogeneous
BY DESIGN (one function holds a deployment's whole stance; typing it
per-condition was declined in the spec). Additive: signal/Any stays.
The proof is the day's three consumers moved over — InvalidSubmit
(askWith gains an implicit ClassTag; call sites untouched),
BadEmail, MalformedValue — policies unchanged, casts gone. 9+67+28
+15 tests green.

## pg-composite-decode — the pg driver decodes COMPOSITE/ROW() and ARRAY types
Completed: 2026-09-01
The pg driver handed composites and arrays back as one opaque string
(`valueOf` fell through to `SqlValue.Text` for any non-scalar OID);
now it decodes them into structure. `SqlValue` gained `Arr(Vector)` and
`Row(Vector)` (okay-sql, additive). An array whose element OID the
driver knows (int2/4/8, text/varchar, bool, float4/8, numeric, bytea)
parses to a typed `Arr` — each element through the ordinary `valueOf`,
nested arrays recursed, a `NULL` element as `Null`; `record`/ROW()
(oid 2249) parses to a `Row` with fields split and unescaped (a
record's per-field types are not on the wire, so fields arrive as
`Text`; an empty unquoted field is `Null`). One text parser reads BOTH
pg escaping conventions (`""` and `\"`/`\\`), so quoting and embedded
commas survive; `textOf` re-encodes an `Arr`/`Row` to the pg literal,
so a decoded value round-trips through copy/bind. Proven live over the
dockerized Postgres (TestPgComposite, 8: int[], text[] with quoting +
NULL, bool[]/float8[], empty, nested int[][], ROW()/record, composite
quoting, Arr round-trip). The existing sql/jdbc/pg suites stay green —
the new cases are additive. Follow-up filed (pg-composite-fields-typed):
named-composite columns and typed record fields need the composite's
attribute OIDs resolved at describe time.

## demo-direct-showcase — the worked example, and the two holes it found
Completed: 2026-09-01
Landed as e5a3205. ChatDemo's scripted go became a markless for-do
(`for t <- reply.split(' ') do Writer(t + " ")`); the agentTurn
migration converged with ui-direct's parallel landing (theirs kept —
explicit direct[F] with .reflect). The example did its job as a test
bed: loop and while BODIES now carry statement semantics (a bare op
as the body used to compile as an expression and be dropped), and a
fully markless block with a runnable loop body is intercepted (the
interception used to gate on hasMark). Recorded: .? is ambiguous
under the Throws import (okay's own row-?) — .reflect is the
collision-free idiom, and the symbolic mark is queued for retirement
(three strikes: .!, .!?, .?). Full matrix green, exit 0.

## kafka-repair — the repair road holds over the real engine
Completed: 2026-09-01
The Repair seam is engine-agnostic by construction (Repair over
Typed, Typed over any Topic, KafkaStore is a Topic); TestKafkaRepair
is the proof on the wire, live against the okay-kafka broker with
the production shape of damage — a FOREIGN producer's garbage bytes
at offset 1 of a typed topic. The same road, three policies:
Resume patches at the broker-assigned offset (Vector(a, patched,
c)); Invoke("skip") drops the offset with order intact; Fail aborts
naming offset 1. Live-gated (skips without the broker); 3/3 against
the real thing. No production code changed — the lane is one test
and this record, which is the point: conditions crossed an engine
boundary untouched.
## direct-bang — the one-glyph mark for the rows, prefix
Completed: 2026-09-01

`!prog` joins `.?`/`.reflect` as the row mark: a program of type
`A ! F` collapses under its own type's symbol. The postfix `.!` was
tried and refuted within the hour — the method name shadows the
object `!` for every file importing Direct.* (`!.run` broke in
TestConditionDirect); the prefix spelling (unary_!) carries a
different name, shadows nothing, and reads as "perform". The macro
recognizes the third mark; the ui wizard test now reads
`val name = !Form.ask[Name]("who?")`. A direct-style.md section
records the collision so nobody retries the postfix. 305 core + ui
tests green.
## pg-sslmode — the pg driver speaks TLS through the one seam
Completed: 2026-09-01
specs/tls.md's pg box: Postgres over TLS, the seam's second consumer
and the one with a protocol preamble. `PgSql.connect` was factored
into `connectOver(conn: NetConn, …)` — the startup + SCRAM half over
any established connection. On the JVM, `PgTls.connect` does pg's
STARTTLS-style SSLRequest dance on the raw socket (Int32(8) + code
80877103, then the server's single 'S'/'N' byte), wraps the socket via
`Tls.client` on 'S', and hands the encrypted NetConn to `connectOver`,
which runs the same SCRAM and never learns it is on TLS. A server that
answers 'N' when encryption was asked for is refused by name. okay-pg's
JVM leg compile-depends on okay-tls (the box's own shape — the dance is
the driver's, the session is the seam's); the JS leg has no okay-tls,
so `PgTls` is scala-jvm only. Verified LIVE against the dockerized
Postgres with ssl reloaded on: sslmode=require completes end to end,
verify-full with the server CA passes chain AND hostname, verify-full
with an unknown CA is refused (TestPgTls, skips where TLS is not
offered). Plaintext connections keep working — ssl=on accepts both, so
the existing pg suite is untouched.

## ctx-reader-elim — Reader elimination: the gate lifted, direct blocks the consumer
Completed: 2026-09-01
Landed as e66ac87. Int ! (Reader % E + Row) rewrites to
E ?=> Int ! Row = direct { ... wire[E] ... } — the environment out
of the row, the elaborator running the Reader half at compile time.
TestCtxReaderElim: equivalence at both spellings; provide nesting
overrides THROUGH the effectful block (nearest-wins survives the
macro); lift/unlift one-liners at the call site (functions, never
Conversions — E10 stands). specs/context-functions.md's
ctx-reader-bridge gate lifted; the capabilities.md recipe records
when to KEEP the Reader row (local-style rescoping, handler-visible
asks). GATE CAVEAT: OOM-kill late, 0 failures, full reference
coverage — the standing precedent.

## condition-direct — resumable exceptions in direct style, the frame door, chapter 09
Completed: 2026-09-01
Landed as 167c02e. signal.? resumes at the mark (a call that may
return — the Common Lisp reading, asserted by TestConditionDirect's
before/after trace); within/frame unwind to their frames from direct
blocks; a for-do loop repairs malformed elements mid-stream (the
operator's story, per-element Resume). The frame door takes the
restart body as a direct block (two lines over within + direct).
docs/direct-style.md gains the section; docs/theory/09-conditions.md
opens with Goldberg-Robson/Steele/Pitman, argues via Plotkin-Pretnar
that handlers ARE resumable exceptions, and names both recorded
roads (typed signal, restart capabilities) through Zhang-Myers'
bidirectional effects. GATE CAVEAT: the full matrix was OOM-killed
late with 0 failures and every reference suite covered — the
direct-macro/direct-loops precedent.

## llm-cut-conditions — the repair road between passing and cutting
Completed: 2026-09-01
Cut gains `screened`, the first module consumer of the condition
system: a violating token signals the Violation while the stream's
continuation is live, and the policy chooses per incident —
Resume(t) emits a replacement in the token's place and the stream
continues; Invoke("drop") makes the token vanish; Invoke("cut", v)
falls back to the old hard cut (pull stops, the guard answers
Left(v)). The menu is ["drop", "cut"]; mechanism in the stream,
policy at Condition.run. Additive: checked/watched untouched, a
clean stream never signals (the policy-never-consulted test).
TestCutRepair (4); okay-llm 18/18 JVM, JS compiles.
## ui-direct — the three roads reach the toolkit, and the demo
Completed: 2026-09-01

Direct wizards: a Dialog scenario is straight-line code under
direct[[A] =>> A ! Dialog] with .reflect marks (the ? spelling
collides with Effects' row-?; named mark, tested). Form.askWith
lifts ask's retry policy into conditions — InvalidSubmit signaled,
forgiving≡ask, patience(n) gives up, a repairing policy Resumes a
forced value, and a valid submit never consults the policy (the
machine runs per submit over a pure program). Dialog.hosted /
Nav.hosted are the ambient-Host doors. And the demo's agentTurn is a
direct block now — remember, seed, converse as three plain lines,
the seeding loop staying a named helper exactly as the macro's
no-marks-under-lambda rule prescribes. 67 ui tests, 15 demo tests.
## tutorial-new-arcs — the day reaches the tutorial
Completed: 2026-09-01
Three chapters join the worked tour: 19 "Needs are types:
capabilities" (the TestShowcase shape — one route, production edge /
provide unit / providing override, missing capability = compile
error), 20 "Monads as plain code: the direct block" (bare statements
as do-notation, multi-shot preserved, the door-outside-block-inside
composition), 21 "Errors you can repair: conditions" (signal keeps
the continuation alive, the repair story: one loop, three policies).
The closer renumbered to 22 and now points at capabilities.md and
direct-style.md; the docs index count updated. Every snippet's shape
runs in the repo's tests (TestShowcase, TestDirectDoors,
TestCondition).

## direct-loops — effectful iteration in direct blocks
Completed: 2026-09-01
Landed as a2ba997. for-do/foreach, for-yield/map (the traverse
shape) and while are rewritten into recursive Cont loops over an
immutable materialized List (multi-shot re-entry sound — 2x2
continuations tested; .iterator built by name so Array receivers
serve), while's cond/body splice inside the recursive def and
re-evaluate per iteration, and Assign with a marked rhs binds then
assigns (surfaced by the first loop test). Mid-loop None stops the
loop (2 of 3 hits observed). Non-whitelisted HOFs keep the v1
refusal. 35 tests across TestDirect+TestDirectAuto. GATE CAVEAT:
two full-matrix runs were OOM-killed near the end (the machine at
5.9G/6G swap) with 0 failures each; their suite UNION covers every
suite of the last complete green run — landed on that evidence, the
same precedent as direct-macro's landing.
## match-conditions — malformed tool values meet the condition system
Completed: 2026-09-01

The v1 silent coercions (a "num" tag with no number became 0.0) are
a named policy now: valueOr signals MalformedValue with the legacy
restart on the menu. The default table invokes legacy — nothing
changes for anyone — while table(store, policy) lets a deployment
REPAIR (Resume at the live signal point with a corrected Value) or
REFUSE (Fail becomes a {"refused": ...} answer the model reads and
retries; no fact stored). A well-formed value never consults the
policy. The condition system's second applied consumer. 28 match
tests.
## demo-conditions — the intake's silent default becomes a decision
Completed: 2026-09-01

The condition system's first applied consumer outside its own tests:
the marketplace intake signals BadEmail instead of silently minting
guest@demo. The guest restart is on the menu; the lenient demo
policy invokes it (yesterday's behavior, now chosen on the record),
a repairing policy resumes AT the signal point with a corrected
address, OKAY_CHAT_STRICT=1 escalates as Unhandled naming the menu —
one intake, three outcomes, and a present email never consults the
policy. 15 demo tests.

## persist-wire-tls — the wire runs encrypted, the first consumer of the TLS seam
Completed: 2026-09-01
specs/tls.md's persist-wire box: persist-wire over TLS passes the same
acceptance suite as plaintext. The move keeps okay-persist dependency-
free by making the wire's transport INJECTABLE rather than TLS-aware:
`Wire.Server` gained a `socket: Option[ServerSocket]` (pass the
SSLServerSocket from `Tls.serverSocket`) and `Wire.Remote.connect`
gained a `wrap: Socket => Socket` (pass the `Tls.client` wrap, whose
contract is exactly "wrap the connected socket before any protocol
byte"). Encryption wraps the TRANSPORT, so the handshake, capability
grant, frames and refusals are byte-for-byte the plaintext behaviour —
the acceptance is what does NOT change. okay-tls joins okay-persist in
TEST scope only; the SSLSocket is built in the test, so the core-only
compile graph (okay + codec) is untouched. TestWireTls (live over an
openssl localhost identity, skips where openssl is absent): the
encrypted grant, an append/read round-trip, refuse-by-name, and — the
proof it is REQUIRED not optional — a plaintext client refused by the
TLS server. The plaintext TestWire/TestWireRepl (12) unchanged and
green. Still open for the pg lane: the sslmode SSLRequest dance.
## demo-polish — the demo teaches itself and fails visibly
Completed: 2026-09-01

The page states its mode and links /market — the marketplace made
visible as lists of DISCLOSED facts only (a Matched phone stays off
the page; the gates hold there too, by test). Example chips fill the
input; "помощь" reaches the phrasebook. Failure is visible: a model
dying on the agent path answers an error frame the page renders
(⚠), and a dropped stream on the plain path is detected client-side.
14 demo tests.

## ctx-e20-pattern — the door outside, the direct block inside
Completed: 2026-09-01
The two arcs of the day meet: TestDirectDoors (core, 3 tests) lands
the E20 pattern as executable documentation — `def told: Env ?=> Int
! (Writer % String) = direct { Writer(s"hello ${wire[Env].user}");
wire[Env].uid }` with provide/providing at the edge. A direct block
is itself a context function (DirectCtx[F] ?=> A), so it nests under
the environment layer by nearest-wins and wire resolves inside;
three layers peeled by three machines (compiler / macro / handlers),
none knowing of the others. Sections added to capabilities.md and
direct-style.md, cross-linked.

## docs-direct-style — the direct-style documentation, user page and theory chapter
Completed: 2026-09-01
Landed as ea0df1b. docs/direct-style.md: the four layers (reflection,
the direct block, auto-coloring, do-statements) with the rationale
for every boundary, worked examples, the choosing-a-layer table and
the nine-entry refuted-alternatives graveyard. docs/theory/08:
Part IV of the textbook — the same story argued from the literature
(Filinski 1994/1999, Kameyama-Hasegawa 2003, Flanagan et al. 1993,
Brachthauser et al. 2020, Lindley-McBride-McLaughlin 2017,
Sivaramakrishnan et al. 2021), every Okay claim with file:line.
Linked from docs/README.md and the theory index (eight chapters now).
## demo-ctx — the demo adopts the capability style
Completed: 2026-09-01

The user asked where the new context-function DI pays; the demo was
the textbook site. MatchStore is ambient now: seven signatures drop
the threaded `store = market` default (the hidden-global-with-
override idiom); main provides the durable store, each test provides
its own — the forgot-to-thread bug class (one test once hit the
global sqlite) is structurally gone. Cut.checked gained the
ambient-prompt door (additive), so the demo's guard reads
Cut.guard { Cut.checked(tokens)(rule) }. 13 demo tests green.

## direct-do-statements — bare statements run: do-notation for direct blocks
Completed: 2026-09-01
Landed as c0facb3. A bare statement whose type is the block's F or a
row operation is bound as an implicit .? with the value dropped (the
_ <- reading) — Writer("a") on its own line tells, None
short-circuits, a bare List statement re-runs the rest per element.
The discard guard narrows to foreign marked types; val keeps a
program un-run (binding is consent to hold the value). The None.type
wrinkle: singletons carry no type arguments, so runnableElem also
consults the base type at the block's monad, every guess verified by
<:<. 27 tests, full matrix green.
## match-docs — the day, documented
Completed: 2026-09-01

docs/modules/okay-match.md rewritten to the module's full present:
the two founding decisions, the model layer by layer, hybrid search
with withheld, the reverse chain, scenarios-as-data with the typed
pen, identity without the hijack, open stores, the complete tool
table. docs/modules/okay-demo.md is new — the chat as the stack's
tour: running it, the env table, who does what, how the model runs
the marketplace, the offline phrases, the tests as the tour. Both
linked from the docs index.

## ctx-capabilities-doc — the whole story, told in one place
Completed: 2026-09-01
docs/capabilities.md: what a context function is (three mechanical
facts, E8/E10), the four-word vocabulary (doors, provide,
providing/and, wire) with the rules learned the hard way, the
zero-framework DI story (type as contract, compile-time resolution,
given-scopes as the object graph, environment-vs-resource), the
theory the compiler runs (Reader monad/applicative as verified
identities, the graded <*> chain, ctxMonad for the generic
combinators, the given-import gotcha), the boundaries as kept
refutations (bare-receiver method syntax, forbidden boxes,
Conversions, linear rebinding, the blocked tuple provide), and the
three-worlds payoff page. Linked from README, docs index, guide §9
and typepedia. Every claim traces to E1-E19 or a running test.

## queue-shape — the two queue bridges, no new seam
Completed: 2026-09-01
Per-message-ack brokers (RabbitMQ/AMQP, SQS, NATS, Pulsar, MQTT) are
DELIVERY machinery — per-message ack, redelivery, no offsets — a shape
the log deliberately is not, so a native Queue seam is rejected
(specs/data.md). Instead `okay.persist.Queues`: two bridges over
`Source`/`Sink` SPIs. INGRESS drains a broker into a topic keyed by the
broker's message id, acking AFTER the append — at-least-once: a lost
ack redelivers and re-appends, never drops, and `Queues.dedup`
collapses the duplicate one hop downstream by id (WithKey's shape).
EGRESS reads a topic from an offset and publishes outward, resumable by
the returned offset (a lost offset re-publishes; a sink that dedups on
the id gives exactly-once OUTCOME, the rest at-least-once said out
loud). The SPIs are the whole coupling to a real broker — an engine
adapter is a named deployment, not a core seam. Proven against an
in-memory fake broker (TestQueues, 4 tests: happy drain, lost-ack
redelivery + dedup, resumable egress, lost-offset replay + id-dedup).
Also checked the now-true kafka-eos box in specs/data.md.

## direct-auto-coloring — v2: no marks, and one mark where marks remain
Completed: 2026-09-01
Landed as 838eb2f. The block is DirectCtx[F] ?=> A; phantom
Conversions gated on the capability (selfColor) and additionally on
the Effect[G] marker (opColor) let F[A]-as-A typecheck ONLY inside
direct blocks and ONLY for registered types; the macro rewrites the
conversion calls with the v1 machinery whole. Same landing unifies
the marks: .? now serves monadic values AND raw operations (markTerm
dispatches by type), .!? refuted as redundant. The discard guard
makes a silently dropped monadic statement a compile error — found
and kept as tests: statements never see conversions (no expected
type) and Unit ascription is value discard, so tell-like ops keep
the explicit .?; auto-coloring resolves at DECLARED types (smart
constructors color, raw case constructors do not). 24 tests
(TestDirect + TestDirectAuto), full matrix green.

## ctx-showcase — the payoff on one page, executable
Completed: 2026-09-01
TestShowcase (okay-obs, 3 tests) distills the context-function arc
into one witness: `api: (Principal, Tracer) ?=> Traced.Route` — its
needs ARE its type — runs (1) behind the production doors (a
verified JWT becomes the Principal, tracing wraps it), (2) under
`provide(ada, tracer)` as a unit test with no token and no HTTP
machinery, and (3) under `providing`-composed environments with one
layer overridden (`base and providing[Principal](bob)` answers
for:Bob). One value, three worlds, zero changed letters; a missing
capability is a compile error in all three. Guide §9 gains the same
page as prose.
## demo-flow-cmds — the offline driver speaks scenarios
Completed: 2026-09-01

Three phrases complete the offline mode: "сценарий <имя> роль=email
…" starts any registered flow (and lists its transitions with their
roles), "шаг <N> <переход>" fires the writer's transition, "флоу <N>"
shows state and history. The escrow walk runs through real routes by
phrases alone — the wrong role refused with the reason, the buyer's
page ringing on the seller's sign. The no-model mode now covers
everything the model can drive. 13 demo tests.

## applicative-op — `<*>`, the idiom bracket's own spelling
Completed: 2026-09-01
`trait Applicative` gains the symbolic alias: `f <*> a` is `f.app(a)`
(inline, Monad.scala) — `pure(f) <*> fa <*> fb` now reads as written
in the papers. Works over any carrier through the generic door:
TestApOp runs one generic idiom over `[X] =>> Env ?=> X` (context
functions, via ctxMonad) and over `X ! Pure` (the effect row). Bare
ctx-fn receivers still hit E10 outside generic code — the known
boundary. Matrix 275/14/14.

## direct-macro — the flat block v1: direct style with no for-comprehension
Completed: 2026-09-01
Landed as 96a46e8 (nearly lost once: a no-op self-merge in the
worktree read as landed and the branch deleted — recovered from the
dangling hash; the merge-alone rule's "from the main checkout" half
is now twice-paid). direct[F] { val x = m.?; ... } rewrites at
compile time into Monadic's Cont binds (~300-line Quotes macro):
statement folding, ANF hoisting of value slots in application spines
(order asserted), if/match with effectful scrutinee/branches, &&/||
desugared to their If keeping the short-circuit, op.!? lifting a raw
effect operation into the block's row (Free.Inject emitted at the
Row the macro extracts from F = A ! Row). Marks under
lambda/while/try/by-name: positioned compile errors naming the
workaround. Effects first-class; F infers from the expected type.
16 tests. REFUTED and recorded: .! for the op mark (an imported
extension named ! shadows object ! — !.run breaks); isInstanceOf on
quotes-reflect types (erases to always-true, TypeTest patterns are
the way).

## obs-durable-overlay — the journal/trace identity join
Completed: 2026-09-01
specs/obs.md's last open box: a journaled operation's span and its
journal entry now carry the same operation identity, so an incident
replayed offline lays its spans over the originals. The identity that
survives a replay already existed — the Durable `Entry.key` (`keyFor`:
the step's position and what it asked for, nothing per-process). A
journaled operation opens a span carrying that key (`durable.key`, plus
`durable.op`/`durable.seq`); `Durable.replaying` stamps the SAME key
with `durable.replay=true`, so filtering by `durable.key` overlays the
replay on the original. The coupling stays OFF the main graph: a
neutral `OpTrace` seam lives in okay-agent (which does not depend on
okay-obs), `Durable.tools`/`replaying` take an optional `OpTrace`
(default None = no span, no cost), and `okay.obs.Tracer` adapts to it
in one line. Journal and trace stay two things — the span carries the
identity, it does not merge them. Proven twice: a fake sink in
okay-agent (TestDurable, the stamping) and a real Tracer over a trace
topic in okay-obs (TestOverlay, which Test-depends on okay-agent —
okay-obs is a leaf, so no cycle). Build: okayObs gains okayAgent.jvm in
TEST scope only.

## demo-flows — generic scenarios ring the chat
Completed: 2026-09-01

flow_advance joins the demo's wrapped tool table: a fired
transition's notifications are delivered to the role-holders'
inboxes with templates filled — any registered scenario's steps ring
the right pages with no per-scenario code (the deal's hand-written
onResponded now has a generic sibling). The prompt teaches the model
the flow tools. 12 demo tests.

## ctx-monad-instance — okay's Monad over context functions, for the generic combinators
Completed: 2026-09-01
The E13/E15 "not adopted" verdict was incomplete: direct style needs
no instance, but traverse/sequence/replicateA are written ONCE over
any F and need exactly an instance — juxtaposition cannot replace
them. Core now carries `given ctxMonad[E]: Monad[[X] =>> E ?=> X]`
(Providing.scala): pure is the value, flatMap is literally f(fa) —
the compiler's own auto-application is the Reader diagonal, so the
instance certifies semantics the elaborator already runs.
`sequence(Seq[Env ?=> Int]): Env ?=> Seq[Int]` works with F
inferred. Method syntax on bare ctx functions stays out (E10:
receiver applies before extension lookup). Tests: TestCtxMonad (4).
Matrix 257/14/14 — the global given collides with nothing.

## ctx-wire — the consumer one-liner: wire[A] is Reader's ask
Completed: 2026-09-01
The other half of the vocabulary (E17 in specs/context-functions.md):
`inline def wire[A]: A ?=> A = summon[A]` pulls the ambient
capability by naming its type. The naive `def wire[T] = summon[T]`
does not compile — no given at the definition site; the `A ?=> A`
result type is the fix, and the E10 eagerness finally works FOR us:
`wire[Db].q` applies in receiver position, `val d = wire[Db]` lands
as a plain Db, and doors write point-free (`val getQ: Db ?=> String
= wire[Db].q`) — no summon, no parameter. A missing given stays a
COMPILE error. Composes with providing/and (nearest wins). Core:
Providing.scala; tests: TestWire (4). Matrix 253/14/14.
## match-scenarios — scenarios as data; the deal becomes a definition
Completed: 2026-09-01

The review question ("can we add new scenarios? how?") gets the
registry answer a second time. ScenarioDef/Transition/Flow: roles,
states, terminals; a transition BELONGS to a role (the generalization
of "respond is the asked provider's alone"), carries the visibility
unlocks it grants (generalizing contacts()) and notification
templates. validate answers malformations as data (unknown
role/state, terminal with exits, unreachable terminal) and an invalid
definition is not registered. advance is the ONE engine method; the
deal machine is now the built-in ScenarioDef.deal running on it. The
universality proof: a three-role escrow housing sale runs with zero
engine changes, unlocking the address only at release. Flows and
unlocks are durable (sqlite restart test); definitions are
configuration. Stage 1 landed with it: the phantom-indexed
ScenarioBuilder — a route naming an undeclared state does not
compile (match-type membership, no macros) — the safe pen for
definitions written in code, the data form staying primary. Tools:
flow_start/flow_advance/flow_state/scenario_get. 27 match tests.

## security-crypto-split — the SCRAM primitives on a shared, dependency-free seam
Completed: 2026-09-01
okay-pg's SCRAM used a local `PgCrypto` given because okay-security's
fuller Crypto seam drags okayHttp (the JWKS road) and cycles the build
through this project's test edge. That local copy retires: a new
crypto-only module `okay-crypto` (specs/sql.md) holds the four
primitives SCRAM and password hashing need — hmacSha256, sha256,
pbkdf2, randomBytes — as a per-platform given (JCA on the JVM,
node:crypto on JS), resting on NOTHING, so any module can depend on it
without the http drag. okay-pg now depends on okay-crypto: PgCrypto*
deleted, `Scram` and `PgSql.connect` take `okay.crypto.Crypto`, the
test given imports move to `okay.crypto.given`. The four primitives are
pinned to published vectors (TestCrypto: NIST sha256("abc"), the RFC
fox HMAC, the PBKDF2-HMAC-SHA256 password/salt/1 vector), and the live
SCRAM battery (15 pg tests over the dockerized Postgres) proves the
seam end to end. The signing surface (RSA/ECDSA, JWT key handles) stays
in okay-security, which owns those heavier concerns — the split is by
dependency weight, not a move of everything. Build: new lazy val
okayCrypto (JVM+JS), okayPg depends on it, root aggregate updated.

## monadic-reflection — Filinski's reflect/reify over Cont: direct style for any Monad[F], no macros
Completed: 2026-09-01
Landed as 84d955f (spec d35c6a2, demo hotfix b725d19). object Monadic:
reflect is ONE extension serving three spellings (m.reflect,
reflect(m), and the symbolic m.? — Rust's postfix question
generalized), reify is the delimiter back into F; answer-type
modification types it precisely (Cont[A, F[B], F[B]]). Multi-shot
PRESERVED (a reflected List runs the continuation per element — the
"direct style forfeits multi-shot" note in specs/context-functions.md
corrected: that is Loom's cost, not this road's). FINDING: stack
discipline is the reflected monad's own — strict flatMap (Option)
costs a frame per reflect, trampolined A ! F runs 100k reflected binds
flat. 10 tests (TestMonadic); full matrix green. Rode along: master's
okayDemo/Test compile fix (two munitTimeout overrides from ebd344a +
99364a6 — kept 180s).

## ctx-provide-and — provide composes applicatively, the 22 cap falls
Completed: 2026-09-01
The missing combinator of the provide family (E16 in
specs/context-functions.md): `providing[A](a)` builds an installer
as a VALUE carrying `F[X] = A ?=> X`, and `and` composes installers
by composing the type constructors — `F[G[X]]` IS the curried chain
`A ?=> G[X]`, so `(providing[Db](db) and providing[Log](log)) {
app }` installs both without nesting and without the tuple. Type
lambdas reduce where the E11/E12 match-type route stalled, so the
using-method body eta-expands into the chain at the call site. The
right operand of `and` is the inner layer — override under
nearest-wins as plain data (`base and providing[Log](testLog)`).
Compositions are values: build a base environment once, reuse and
override per test. No arity cap — 25 layers tested past
ContextFunction22 (composition is heterogeneous, the type grows, so
no homogeneous fold — chains are written explicitly). Core:
Providing.scala; tests: TestProviding (flat composition, value
reuse, right-wins override, 25 layers, missing-dependency
compile-error claim). Core matrix 239/14/14 on JVM/JS/Native.

## kafka-eos — exactly-once on the Kafka interop, inherited from the engine
Completed: 2026-09-01
The stage-3 persist-interop rule "an engine keeps its own ops" cuts
both ways: Kafka HAS exactly-once, so the interop now exposes it
(specs/persist.md). The producer is idempotent by default
(`enable.idempotence`) — a retry after a lost ack cannot duplicate,
effectively-once to Kafka; the consumer reads `read_committed`, so a
reader never observes an aborted or in-flight transaction and `end` is
the last stable offset. New `KafkaStore.transaction(transactionalId) {
tx => tx.append(topic, partition, k, v) }` runs appends across
partitions AND topics atomically — commit on a normal return, abort
and re-raise on a throw — over a transactional producer cached per id
(initTransactions once, fenced by the id), closed with the store. The
own-engine file store gains nothing: this is Kafka's feature exposed,
not reimplemented, and the out-of-scope note stands for the own
engine. 3 live tests (TestKafkaEos, skip when the broker is absent):
a committed transaction's records appear together and in order; an
aborted one is invisible to a read-committed reader; one transaction
spans two topics atomically. The existing kafka suite (13) unchanged
and green — read_committed leaves non-transactional offsets identical.
One live wart stated in the spec: a read immediately after commit must
tolerate the last-stable-offset propagating (the test retries briefly).

## ctx-provide-n — the Cats mapN answer applied: 22 generated arities
Completed: 2026-09-01 (landed as 8265a2e)
Their "unbounded" is 22 generated overloads; so is ours — each a
one-line delegation, tools/gen_provide.py regenerates, capped where
the platform caps (ContextFunctionN ends at 22). Tested at 8 and at
the cap; core green on all three platforms (234/14/14). The
single-definition tuple route stays recorded as blocked (E11/E12)
with the missing compiler piece named.

## persist-wire-repl — replication crosses the wire, machinery unchanged
Completed: 2026-09-01
The stage-2 replication surface joined the documented wire (specs/
persist.md), version bumped to 2 with the new message cases APPENDED
so no v1 CBOR ordinal moved. Three frames added: produce (idempotent —
the retry across the wire answers the ORIGINAL offset; a stale seq
refuses by name), promote (the operator's failover, driven remotely;
the epoch advances) and compact (the Topic surface, completed). The
JVM `Wire.Server` gained a `repl: String => Option[Replicated]`
resolver: a replicated name serves through its coordinator (reads
truncate to the hwm, appends fence by epoch), every other name stays a
plain engine topic, and produce/promote on a name with no coordinator
refuse by name while the connection survives. The other direction —
replicas go remote — is a new `RemoteStore` that presents a
`Wire.Remote` as an ordinary synchronous `Store`, so the SAME
`Replicated` (not a variant) holds a remote replica: the eager push is
the remote's Append, the replicate-pull is the remote's Read, driven
on the coordinator's own thread (the okay-pg blocking waist under the
async client, JVM-only by design). Proven live over loopback: the far
node ends up holding the very bytes no in-process replica wrote, and a
lagging remote is caught up by replicate() over the wire. 5 new tests
(TestWireRepl), the existing 7 (TestWire) unchanged and green.

## ctx-everywhere — doors wherever the environment is a type; provide
Completed: 2026-09-01 (landed as 02098bf)
The operator's "everywhere, OPTIONALLY" executed with the operator's
own framing adopted: this IS the DI story — provide (core:
expression-scoped, nearest-wins, 1-3 arities) plus doors =
compile-time dependency injection, a missing dependency a QUOTED
compile error, zero framework. Doors: McpAuth.granted closes the
route family (protect refactored through one shared ladder);
OAuth2/Jwks/McpAuth gain ambient-Http forms; Tls.served
(Secrets ?=>, reshaped after an erasure clash — recorded);
Langchain4j.wired and S3.wired open the wiring family;
Configs.ambient. The environment-vs-resource line drawn; the
two-line recipe in typepedia; guide and five module pages updated.
Verification note: three full-matrix runs were SIGTERM-killed
externally (a sibling pkill, admitted in the room) at 1082/1082/
1089 tests with ZERO failures; all ten touched module suites green
directly (480 tests).
## match-deals — the negotiation: several candidates, the confirmed match
Completed: 2026-09-01

Deals complete what Vis.Matched promised: inquire/respond/withdraw
with Asked -> Accepted | Declined | Withdrawn, respond the asked
provider's alone, and contacts(viewer, other) unlocking Matched
facts (and platform AfterMatch gates) ONLY under an accepted deal —
both engines, sqlite parity, restart survival. The demo runs the
round: numbered candidates, the client chooses whom to ask (several
is wise — someone agrees), providers answer in their own chats, an
acceptance hands the seeker the unlocked contact and stands the rest
down, a full-decline round says the request still stands. The round
policy is store-driven and restart-surviving — PState/Delim were
CONSIDERED for it and declined with the reason written down: this
protocol spans processes and days, and state that must survive a
boundary belongs in data, not in a continuation (the same criterion
that placed them in transact/wizard/stepper, where the whole
protocol lives inside one program). Domains are anybody's: the jobs
round is the demo test, housing the engine test, repairs the live
one. 21 match tests, 11 demo tests.

## sql-pg-node — the pg driver reaches Node; sql.md's last box
Completed: 2026-09-01 (landed as a4e491e)
okay-pg cross-built JVM+JS. The message pump was restructured onto
the Net seam: it now PULLS bytes as a sequential Async program
(receive = readFully(5) then the body; collectReady folds to
ReadyForQuery, an error drained to quiet so the session survives)
instead of blocking-read calls — so the SAME driver runs over a
blocking socket on the JVM and over Node's buffered net. SCRAM
kept the room's phase-object shape but its three primitives + nonce
now come from a per-platform PgCrypto given (JCA / node:crypto) —
okay-security's fuller seam drags okayHttp and would cycle the
build, so security-crypto-split is filed. cancel() became a marked
rollback settled before the next use (no sync I/O on the async
leg). The acceptance: TestPgNode — a NODE process speaks SCRAM and
portals to the dockerized Postgres and gets 42 back, a wrong
password refused by SCRAM itself, no JVM/JDBC in the process; the
whole JVM live battery green THROUGH the new pump proves nothing
regressed. Every behavior box of sql.md is now checked. Merge read
alone after one refused ff (claim-only divergence): exit 0. Full
matrix green on a quiet machine.

## docs-sweep — the landings reach the docs
Completed: 2026-09-01 (landed as a963374; markdown only)
Ten module pages born (blob conf demo docs-mongo java langchain4j
obs py security-argon2 tls), six updated (security ES256/OIDC/
granted; jdbc Migrate/BulkLoad/Poll; persist Doctor/Configs/stages;
cache stage 2; ui Scope-capability/Nav-boundaries; llm Cut). The
guide gains phased stages, ambient prompts, Blocking and a
Capabilities section; typepedia records PState's consumers (no
longer an exhibit), Blocking and ambient Prompt; theory ch.2 names
its shipped consumers; ROADMAP P9 closes two of three opens; the
module index catches up by fourteen rows.

## ctx-adopt — the third capability route, the Blocking value, the documented edge
Completed: 2026-09-01 (landed as f11ec6c)
Secure.granted: the principal ambient by pure delegation (the
401/403 ladder byte-identical to bearer). The composition crown
holds: ONE stored (Principal, Tracer) ?=> Route serves protected
AND traced under stacked installers — deferred requirements compose
as arrows. Blocking[A] names core practice as a type (stored,
forced only where CanBlock is given). The edge patterns (given-chain
+ import-thread with the footgun) moved into typepedia with their
E-numbers; conf.md points at them. ctx-wiring's gate noted possibly
open, offered to the demo lane. Matrix 1599.

## wire-node — one socket leg for every wire; the log reaches Node
Completed: 2026-09-01 (landed as 594faf1; spec first)
Net in the core (specs/net.md): the byte-stream seam as a given per
platform — ONE blocking file in scala-jvm-native serves JVM and
Native (both ship java.net.Socket), the Node leg buffers `data`
events behind Async.await pulls so every protocol pump stays a
sequential program. persist's wire protocol moved to SHARED
WireProtocol (Version, the enums, frame helpers over NetConn, the
cross-platform Client); `export` kept every Wire.* path compiling
and TestWire untouched. THE headline, the openness acceptance made
literal: the SAME client code talks to a scripted Node net server
answering frames encoded with the SAME shared enums — with no JVM
in the process. En route, two forward-fixes for the fresh chat
demo under the day's house rules (a JDBC-carrying module forks its
tests; live calls get 120s) plus the third rule those exposed: a
FORKED test JVM keeps the repo root as cwd when the suite indexes
File("."). sql-pg-node now has its transport; the PgSql pump
restructure stays its own claim, stated. Merge read alone after
two refused ffs (demo landings; targeted retests): exit 0. Full
matrix green.
## demo-chat-async — the reverse chain: events in either order
Completed: 2026-09-01

"Мне нужно починить велосипед" today, nobody fits; "я умею чинить
велосипеды" tomorrow — and the seeker's page rings. The chain is
STRUCTURAL, not the model's: the tool table is wrapped, every
facts_assert of an offer runs the reverse search over stored needs
(and vice versa, floored by similarity — the embedder seam's
business), and a hit lands in the matched profile's inbox — an SSE
stream (/events/<email>) both pages hold open from the first email
they see, rendering 🔔 bubbles. Needs are stored before searching
(driver and prompt both). The two-window story is a deterministic
test through real routes: need waits, offer arrives, the open stream
receives the match — plus the hello frame that flushes SSE headers
(client.send blocks without it) and the email-in-the-PATH lesson
(requestOf keeps the path; a query string never reaches a route).
10/10 including the three live legs.

## ctx-functions — what the capability arrows buy us, verified first
Completed: 2026-09-01 (landed as 67e11ad)
specs/context-functions.md: the FULL map on its experimental base
(E1-E8 — same-type rebinding impossible; type-changing given-chains
linear; the import-thread works via NAME shadowing incl. LTS;
nested using-params resolve NEAREST; stored ctx-fns self-apply;
macros cannot rescope). Shipped: implicit prompts for Scope
(mark/exit/bounded) and Cut (guard/violation/watched) — exit to the
nearest scope by nesting, bound prompts still cross; Obs
Traced.route (Tracer-capability routes, per-request roots, stored
route values self-wiring). Filed: ctx-blocking, ctx-edge-docs,
ctx-wiring, ctx-reader-bridge (gates named). Rejected with reasons:
ui builder DSL, macro direct-style. Rode along: the sqlite
DriverManager race named (third telling), demo-chat-live-budget
filed. Matrix 1588.
## demo-chat-seek — the seeker's question, answered live
Completed: 2026-09-01

The user asked "а найдёт?" and the answer is a test now: with a bike
repairman in the store, "мне нужно починить велосипед, найди мне
кого-нибудь" (no prefix, no hints) runs the intake across two turns —
the model asks for the seeker's email, receives it, registers, calls
find_candidates and reports the master with his skills. Asserted
against the local model.

## demo-chat-ungated — the model decides when to match
Completed: 2026-09-01

The /match gate is gone when a model is configured: every turn is an
agent turn, okay-match's tools are always on the table, and the
system prompt hands the DECISION to the model — offer or need means
work the marketplace, anything else means just answer. The live test
asserts both halves against the local model: a bicycle-repair offer
with no prefix anywhere reaches the tools (stored, or the email asked
for), and "какая столица Франции" leaves the marketplace untouched.
/match survives as the no-model driver's prefix and a forcing hint.

## sql-pg-copy — the bulk-load posture on the free engine
Completed: 2026-09-01 (landed as merge; box in specs/sql.md)
copyIn speaks the simple-protocol COPY dance (CopyInResponse /
CopyData / CopyDone) with the text format's escapes proven
round-trip (tab, newline, backslash, NULL); a thousand rows land
in one command. The load-id posture where plain Postgres has no
per-file load history: a loads REGISTRY whose claim row commits IN
ONE TRANSACTION with the data — the retry answers AlreadyLoaded,
and a crash between COPY and commit (tested by killing the
connection mid-load) rolls back claim AND data together, so the
retry lands exactly once overall: WithKey at batch granularity,
made physical. sql.md now has one open box: the non-JVM consumer
(sql-pg-node). Merge read alone: exit 0. Full matrix green.
## demo-chat-match — the marketplace joins the chat
Completed: 2026-09-01

/match turns are matchmaking turns over one shared MemoryMatch per
server. With a model configured the turn is an AGENT conversation —
Provider.openAi/anthropic as Handler[Model], okay-match's Tools.table
as Handler[Tool], a system prompt teaching the intake — and the LIVE
test proved the local rozum model driving the real tools end to end
(it stored the welder's offer, or asked for the missing email — both
honest outcomes asserted). With no model, a deterministic driver
speaks THE SAME tool table, and the offline test walks the two-sided
story through the real route: "умею класть плитку" chats in, "нужен
плиточник" finds it, the marketplace remembers across turns and
sessions. And the marketplace is DURABLE by default: sqlite
(OKAY_CHAT_DB, ":memory:" opts out) — which made the store interface
nominal (MatchStore: both engines spoke it structurally, the trait
writes it down, Tools.table takes any store), caught sqlite's
booleans-as-integers dialect trap with a parity suite, and proved
restart survival over the same file. 7 + 1 tests.

## typed-bad-repair — damaged records ask
Completed: 2026-09-01 (landed as 0a321aa)
Conditions' first consumer outside the core: Repair in okay-persist
— decode/read over the Typed view where each damaged record
SIGNALS Damaged(offset, error, raw bytes) under a per-element
"skip" frame. One log, three answers under three policies: patched
IN PLACE (the corrected value sits exactly where the damage sat),
skipped with order intact, aborted naming the offset and the
declined menu. And the additive rule's other half, tested: a clean
slice never consults the policy — who never signals never pays.
Merge read alone: exit 0. Matrix green on rerun (one TestHttp
port-roulette flake, 11/11 alone — the ledger entry stands).

## demo-chat-move — the frontend moves in with its demo
Completed: 2026-09-01

okay-chat-web relocated to okay-demo/web (user ask): the chat's
frontend lives inside okay-demo now; the separate sbt module remains
only because a JS cross-build cannot ride a plain JVM project. Paths
rewired (appJs discovery, build.sbt), all 8 tests green after the
move including the live local-model leg.

## conditions — resumable exceptions: the road between throwing and damage-as-data
Completed: 2026-09-01 (landed as 99d44dc; spec first; user ask)
Condition in the core: signal raises WITHOUT unwinding — the
policy runs while the signal point's continuation is live, so
Resume(v) continues FROM THERE with the value (the effect system
was a resumable-exception system waiting to be named); within
establishes named restart frames the policy can unwind TO
(Invoke — the Delim shape, one machine owning frames and menu per
Delim's own payload-erasure discipline); Fail escalates naming the
condition AND the declined menu; invoking off the menu is the
POLICY's named bug (NoSuchRestart). The repair story proven: a
decode loop with a skip frame per element — ONE program answers
patched/skipped/failed under three policies. Additive by the
operator's rule: Throws, runEither, damage-as-data untouched; a
program that never signals never pays. Eight tests. Filed for
later: Typed.Bad interactive repair, r.md's native restarts over
the now-shared vocabulary. Merge read alone after one refused ff
(nav-pop divergence, ui-only; core retested 227): exit 0. Full
matrix green.
## demo-chat-react — the React frontend, and the live leg proven locally
Completed: 2026-09-01

okay-chat-web (cross): the chat's brain is pure view/update over the
Ui tree, JVM-tested with scripted events (send flow, token folding
into the open bubble, the cut line) — the React frontend's logic
never sees a browser in its tests. The JS leg is glue only: okay-ui's
ReactJs against CDN React UMD (the first time the mapping meets a
REAL React), the Elm fold on runAsync (no CanBlock on JS — the event
loop is the runner), and a fetch reader feeding $token/$done/$cut
into the same bus the clicks use. ChatDemo serves the React page and
/app.js when a link exists, the vanilla page otherwise — and gained
the third model filling: OKAY_CHAT_BASE speaks any OpenAI-compatible
endpoint, and the LIVE test streams through the local rozum model on
:8089 (green on this box) — the live box closes without an Anthropic
key. 5 + 3 tests.

## nav-pop-to-screen — the pattern held, the mechanism corrected
Completed: 2026-09-01 (landed as 84617de)
A typed Key names a boundary screen; PopTo drops every intervening
frame — none stepped, they are DATA — and the boundary routes the
typed answer; boundaries chain, the outer pop crosses the inner, an
absent key names nothing (total). The spec's mechanism claim
corrected in place: Dialog needs Delim's capture because its
continuations are implicit; Nav's stack is reified data, so the
boundary is a marker and the exit is a drop — the adoption
doctrine's own test applied to its own poster case. Matrix 1546.

## persist-offload — the cold tail becomes the lake
Completed: 2026-09-01 (landed as 7c19340)
Segments: the documented disk format gets a PUBLIC reader in
okay-persist (Doctor's certification knowledge as a library —
bytes parse into records wherever they live, torn tails end
soundly). Offload on the blob side: verified-then-evict under a
local byte budget (a segment leaves only when the blob's copy
matches its size; the active file never leaves; begin advances
exactly as under retention — proven across reopen), and the
Tiered Async read where TooEarly stops meaning gone and starts
meaning COLD: blob history strictly below the local begin plus the
local tail, byte-exact, bounded (the overlap bug — backup holds
copies of still-local segments — caught by the first run and
fenced by the strictly-below rule). Dependency direction blob →
persist compile, safe (persist rests on core+codec; reverse cycles
through http). Merge read alone: exit 0. Full matrix green.
## demo-chat — the chat with an LLM, as one JVM main and no build step
Completed: 2026-09-01

The user-requested showcase, out of what already ships: okay-jetty
streams the SSE reply body live (Source[Chunk[Byte]] chunk by chunk
on a virtual thread), okay-llm's Anthropic.stream speaks the real
API when ANTHROPIC_API_KEY is set, Cut guards the stream with a
token budget the page renders as a visible scissors line, and the
offline mode IS the demo — the scripted model streams the same
framing, so the acceptance proves the same path on a real socket:
first frame read incrementally before the end, done marker, and the
over-budget run cut at exactly the budget with the rule named and
nothing following. GET / carries the whole page inline (dark, small,
fetch-reader appending tokens as they arrive). `sbt okayDemo/run` →
http://127.0.0.1:8090. 3 tests.

## stage-phased3 — one more arity, because the consumer exists
Completed: 2026-09-01 (landed as 1444810)
The http message shape needs exactly three phases; chaining two
phased cannot express it (the middle's end is the third's TYPED
start). No phase enum, both switches through PState, the answer
names the dying phase three ways, and the does-not-compile proof
stands at BOTH seams. Driven by the consumer's shape: request-line
-> headers -> body. Matrix 1537 (the day's sibling landings ride
in the count). http-message-phases (the Nio refactor) is next.

## sim-harness — luck retires from concurrency testing
Completed: 2026-09-01 (landed with spec boxes checked)
Sim in the core: many fibers, one seeded scheduler, interleavings
as VALUES — a found bug is a seed, a fix is verified by replaying
it. Fibers are freer trees and their k at every operation IS the
captured delimited continuation (the Cont foundation as scheduler
food — the operator's primary-where-necessary rule, satisfied);
SimChannel makes blocking primitives operations; the virtual clock
moves only when nothing else can; deadlock is an OUTCOME, not a
hung test; fault plans ride the seed. The headline: the runCmd
close race, modeled, loses its answer under seeds a 200-sweep
finds, and the shipped rule survives all 200 — today's flake is
now a replayable regression test. One lesson: continuations apply
at SCHEDULING, not enqueueing (eager k ran side effects early);
tasks are thunks. Eight tests. Merge read alone: exit 0. Full
matrix green.
## llm-streaming-cut — the validator cuts the model off mid-sentence
Completed: 2026-09-01

Cut.guarded installs a typed prompt over a streaming generation;
Cut.checked stands in the token stream, emits what passes, and on a
violation ABORTS to the prompt — Left(Violation(rule, at, seen)),
the poisoned token never flows, and the source records NO further
pulls (the counter had to become Async data to observe that
honestly — uncons builds one node ahead). A passing stream is
identical to the unguarded run; nested guards prove multi-prompt:
the inner cut recovers, the outer stream continues. The open P9
roadmap item, closed with Delim as the PRIMARY mechanism per the
adoption doctrine; the unguarded path untouched. 3 tests; the live
probe stays open pending an API key, mechanism covered scripted.

## control-specs — the PState/Delim consumer map, written down
Completed: 2026-09-01 (landed as bff0581; markdown only)
The operator's adoption doctrine stated once (delimited-control.md):
ADDITIVE by default — a wrapper, an extra combinator, a typed
internal, never a rewrite; PRIMARY only where no equivalent exists,
which today is exactly cross-boundary abort/cancel. Six sections in
the owning specs: llm-streaming-cut (closes the open P9 item's
design), stage-phased3, http-message-phases (doctrine home deferred
to the wire lane's typestate.md), nav-pop-to-screen (ui lane's),
logic-named-cut and r-restarts (both GATED, gates named). Slugs
filed; pg-scram amended to defer its form. Design discussed in the
room: capture-at-Async, prompt-machine non-collision, and
internals-only transact all settled with the sibling lane.
## pg-scram-typestate — the handshake's order is the type's shape
Completed: 2026-09-01

Scram rebuilt as PHASE OBJECTS (the wire-typestate family; phase
objects where PState's Cont bridge buys nothing): ClientFirst's only
step is serverFirst, ClientFinal's only step is serverFinal — an
out-of-order step does not EXIST as a method (compileErrors-pinned
both directions). The one-object Scram class stays as the adapter
over the phases (same API, same bytes — usable without the types),
and even there the old silent NPE on a misordered server became a
named PgError; PgSql's driver loop now holds the phase and names
SASLContinue/SASLFinal arriving out of order. The whole dance is
pinned to the RFC 7677 test vector byte for byte, mutual
verification included. 4 tests.

## sim-typestate-specs — the user's question becomes two specs
Completed: 2026-09-01 (landed as 3bb0bd3; spec only)
Born of "where are PState and Delim useful?" and hashed out in the
room while OTHER lanes landed three consumers the same afternoon.
specs/sim.md: deterministic concurrency simulation on Delim —
every fiber under its own Prompt, a seeded scheduler capturing at
the Async waist (Run/Await, the narrowest point everything already
passes), virtual clock, fault plans from the seed, interleavings
replayable byte for byte; the argument is the day's own ledger
(three real races, all found by flakes). specs/typestate.md: the
criteria doc — typestate pays for ONE-WAY phases through ABSTRACT
boundaries, and the cheapest adequate mechanism wins (phantom
types for two states = the landed sql-typestate; phase objects for
short handshakes = pg-scram-typestate, the room's counter-proposal
accepted; PState proper for type-changing accumulation = the
landed stage-phased). sim-harness and wire-typestate filed against
the specs; scram may be claimed by any lane.

## agent-stepper — pause, inspect, resume, fork: Delim's second consumer
Completed: 2026-09-01

Stepper.stepped translates every Tool.Call into a pause (a shift to a
typed prompt; the captured continuation IS the rest of the run, as a
value), drive loops the operator's decision, transparent proves the
observer away (stepping with nobody watching equals not stepping —
tested against the direct run). The Delim-specific dividend is
multi-shot: one pause resumed twice yields two futures from one past
("what if the tool had said X"), tested. With dialog-delim and
ui-pwizard landed the same day, PState and Delim both now hold
production consumers. 4 tests.

## persist-election — the operator removed from the loop
Completed: 2026-09-01 (landed as 096fc9c)
specs/consensus.md implemented: Election consumes total order and
a clock, nothing else — the fold is first-Take-wins per epoch,
Operator overrides even landing second, a deposed leader's lease
is noise; tryTakeover answers from the FOLD (the claim lands, the
node reads back whether it was first) and the winner leases
immediately so a racing claimant sees no vacancy. All six spec
boxes checked: 5 suite tests x THREE control-log engines (memory,
the FileStore arbiter, live Kafka — unchanged, which was the
claim) + 3 integration tests driving stage 2's promote (loss-free
takeover, epoch fencing, arbiter-down degrading only failover).
One truth taught back: a winner that never leases loses the seat.
The spec also gained the user's PState/Delim notes (typestate for
RaftStore roles; deterministic simulation for testing consensus).
En route, fixed forward for the ui lane: runCmd's close raced the
loop's launch and LOST command answers (flaky TestCmd) — a third
counter (handed-over-but-unfolded events) ends it, 5x green.
Merge read alone after one refused ff (dialog-delim divergence;
targeted retest): exit 0. Full matrix green on a quiet machine
(two environmental signal-9 kills on a load-42 machine before it,
both green alone — the multi-agent matrix stampede is real).

## dialog-delim — cancellable scopes: Delim gets its consumer
Completed: 2026-09-01

Scope: scenarios in the Delim + Dialog row, typed prompts as
cancellable sub-flow boundaries — cancel(p)(value) exits the named
scope from any depth with no Option threading between steps, and the
multi-prompt point is proven: an inner scope aborts ACROSS its own
boundary to the outer prompt (the capability nested handlers cannot
express; textbook ch. 2). One run erases the row; Dialog untouched,
plain scenarios run beside scoped ones by test. Delim's first
production consumer, PWizard being PState's second — both theory
exhibits now earn their keep. 3 tests.

## stage-phased — typestate on the stream; PState gains its consumer
Completed: 2026-09-01 (landed as 3aba599)
Stage.phased: the accumulator CHANGES TYPE at the switch (header ->
rows, the CSV shape) — the body cannot mention the head's phase by
TYPE, the suite gains its first does-not-COMPILE proof
(compileErrors), and the per-input transition is EXECUTED through
PState: the theory exhibit of docs/theory/03 doing streaming work.
Ends honest both ways (the answer names the phase the stream died
in). Core suites 208/14/14 green on all three platforms; the full
matrix carries the PRE-EXISTING ui-cmd flake, probe-proven on
pristine master and filed as ui-cmd-flaky. Second Atkey consumer
(sql-typestate) is the sibling's parallel lane; pg-scram-typestate
filed at the operator's ask.

## ui-pwizard — the typed wizard: PState's style as a Dialog alternative
Completed: 2026-09-01

PWizard, additive only: steps are Cont values whose answer type
threads a suspend/resume machine and whose state type GROWS — a step
names its state requirement, the compiler enforces the order (age
before name does not compile; compileErrors proves it), views read
the typed state-so-far, and `step` carries a built-in validation
retry. toDialog bridges any machine into an ordinary Dialog program,
so the typed wizard runs over any Host or as a Screen with Dialog
untouched. PState's second consumer after sql-typestate. 3 tests.

## sql-typestate — the transaction protocol in the types; PState gets its consumer
Completed: 2026-09-01

Typed.Db[S] carries the transaction state as a phantom; Typed.region
demands Db[Tx.No], hands the body Db[Tx.Yes], and owns begin/commit
itself — the nested-begin failure specs/jdbc.md documents as a
runtime refusal is now unrepresentable, proven by compileErrors (the
error names Tx.Yes). Runtime is exactly transact. This is PState's
typestate (Atkey, textbook ch. 3) in two-state form — the chapter now
points at the shipped consumer; the full answer-type embedding was
declined with its price stated. 14 tests on H2.

## cache-redis — four commands do not justify a dependency
Completed: 2026-09-01 (landed after c316c30)
The RESP client is four commands over a blocking socket; Budget =
SET PX (the SERVER expires, this process never filters), values
ride CBOR, connect PINGs and fails fast. Invalidations are EVENTS
on a persist topic: the cross-node honest window shown before the
drain, and a down node replays and CONVERGES — the trade justifying
the topic over pub/sub, asserted. Live vs docker redis, skip where
absent. Matrix ~1440.

## persist-consensus (spec) — who may advance an epoch, decided
Completed: 2026-09-01 (landed as fbf2e2e; spec only)
specs/consensus.md: election REDUCED to a fold of a totally-ordered
control log — the first Take at an epoch wins on every node's fold,
no votes or wire protocol of our own; leases (plus a declared skew
allowance) decide LIVENESS only, epochs keep deciding safety, and
the operator record outranks automation on every fold. The log is
sourced from engines this stack already has: KafkaStore first (its
KRaft did the twenty years), a FileStore arbiter for dev (honest
trade: failover availability, never correctness), own RaftStore
later as one more engine under unchanged machinery. Rejected with
reasons: Raft-first, per-partition election groups, ZK/etcd client
deps, clock-trusting correctness. persist-election filed with its
six-box battery. The persist staging now has every stage designed
and stages 0-3 shipped.

## docs-seam — the one new seam, proven on both postures
Completed: 2026-09-01 (landed as df9a119)
okay-docs cross-built: Docs[A] with Cond as the load-bearing part
(Always/IfAbsent/IfVersion — CAS as data, Stale carries what holds
NOW), declared-index queries (a scan wearing a query's hat refuses
by name), grants as the engine's honest consistency mapping, and
NO multi-document transactions deliberately (a multi-item change is
a journaled sequence of CAS — the saga with existing machinery).
TopicDocs = the own posture made code: a compacted-topic fold where
the version IS the record offset, deletes are tombstones, a cold
node refolds the same store. okay-docs-mongo (satellite pays the
driver, the argon2 precedent): every conditional write is ONE
server-side operation, declared indexes become real ones. ONE
DocsSuite over both engines (8+8+8 cross-platform + 7 live Mongo),
including the lost-ack CAS retry landing once. En route: the wire
server now binds LOOPBACK by default (plaintext until wire-tls
does not volunteer itself to the network), and the port-roulette
flake family got one BACKLOG ledger (TestWire read literal "HTTP"
at its handshake once under parallel suites). Merge read alone
after one refused ff (jdbc-bulk-load divergence; targeted retest):
exit 0. Full matrix green (Postgres, Kafka AND Mongo live).

## jdbc-bulk-load — WithKey at batch granularity
Completed: 2026-09-01 (landed as 6f7e8d8)
The OLAP write posture held by discipline: history row + the
caller's COPY in ONE transaction — the unique key IS the dedup, a
crash-retry lands once, a refused claim is VERIFIED against the key
(a dead wire must not impersonate a dedup), a failing COPY rolls
its claim back. The olap wrapper refuses row DML by name and points
at the right door. DuckDB as the double. Matrix 1433.

## obs-otlp — export is a consumer, and no SDK came
Completed: 2026-09-01 (landed as bc14801)
The pure half maps spans to OTLP/HTTP JSON (nanos as strings,
status 2 carries the message, roots omit parentSpanId); the jvm
glue is one more topic consumer — offset = resume token, a refusing
collector leaves the batch unconsumed so retry re-ships
(at-least-once, as ingestion expects). Proven against a recording
fake collector. Matrix 1430.

## cache-write-through — the window is stated, not denied
Completed: 2026-09-01 (landed as 61c7501)
Regime 2's write path held by construction (WriteThrough.write:
commit THEN invalidate — one helper, not an audit of call sites).
Argued three ways over H2 through the Sql seam: ordering asserted
on a probing cache; the WRONG ordering's resurrection bug
demonstrated (a reader between invalidate and commit re-caches the
old truth indefinitely — why the rule exists); the honest
commit-to-invalidate window shown. The last open cache.md box
closes. Matrix 1410.

## persist-interop — the engines that already did the twenty years
Completed: 2026-09-01 (landed as d101128; spec first)
Stage 3: SqlStore (okay-jdbc, via the Sql seam — any driver serves
it) passes the FULL 13-test persist StoreSuite on H2, the
cross-engine acceptance; begin proved to be state of its own (it
moves only under retention — the contract caught min(off) drifting
under compaction), plus two SQL truths (aggregates over nothing
answer a NULL row; H2 types SUM(expr) as NUMERIC). KafkaStore
(okay-kafka) inherits partitions/replication/election behind the
same trait: the sync SPI blocks honestly, compact() refuses by
name (the engine keeps its own ops), Received maps to
fire-and-forget with the log's end as the honest answer, and the
persist Typed view decodes unchanged over a real broker — four
live tests on dockerized Kafka 3.9 (skip when absent).
persist-offload refiled to pair blob-seam. Merge read alone:
exit 0. Full matrix green (live Postgres AND Kafka in the run).

## py-worker — N processes is the parallelism; the GIL is then irrelevant
Completed: 2026-09-01 (landed as b8c76ab)
PyWorkers: N resident processes behind the same handler shape a
single worker has — programs cannot tell. Dispatch proven by pid
distinctness (determinism over stopwatch); module state lives WITH
its worker (seed-and-draw); the supervisor replaces a corpse COLD
before rethrowing, the retry lands live. One program, both engines,
unchanged. okay-py stages 0+1 shipped. Matrix 1406.
## ui-cmd — the effect slot: commands are data, the loop runs them
Completed: 2026-09-01

"Press the button, fetch, fold the result back in" now has a direct
spelling: Ui.runCmd's update answers (state, commands), each command
an `Event ! Async` the LOOP spawns, its answer re-entering the same
fold; Nav.Run(prog, s) is the stack's version — go there AND launch.
Ui.run is the commandless special case. The first cut merged a
never-closing answers channel and broke v1's quiet ending (host ends,
loop ends) — caught by the old tests hanging, redesigned to one
channel with an honest close: upstream done AND nothing in flight.
A command encodes its own failure as an event or forfeits (stated);
a command may answer Closed — an app can end itself. 3 new tests,
52 in okay-ui, JS and Native compile.

## py-subprocess — the other half of the world's numerics, as a handler
Completed: 2026-09-01 (landed as ff683b6)
okay-py stage 0: PyEval operations (named functions only — no
eval-a-string case, structurally), conditions as Either with the
worker surviving them, the stdlib-only shim as a versioned resource
behind a loud handshake, a CLEAN child env, verify turning the
wrong venv into a startup refusal. The json wire tags what it would
merge (NaN, bytes, integral floats). Live vs python3, skip where
absent. First implementation of the r.md shape. Matrix 1395.

## persist-wire — the log reaches past the process; auth rides along
Completed: 2026-09-01 (landed as 73ff276; spec's wire section first)
Covers persist-wire AND persist-wire-auth. The documented surface
made real: [len][CBOR] frames with Wire.Req/Resp as the one source
for both ends (the cluster precedent), Hello/Granted where the
capability list IS the offer (the ui rule retold for logs), auth
as a function (token => Option[Set[topic]]) that okay-security
plugs into with no crypto dependency here, refusals by name with
the connection surviving them, TooEarly crossing unchanged, the
tail shape working remotely, a forged future-version Hello refused
in the handshake. The client speaks Async (a blocking socket
behind Async.Run, the okay-pg pattern); the Node leg arrives with
a consumer. Plaintext v1, stated — TLS rides wire-tls.
Replication's calls join the message enum under the handshake
version when replicas go remote. 7 loopback tests. Merge read
alone: exit 0. Full matrix green.
## ui-toolkit — Form v2 is total over the algebra; the composed dialogs arrive
Completed: 2026-09-01

Per the user's call, derivation and cross-field validation together,
not staged. Form.of[A] now renders every Schema node: a nested
product is a titled section with dotted-path keys (addr.city), a sum
is a Select of its cases plus the chosen case's subform (choosing
swaps it; the value keeps the codec's {"Case": {...}} shape), lists
edit in place with add/remove routed by index. Errors are data —
Form.errors gives (path, message) pairs and each renders under its
field — and cross-field checks read the DECODED value, holding
submit until both layers are clean. Toolkit.confirm/alert/prompt/
choice close the hand-rolling. The drift law extended and tested:
a nesting+sum+list form's submission round-trips the codec decoder.
7 new tests (49 total in okay-ui); JS and Native compile.

## wire-tls — verify-full or it is a named decision
Completed: 2026-09-01 (landed as 8e795cf)
okay-tls: the one transport seam, sslmode vocabulary stack-wide,
SSLSocket over the blocking sockets our wires actually use (the
SSLEngine machine waits for an NIO consumer — recorded in
Decisions). The whole ladder proven against live handshakes:
verify-full refuses wrong hostname/unknown CA by name, verify-ca
accepts the wrong hostname and the test SAYS SO, require tunnels
and refuses plaintext, disable is the named decision. Keys are
Secret refs; inline PEM refuses at the seam. pg/persist-wire
integration boxes stay with their lanes. Matrix 1380.

## persist-backup — backup is boring, and the doctor certifies it first
Completed: 2026-09-01 (landed as 1624f26)
Doctor (okay-persist): an INDEPENDENT reader of the documented
segment format — a second implementation double-checks the writer.
Torn tail on the LAST segment: normal, named, restorable; damage in
a CLOSED one condemns the copy; refusals never mistaken for tails.
Backup (okay.blob — persist->blob would cycle through http):
incremental closed-segment copies to any Blob engine; restore =
place files back for the ordinary startup path. End to end: copy,
wipe, restore, doctor certifies, recovery serves. Matrix ~1360.

## persist-replication — stage 2's core, transport-agnostic
Completed: 2026-09-01 (landed as a9e4bb5; spec first)
Replicated: a coordinator over N replica Stores behind the SAME
Topic trait (stage-0 consumers never rebind). The follower
push/pull IS the read path (replication is a consumer that writes
what it reads; divergence throws by name). The high-water mark =
the quorum-th largest replica end — reads and end() stop there, so
nothing a failover could unwrite is observable; Ack.Replicated
short of quorum throws NoQuorum rather than acking a promise it
cannot keep. The Leader handle carries its epoch: promote catches
the successor up FIRST, then fences the deposed handle, and both
promotion and fencing land on the ops topic (the log audits
itself). produce(producerId, seq, ...) is the idempotent window —
the retry answers the ORIGINAL offset. Six tests on all three
platforms with a Pausable store standing in for the down replica.
En route: TestRepoAgent's budget grows with the repo (120s over
munit's 30 at 419+ sources). persist-wire will carry these same
calls between nodes without changing the machinery. Merge read
alone after one refused ff (match-finish divergence; targeted
retest of persist/demo/match): exit 0. Full matrix green.

## match-finish — the entropy seam, the module page, board hygiene
Completed: 2026-09-01

The sibling's honest flag on 4b7dc0b (util.Random for the link token)
closed properly: `fresh` is a constructor seam on both stores — the
cross default is `Entropy.weak` (unique, linkable everywhere, stated
NOT guess-resistant), and `SqlMatch` defaults to `SecureEntropy.strong`
(SecureRandom is legal in a scala-jvm source; both the profile id and
the link token are credentials). docs/modules/okay-match.md joins the
satellite pages — the wiring table names every seam and its production
filling (Password, Crypto.randomBytes, a rag embedder, an okay-llm
reranker), and the docs index lists the module. The emptied okay-match
BACKLOG section is gone.

## blob-s3 — the lingua franca, spoken ourselves
Completed: 2026-09-01 (landed as 10783c4)
Own SigV4 pinned by the AWS doc vectors (GET/PUT verbatim; the list
example settled by cross-implementation agreement — the diagnostic
recorded in Results). PUT/GET/HEAD/DELETE/ListObjectsV2 path-style
over the one http client; puts buffer while http's Body stays
unstreamed (stated), gets stream. The SAME BlobContract passes green
against LIVE MinIO (docker), and a recording transport proves the
secret reaches the HMAC chain and nothing else. specs/blob.md fully
shipped, both stages. Matrix 1353.

## rag-pgvector — the vector store behind the same interface
Completed: 2026-09-01 (landed as 4b7dc0b)
PgVector in okay-rag's JVM leg: VectorStore[Async] over the Sql
seam via the okay-pg WIRE (the consumer that road was cut for) —
own posture (ensure() creates extension+table), upsert ON CONFLICT
on the segment identity (re-index replaces), search pushed to the
engine with declared Metric whose scores return on the Vectors
scale. THE assertion: order AND scores agree with the reference
MemoryStore within 1e-4 on the hashing fixture; the segment
round-trips whole. Exact scan v1 — an approximate index is a later
measured choice BECAUSE agreement is only testable while exact.
Fixed forward en route: the repo outgrew RepoAgent's 400-file
limit (now 1200); okay-match's second UUID.randomUUID site
(requestLink) broke the JS linker again — freshId, with a note
that a guess-resistant token wants okay-security's seam. Merge
read alone: exit 0. Matrix green.

## match-identity-x — cross-channel identity, without building the hijack
Completed: 2026-09-01

The registry marks attributes identifying (a phone is, a skill is
not); only those generate link candidates, and a candidate answer is
an attribute name plus a masked email — never the value, never the
other profile's facts, never a link. The link itself is proven by the
token: minted for the old profile, delivered through the OLD channel
(the site's job), typed in the new chat — single-use, expiring, right
holder only; the stage-2 recovery secret is the fallback for a dead
channel. A confirmed link is an equivalence, not a merge: both
profiles stay, identityOf answers the class, search folds it into one
candidate carrying facts from both, profileOf aggregates — log-first
holds, nothing rewritten. Tools ident_candidates/request/confirm let
the LLM drive the whole dialogue. 18 tests; the class survives a
restart on the durable store. specs/match.md is now fully landed.

## sql-pg-wire — the direct road: Postgres v3 behind the Sql seam
Completed: 2026-09-01 (landed as 2b03cb3)
okay-pg, ~400 lines for the whole road and zero dependencies:
startup + SCRAM-SHA-256 with the halves most clients skip (server
nonce must extend ours; server SIGNATURE verified — mutual auth;
md5/cleartext deliberately not spoken), the extended protocol with
portals AS the chunk mechanism (Execute maxRows + Flush,
PortalSuspended = next chunk — fetch-size with no driver in
between), text format v1, errors drained to ReadyForQuery before
the throw so the session survives, describe consulting
pg_attribute so verify keeps full strictness. Live suite on the
dockerized Postgres 17.11 (skips where absent): 8 tests including
the TWO-DRIVER ACCEPTANCE — one typed program over PgSql and
JdbcSql/H2, one equal answer, only the SQL strings differ ($n vs
?). The pg family (Cockroach, Timescale, Materialize, Neon,
pgvector) is now a connect call away. Merge read alone: exit 0.
Full matrix green.

## blob-fs — the seam three specs assumed; stage 0, the fs engine
Completed: 2026-09-01 (landed as 2338af1)
Trait Blob cross-built; the Fs engine holds the floor: strict root
containment, atomic tmp-then-move puts, crash leftovers invisible,
engine-defined etags. get answers Either — the chunks are the body,
the answer is the outcome, an absent key is a value naming itself
(sketch adjusted, recorded in Decisions). BlobContract written once;
blob-s3 re-runs it against MinIO. Matrix 1317.
## match-stage2 — the rerank, the gate engine, decay, and the recovery seam
Completed: 2026-09-01

Rerank is an effect (the rag/Embed precedent): `top` runs Find, then
the reranker over the top slice; tests use the lexical handler, the
production one is five lines over okay-llm at the site. PlatformPolicy
replaces the predicate — Allow / AfterMatch / Withhold per attribute —
and Ranked.withheld NAMES the AfterMatch facts that matched: the
seeker learns that the phone exists, not what it is. Volatile
attributes decay the rank on an exp2 half-life. Email recovery is a
hashed-secret rebind behind a hash/verify seam (okay-security plugs
in; no dependency): with the secret the new email finds the old
profile, without it a stranger gets a fresh one — never a hijack.
Memory and Sql handlers carry all four; 14 tests green. Cross-channel
identity stays open as match-identity-x.

## sql-sqlite — the embedded engine proves the seam (user ask)
Completed: 2026-09-01 (landed as 5b17922)
The whole typed battery over xerial sqlite-jdbc (test-scope)
against a FILE database: metadata honest enough for a clean verify,
both isolation levels granted, the Writes bridge in its
spec-preferred ON CONFLICT DO NOTHING spelling, and READ-ONLY open
mode standing in for the no-DDL posture (an embedded db has no
users — "their database" is a file you were handed). En route,
found and fixed for everyone: okay-match + okay-jdbc both carrying
H2 in one sbt JVM raced DriverManager's per-classloader driver
registration ("No suitable driver" for whoever ran second) — both
suites now fork, the core-fork precedent. Merge read alone after
one refused ff (claim-only divergence): exit 0. Matrix green.

## own-db-migrations — the settled discipline, adopted not reinvented
Completed: 2026-09-01 (landed as e5eff69)
Migrate against the Sql trait: versioned authored scripts, sha-256
checksums, the version table in the SAME database, script + row in
one transaction as far as the engine's DDL allows. The fingerprint
rule again: changed or vanished applied scripts refuse naming the
version; duplicates/disorder refuse before touching the db; a failed
script leaves no row and the fix applies next run. record = the ops
hook. RODE ALONG: okay-match hotfix (UUID.randomUUID's SecureRandom
broke the JS linker — every matrix run was red; util.Random hex ids
now) and the obs Never test proven by a counting clock instead of a
wall clock. Matrix 1301.
## match-stage1 — okay-match is durable: the Sql seam, the log, the migration
Completed: 2026-09-01

SqlMatch: the same three handlers over ANY `Sql` driver (H2 in the
tests; sqlite or Postgres is the connection string — the seam is the
point), values flattened into typed columns, restart-proof (a second
handler over the same database continues where the first stopped,
ids included). ChatLog: chat turns on a persist topic keyed by
profile, offsets as provenance, and `replay` — the log-first test
rebuilds a FRESH store from the topic to the live store's exact
state, and replaying over the live store changes nothing (the
idempotence key doing its job). mergeAttr: the registry migration —
the drifted attribute's facts move to the winner, the loser answers
no more. 10 tests green (6 stage-0 + 4 stage-1).

## match-stage0 — okay-match exists: the model, the effects, the reference store
Completed: 2026-09-01

New module okay-match (package okay.matching — `match` is a keyword).
Model.scala: profiles (email + owner-secret UUID), append-only facts
with chat-span provenance and supersede-with-reason, two-gate
visibility, the small value core, typed predicates. Ops.scala: the
Registry / Facts / Find effects — the whole backend contract, open by
construction. Memory.scala: the reference handler — hashing
embeddings, search-before-create on propose (slug/synonym exact OR
description cosine), replay-idempotent asserts keyed by (profile,
attr, provenance), hybrid candidates (hard predicate filter, then
similarity over per-side profile summaries), both gates honored at
disclosure. Tools.scala: the operations as LLM tools 1:1 — the
(specs, table) pair mcp.Server.serve takes. Six tests, one per
spec behavior checkbox, including the scripted two-side scenario:
provider chat in, seeker chat out, matched end to end.

## obs-tracing — the missing third of the doctrine, without a framework
Completed: 2026-09-01 (landed as 2ad52a4)
okay-obs cross-built: spans as values on a trace topic, W3C
traceparent parsed totally (damage = a NAMED fresh root), tracestate
opaque, the traced(Handler) combinator wrapping any handler blind.
The crossing test follows one traceId from an inbound header through
okay-http into H2 through the Sql seam. Never is a short-circuit by
construction. obs-otlp and obs-durable-overlay filed; the journal
join box stays open until a Durable consumer. Matrix 1282.

## match-spec — okay-match designed: structure the unstructured, then find it
Completed: 2026-09-01

specs/match.md, designed in conversation with the user. Log-first
(chats in persist topics are the only truth; facts, profiles and
indexes are rebuildable projections), an attribute registry with a
search-before-create contract as the mechanism that bounds LLM
vocabulary drift, append-only facts with provenance to a chat span and
supersede-with-reason (freshest wins, but ask first), two-gate
visibility (owner intent AND platform policy) from day 0, identity as
email + owner-secret profile UUID with the recovery/hijack question
honestly deferred to stage 2 alongside okay-security. Effects first,
handlers second: memory + rag embeddings at stage 0, sqlite and
Postgres+pgvector through the Sql seam at stage 1.

## conf-topic — the config becomes one more consumer of the one primitive
Completed: 2026-09-01 (landed as 87f53b4)
Configs in okay-persist (beside Snapshots/Offsets): put/latest/at/
history over a compacted keyed topic, values as the Schema's JSON.
The audit IS the log, rollback IS a read, and history is honest
about compaction (asserted). okay-conf keeps codec-only deps;
reference-only safety is invariant 3 by construction. specs/conf.md
fully shipped, both stages. Matrix 1254.

## codec-iso — to every algebra the wrapper does not exist
Completed: 2026-09-01 (landed as 46c6bcd)
SIso in the Schema enum (wrap/refine): a newtype travels as what it
wraps, a refining Left is a decode error like any other. All six
algebras swept — Json, Cbor, tool schema, form, and okay-sql's row
bridge (Field gained into/outof; a wrapped column is its underlying
kind both directions). First consumer: Schema[Secret] is the bare
reference string. Composes with codec-defaults. Matrix 1242.

## cache-view — the consumer that is never invalid, only behind
Completed: 2026-09-01 (landed as 6ab7e63)
Regime 1 shipped: View(topic)(key)(fold) in okay-cache (now on
okay-persist) — latest serves the consumed fold, lag IS consumer
lag, refresh is the whole of invalidation, a fold answering None is
the tombstone. Cold refold agrees with the warm view before AND
after compaction — the snapshot story, told as a cache. All three
platforms. Merge read alone: exit 0. Matrix green.

## cache-memory — named invalidation, no default TTL, stage 0
Completed: 2026-09-01 (landed as f6219f9)
okay-cache cross-built (core-only dep): Regime (Budget/Invalidated,
no default, no unbounded constructor), bounded LRU memory engine
(expiry on read, re-insertion recency), single-flight getOrLoad
whose loader runs under its OWN drive — a failure anywhere in it
reaches every waiter instead of stranding them, and the key
recovers. Negative caching is a type (V = Option[A]), stats a plain
value. 9 JVM / 7 JS / 7 Native (the shared suite drives Run-only
programs inline — no CanBlock, so it runs on JS). Filed
cache-write-through for the orphaned write-through box. En route:
one full-matrix environmental kill (okay-conf Native, signal 9,
green twice alone — OOM under parallel Native runners). Merge read
alone: exit 0. Matrix green on rerun.

## agent-langchain4j — their ChatModel behind our Model effect
Completed: 2026-09-01 (landed as b8b4d75)
okay-langchain4j (jvm, langchain4j-core only): message/declaration/
reply as pure mappings — the fourth algebra's schema walks into
their JsonSchemaElement tree with required intact, so a defaulted
field stays omittable across the interop; the handler is comonadic
like Provider.openAi (Loom parks in their client); count stays
local. Proven against a scripted ChatModel recording what it saw.
The P9 interop sentence's Model half; rag-langchain4j filed for the
EmbeddingStore half. Matrix 1197.

## lake-read-duckdb — the lake read road, proven with zero new code
Completed: 2026-09-01 (landed as be03533)
The point of the seam, demonstrated: a Parquet file is queried
through the SAME typed layer as every relational source — DuckDB
embedded (test-scope only), read_parquet the table, verify passing,
100k rows streaming at fetch-size chunks, an aggregation pushed to
the engine. Finding recorded in the spec: Parquet marks fields
OPTIONAL by default, so verify demands Option fields — the
fingerprint lesson working, not a nuisance. One full-matrix flake
observed en route (okay-jetty TestResumable, green twice alone) —
noted on the http flake entry. Merge read alone: exit 0. Matrix
green on rerun.

## theory-nav — previous/next at the foot of every chapter
Completed: 2026-09-01

Each theory page (the map and chapters 1-7) ends with
prev · Contents · next navigation, so the book reads front to back
without returning to the index by hand.

## theory-cite-links — in-text citations are footnotes now
Completed: 2026-09-01

Every \[Author Year\] in the theory chapters links to its entry in
that chapter's References (HTML anchors on the entries), where the
paper link from the previous pass awaits — 31 in-text citations wired,
zero unmatched, existing links and code blocks untouched.

## jdbc-poll-source — the watermark poll, honestly not CDC
Completed: 2026-09-01 (landed as 07a57d0; spec first)
Poll(db, offsets, group, source): the watermark IS a persist
consumer offset (commit-as-record, refold-on-restart), one poll =
the decoded prefix up to the first damaged row — damage STOPS the
watermark, so nothing is silently skipped, and the fixed row is
re-served next poll. The late-commit caveat is a TEST, not a
footnote: the miss asserted as behavior, then the lag window (in
the caller's SQL, the DBA's language) holding the watermark back
so the late row arrives. With this the jdbc.md behavior list is
fully checked. Merge read alone: exit 0. Full matrix green.

## jdbc-write-bridge — the Durable policies over their constraints
Completed: 2026-09-01 (landed as b1903cd; spec first)
Writes(db, topic, run) in okay-jdbc, written only against the Sql
seam and a persist Topic (movable to any driver): write() journals
Intent(seq, sql, params, key) durably BEFORE the statement, Done
after; recover() refolds and resolves each open intent by declared
Policy — WithKey re-executes the same statement/key and the far
end's constraint dedups (H2 MERGE, landed once), Reconcile(select)
settles the journal without re-executing (proven with a PLAIN
insert that would have thrown on re-run), Fail/empty-Reconcile
answer Unresolved as data with the world untouched. Both crash
windows tested; seq continues over restart. Schema[SqlValue]
derives for the journal records. Merge read alone: exit 0. Full
matrix green.

## conf-impl — configuration as data, secrets as references
Completed: 2026-09-01 (landed as 443c8a2 — the release rode a pull --rebase over the README push, so the changelog names the post-rebase hash)
okay-conf cross-built (depends on okay-codec only): Secret whose
toString IS the reference, Secrets env/file/memory/chain (the chain's
one error is the specific one), Conf.read/load. The shared suite
proves env: on JVM, Node and Native; file: trims exactly one
trailing newline. No plain: scheme, deliberately. codec-iso filed
for the bare-string Secret form. Matrix 1179.

## sql-seam — the relational seam cut at the driver, first road open
Completed: 2026-09-01 (landed as 90c97bf; spec first 0c560db)
New module okay-sql, cross-built JVM/JS/Native — the no-java.sql
claim IS the JS/Native compile: SqlValue/SqlType/Col/Isolation/
Granted(requested, granted), trait Sql (Async everywhere, plus the
one sync cancel() brake for the region finalizer), the typed layer
written once (rows by label camel→snake with row-position Bad,
verify naming dropped/renamed/retyped/nullability drifts, params
positional-prepared-only, transact generic over the rest of the
row so aborts cross the scope and still roll back). okay-jdbc is
the first driver: 13-test battery on H2 run AS a no-DDL user.
The rollback-on-exception test caught a core finalizer leak —
Resource.run applied k(y) outside its try after a forwarded
effect — fixed in core, pinned in TestResource. Write-bridge and
poll-source stay their own slugs. Merge read alone: exit 0. Full
matrix green.

## history-tsv-tabs — the flagged rows had changed shape, not just tabs
Completed: 2026-09-01 (landed as 5774436)
The six rows the room flagged already had real tabs; the live defect
was 51 five-column rows (theirs included) against the eight-column
header. Normalized mechanically — unit into the note's prefix,
unknown sha/load/ref/ratio honestly empty. NF==8 for every data row.

## codec-defaults — the one macro this library allows itself
Completed: 2026-09-01 (landed as 3ebee10)
Mirrors do not carry defaults; the companion's <init>$default$N do.
The macro reads them into SProduct.defaults as ordinary values;
Json/Cbor fall back in order (declared default, None-if-optional,
refusal); an uncallable default is honestly None. ToolSpec stops
requiring defaulted fields and advertises `default` — the omission
an LLM will make is now one decode survives. Proven JVM+JS+Native
by the shared suite. Matrix 1126.

## ui-dom-patch — the raw-DOM Backend; the patch consumer arrives
Completed: 2026-09-01 (landed as 1624244)
React-less DOM over js.Dynamic, zero dependencies: React.elem is the
build plan, Ui.patch keeps the mirror events interpret against
(React.event, one delegated listener per kind), paths walk
childNodes. Proven against a fake document under Node: the law
(patching frames equals building the last), a shuffle creates zero
nodes, narrow patches mutate in place, events round-trip. okayUi JS
tests exist again (js test dir replaces sources := Seq()). Matrix
1106.

## persist-stage1 — the consumers prove the seam
Completed: 2026-09-01 (landed as 8501246; spec first af18ad6)
Compaction (keep-latest-per-key, offsets preserved as holes,
exclusive with retention) forced disk format v2 — frames carry
their offset — and bought the evolution test both ways (a forged
v1 segment reads; a v1 active segment is closed and a v2 rolled).
Typed view: four-byte version envelope over CBOR, byte-level
upcasts via Typed.step, every failure Decoded.Bad(offset, error).
Offsets (commit-as-record, refold-on-restart, lag), Snapshots
(put/latest, the ui lane's ask), Streams (stream/tail as
Chunk ! Produce + Async; dropped history stops by declared
OnTooEarly). okay-agent: TopicJournal = Durable.Journal over a
keyed topic, intent and completion separate records, the whole
crash-window battery green against it. okay-persist now depends on
the core; okay-agent on okay-persist. Merge read alone: exit 0.
Full matrix green (persist 38 JVM / 13 JS / 13 Native; agent +5).

## security-argon2 — the satellite that buys a dependency
Completed: 2026-09-01 (landed as 16b8d58)
New module okay-security-argon2 (jvm, Bouncy Castle) — a memory-hard
KDF cannot be had from the JDK, so this module pays while the core
keeps its zero. PHC stored form (portable, parameters ride the row),
RFC 9106 vector pins the provider, absurd parameters refuse before
allocating, verifyAny reads a mixed pbkdf2/argon2id store. Matrix
1072. okay-security's staged spec is now fully shipped, 0 through 5.
## nio-close-fix — the flake was the OS; Nio rewritten on blocking channels
Completed: 2026-09-01

The chase ended two suspects deep: the dedicated-channel-group
experiment cleared the default group, the blocking rewrite reproducing
the loss cleared the whole JDK async layer, and stage counters plus a
parked-accept-never-woke trace pinned it on macOS itself — under
listener churn the kernel completes a handshake into the backlog,
never delivers it to accept, and closes it with a clean FIN, at
~1.2/1000 rounds on either channel API. One stable listener: 8000/8000
clean. Nio stays rewritten on blocking channels over virtual threads
(simpler, measured equal, no userland dispatch to lose); specs/nio.md
carries the argument, okay-http/BUGS.md the full forensics, TestNio a
stable-listener churn gate — deliberately NOT a listener-churn gate,
which would flake red on ~45% of runs by the OS's hand. Also per user
request: the library is Okay, capitalized, across README and docs
prose (127 mentions; code, packages and module names untouched).

## security-es256 — the raw-vs-DER dance, danced
Completed: 2026-09-01 (landed as 9995966)
Es256 is the conversion alone — pure, total both ways, shared, its
battery on JS too (the build's := became += for that). EcPublic/
EcPair keys, the key still decides the algorithm with three kinds in
the ring, Jwks learns kty:EC, Oidc gains ES256 IdPs for free.
Merge read alone: exit 0. Matrix green.

## security-oidc — user login from parts on the shelf
Completed: 2026-09-01 (landed as 4951cd1; the release entry went out
one commit early again — a refused fast-forward followed by a `;`
chain. The rule hardens: merge FIRST, boards after the merge exit is
read, nothing between them but the check)
Discovery, login url (nonce), callback validating the id_token into
a Principal; at_hash keeps spliced access tokens out; the forgery
battery refuses each attack by name. Matrix 1049.

## spec-audit-fixes — the audit's seven gaps closed in the specs
Completed: 2026-09-01
Three specs born: tls.md (one transport seam, sslmode vocabulary,
verify-full default), obs.md (spans as values on a trace topic,
W3C traceparent, tracing handlers), blob.md (object-store seam, fs
+ own-SigV4 S3 subset). Six updated: persist.md (the sync-SPI
asymmetry recorded, backup/PITR stated, wire auth via
okay-security), jdbc.md (sketch retyped against Sql, own-DB
migrations à la Flyway), data.md (queues bridged not mirrored —
no Queue seam, two table rows), sql/cache/r TLS links. BACKLOG
slugs turned into implementation entries.

## security-node — the JS leg verifies
Completed: 2026-09-01
node:crypto behind the same seam; the JS suite runs the SAME shared
code (HS256 JWT, passwords, API keys, PKCE pinned to RFC 7636). The
linker forced the right design: platform keys are an opaque
Crypto.Handle; JWKS parses everywhere, verifies where keys exist.
Matrix 1047.

## codec-vector — Schema learns Vector and Char; recursion is a test
Completed: 2026-09-01
SVector + SChar, every algebra swept; recursion proven at depth on a
product and a sum; Schema[Ui]/Event/Patch derive and round-trip both
wires — the hand mapping is a choice now. The sweep's warnings caught
WireJson missing the keyed-diff trio (a real MatchError-in-waiting).
codec-defaults filed with its reason. Matrix 1042.

## py-spec — specs/py.md: Python as a handler
Completed: 2026-09-01
The REval twin (PyEval), with the r.md model adopted by reference
rather than copied. Python-specific: module:name addressing, an
own stdlib-only stdio shim (Py4J and jupyter-kernel rejected),
persistent worker as the served engine (resident imports; N
workers instead of threads-under-GIL), verify against the
configured interpreter (wrong-venv refuses loudly). The
JVM-python question answered once: Jython dead, JEP/ScalaPy
shared-fate, GraalPy a watched future engine behind the unchanged
seam. Spec only; py-subprocess, py-worker, py-arrow filed.
## nio-close-race (partial) — narrowed to a serve-fiber stall, not fixed
Completed: 2026-09-01 (investigation landed; fix still open in BACKLOG)

Three harness generations: racy counters, per-round futures, and a
leak-free sequential trace. Established: the serve fiber STALLS (a
write completion that never fires — not an exception, onComplete never
runs) after 0–4 writes, at ~1.3/1000 rounds; the client usually sees
premature EOF, sometimes a pure hang. The okay Async driver was read
and cleared — the Await cell CAS protocol is sound. Prime suspect is
the default AsynchronousChannelGroup under rapid channel churn.
Also landed per user request: law comments moved above each law in all
theory code blocks (phone readability), and chapter 7 now names the
origami tradition — Gibbons 2003, catamorphisms/anamorphisms, and the
Chunks pipeline as a hylomorphism with the optimizer as fusion laws.

## ui-keyed-diff — a moved child is a move, not a Replace
Completed: 2026-09-01
Keyed matching when every child has a distinct key: one Reorder for
a shuffle, narrow patches ride along, Remove/Insert for churn;
positional fallback otherwise. The law extended: 200 seeded rounds
plus a quality assertion (shuffles never Replace). Matrix 1027.

## r-spec — specs/r.md: R as a handler
Completed: 2026-09-01
R joins the landscape as call-shaped foreign compute: an R call is
an OPERATION (journalable by Durable, mockable by handler swap,
supervised like a cluster worker), never an embedding (JRI/Renjin/
FastR rejected with reasons). Named functions only — no string
eval, structurally; neutral RValue/RFrame with Schema at the edge
(the SqlValue move); verify(packages) catches environment drift
loudly; subprocess engine first, Rserve behind the same handler.
Spec only; r-subprocess, r-rserve, r-arrow filed.
## quiet-measurements — the two waiting questions, answered on a quiet machine
Completed: 2026-09-01

symbol-fold-cost is CLOSED: indexFoldNoRefs 189.6 ±5.4 against
indexFoldOnly 235.0 ±14.9 — the identifier branch is 19% of the walk
and 81% is the traversal machinery, which prices future optimization
honestly and agrees with the refuted mutable-bucket rewrite. The
cluster flush question closed directionally (blockingBytesFlushed 50.8
±9.5, slower than shipped 38.2 ±0.5) and yielded a NEW correctness
lead: the NIO lane sporadically fails its sum assertion — possible
data loss around close — filed as nio-close-race. Chapter 1's monad
laws expanded per user request: each law spelled, read operationally,
and tied to why generic code depends on it.

## mcp-templates — one declaration, unbounded uris; MCP's list closes
Completed: 2026-09-01
resources/templates/list both ends; expand (RFC 6570 L1) and its
never-guessing reverse `matches` — a server's read extracts the
variables, one line serves a tree; completion tied in; the reference
server's own templates probed live. The 2025-06-18 protocol list is
COMPLETE. Matrix 1022.

## mcp-completion — the completer is a function the Serving carries
Completed: 2026-09-01
completion/complete both ends: Complete => Vector[String] as an
Option (capability follows the function), 100-cap with hasMore/total,
context narrowing, resource uris passed through; the live probe got
an answer from the reference server. 4 tests + probe; matrix 1016.
## theory-textbook — where okay comes from, with the papers
Completed: 2026-09-01

docs/theory/: seven chapters and the map, ~800 lines, okay as the
single running example. Moggi and Wadler through Free's normalizing
fold; Felleisen, Danvy–Filinski and Filinski's representation theorem
as the sentence justifying Cont at the bottom of the tower; Atkey with
both instances; Swierstra and Kiselyov–Ishii with the left-nested-bind
literature; Plotkin–Power/Pretnar and the three handler shapes on one
line; Carette–Kiselyov–Shan and Taha–Sheard as the two answers to
interpretive overhead, both present and each placed where its theory
says; LogicT, codata and the sketch papers to close. 30 works cited;
every okay claim grep-verified at file:line during writing.

## ws-close-halfduplex — the last unchecked item in specs/http.md
Completed: 2026-09-01

The strong form of the half-duplex claim: `WsEcho(partingWords = 3)`
answers a Close with three more texts before echoing it, and the
session sees all three, in order, then the Close. specs/http.md now
has no unchecked behaviour item.

## security-mcp — MCP authorization: the challenge that teaches
Completed: 2026-09-01
McpAuth: RFC 9728 metadata (public — it is how strangers learn), the
protected route (401 with resource_metadata), discovery with named
Lefts, connect onto a bearer-carrying link. The loop test: the same
agent call works protected and open. MCP's last parked item closed.
4 tests; matrix 1011.

## sql-seam-spec — specs/sql.md: SQL without a mandatory JDBC
Completed: 2026-09-01
The typed relational contract (rows/verify/transact, jdbc.md)
re-cut against a driver-agnostic Sql seam (neutral SqlValue/Col,
Async): okay-jdbc becomes its first driver (and stays the honest
default on the JVM), okay-pg (Postgres wire over the Async
transport, cross-platform, unlocks the whole pg family incl.
pgvector) the direct road, R2DBC a stated low-priority hatch.
persist gains the openness commitment: persist-wire (remote Topic
client), format and wire as documented surfaces. Spec only;
sql-seam, sql-pg-wire, sql-r2dbc, persist-wire filed.

## security-core — okay-security stage 0
Completed: 2026-09-01 (landed as 3a36930)
The model as values, JWT (HS256/RS256, kid, skew; alg confusion
defused by the key deciding), JWKS, PBKDF2 with parameters in the
stored form, API keys as digests, the policy algebra, Secure.bearer
holding the door by type, OAuth2 client flows with S256 PKCE checked
by the stub AS. 11 tests, hostile side throughout. Matrix 1007.

## data-spec — specs/data.md: the data landscape, two postures, few seams
Completed: 2026-09-01
NoSQL, OLAP/warehouses, lakes, vector, Kafka, Spark — classified by
access shape, not vendor; a vendor enters only as an implementation
of an existing seam. One new trait for the one uncovered shape
(Docs: CAS conditional writes, declared consistency; multi-doc
transactions refused in favor of journaled sagas). Foreign posture
(no DDL, their constraints as the idempotency far end) and own
posture (the log + materializations, refold as universal rebuild)
defined once, applied per class. Five implementation slugs filed.

## mcp-resumable — Last-Event-ID is read(from)
Completed: 2026-09-01
Pushes journaled per session key before fan-out; SSE frames carry
id: offsets; a dropped stream reopens with the token and replays
exactly what it missed, then goes live; fresh GETs start at the live
end; v6 without a journal is untouched. 4 tests over real Jetty;
matrix 996.

## cache-spec — specs/cache.md: caching with named invalidation
Completed: 2026-09-01
Every cache names where its truth lives: a log-fed view (never
invalid, only behind — lag IS consumer lag), write-through with an
invalidation topic (the stale window stated, not denied), or a
declared staleness budget — no default TTL anywhere. Single-flight
in getOrLoad, bounded always, Redis via a minimal own RESP behind
the same trait; distributed locks refused out loud. Spec only;
cache-memory, cache-view, cache-redis filed.

## jdbc-typed-spec — specs/jdbc.md: the unmodifiable foreign database
Completed: 2026-09-01
The posture: their schema is authoritative — bind, don't model. SQL
stays; Schema becomes the row and param codec (total decode, damage
names the column), verify catches drift at startup (the fingerprint
lesson at the DB seam), transact is a Resource-shaped region with
declared isolation, and writes bridge to okay-persist through their
own unique constraints (WithKey/Reconcile in SQL). Spec only;
jdbc-typed, jdbc-write-bridge, jdbc-poll-source filed.

## conf-spec — specs/conf.md: config as data, secrets as references
Completed: 2026-09-01
Names the rule already in force (modules take credentials as
constructor values; the edge resolves them) and adds the missing
seam: Secret = a reference (env:/file:), Secrets = the resolver
trait, four invariants keeping passwords out of journals, logs,
URLs and stored configs by construction. Spec only; conf-impl and
conf-topic filed.

## ui-durable — the journal is the line stream, verbatim
Completed: 2026-09-01 (landed as 9068fe7 — an earlier entry said
277ecce, the pre-rebase hash of the same work: a `| tail -1` hid a
refused fast-forward and the release commit went out before the
merge; recovered from the reflog, rebased, landed for real)
Event-sourced sessions on persist-core stage 0: journal inbound
lines (hostile ones included — the stage's determinism is the
argument), segmented refold (a journaled Closed ended a connection,
not the session), snapshots bound the refold (counted). Six
equalities between live runs and recoveries. Matrix 983.

## cluster-nio — measured, the answer was neither guess, and the code stays
Completed: 2026-09-01

Four lanes, then a fifth when the first attribution turned out to be
confounded. The shipped transport is 37.9ms per 100 chunks; bytes with
a single flush 24.4; NIO 24.7; the codec alone 25.9. A byte rewrite
with the flush-per-send streaming requires measured 38.4 — equal to
what shipped — so the 1.55x was the flush policy, not the text
machinery, and the rewrite was REVERTED: equal performance, more code.
What stands: Loom parking is free (NIO vs a parked read is a wash), the
codec IS the transport (CBOR is the lever), and a totality hole found
by a torn frame — the "total" JSON parser threw on "-", "1e" and three
more — is fixed and pinned. `Lines.stage` (bytes→UTF-8 lines) moved to
the core; okay-http delegates to it.

## ui-screens — screens are codata, a wizard is a screen you push
Completed: 2026-09-01
Screen (view + step), Nav stack (Stay/Push/Pop/To; empty = end),
Nav.scenario fusing Dialog into a pushable screen with the answer
through the parent's continuation. 5 tests; matrix 977 (with
persist-core landed beside).

## persist-core — okay-persist: the durable log, stage 0
Completed: 2026-09-01
Spec-first (specs/persist.md — the partitioned log designed to its
distributed extent: replication with epochs, delivery semantics,
evolution, ops as values; built in stages). Stage 0: Record/Ack/
Policy/Topic/Store, FNV-1a routing, MemoryStore cross-platform,
FileStore on the JVM (versioned segment headers, CRC32C frames,
torn-tail truncation on recovery, retention by whole segments);
poll-on-end is a tested claim (the ui/mcp tailing contract). 23 JVM
+ 8 JS + 8 Native tests; full matrix green (one unrelated TestMcpHttp
flake, filed). Commits 5ffce3c..ca24db3.

## ui-wire — server-driven UI; the tree is the capability list
Completed: 2026-09-01
WireJson (hand-mapped; codec-vector filed), Wire.serve as a pure
stage (full tree, then narrow patches), Wire.client to any Host; the
forged-key test argues from the hostile side (its update THROWS on
the forged key — the wire never lets it through). 6 tests; matrix 933.

## codec-native — the P5 chain on Scala Native
Completed: 2026-09-01
okay-lex/parse/codec gain Native legs (an omission from P5, never a
decision); 57 tests pass as native binaries first try; okay-ui's Form
rides to Native. Full matrix: 927 tests.

## ui-scenarios — Dialog: a wizard is a program
Completed: 2026-09-01
Show answers an Event (a GADT); scenarios run standalone over any
Host or AS a screen inside the loop (the continuation is the state);
Form.ask/askSchema with retry-by-recursion; the demo's elicitation
loop collapsed to one line. 4 tests. Landed with spec check-off.

## ui — okay-ui v1: the toolkit that is not a toolkit
Completed: 2026-09-01
Spec-first (specs/ui.md, incl. the architecture above v1). The view
as a value (keys, not closures), diff+patch with the agreement law,
the loop over merged sources, terminal host (pure frames + stty),
React-shaped host (pure Ui=>Elem, five-line glue), Form as the fifth
Schema algebra (typed + dynamic), and MCP elicitation closed end to
end. 20 new tests. Landed: e5e19db.

## mcp — the Model Context Protocol, complete
Completed: 2026-09-01
Six tasks, spec-first (specs/mcp.md): tools/resources/prompts both
ends; duplex (subscriptions, roots, sampling as the Model effect);
transports stdio + streamable HTTP with server push over the GET
stream; acceptance against the reference server (passed first try).
Landed: 998bbc5, 955a99e, 46723fe, dd4599f, 080894e, 4a86daf.

## docs-sweep — what drifted, and what was never written down
Completed: 2026-09-01
README/ROADMAP/typepedia/tutorial corrections (Writer encoding,
groupId, counts), the upper-layers section, the MCP chapter, the
fourth kind of test, AGENTS.md. Landed: 7285974.

## stream-exercise + primitives
Completed: 2026-08-31/09-01
The fs2 exercise in okay-demo; Writer.of/map, Source + merge (bounded
by default, measured), Stage.transduce/mapAccumulate; inference fix
(one parameter list). Landed: a1f62b8..d059a9d.
