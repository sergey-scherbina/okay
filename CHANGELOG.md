# Changelog

## demo-warnings-zero — master had 23 warnings and the policy says none
Completed: 2026-09-05
Landed as 97c7d133. They arrived with `match-moves-out`: the
marketplace left okay-demo and its imports stayed. Reproduced on an
untouched master checkout before touching anything, so not a stale
cache and not one worktree's problem. This repository has a lane whose
subtitle is "zero warnings enforced as policy", and it had been broken
since that merge.

Sixteen unused imports across `ChatDemo` and `TwoNode`; three private
helpers in `TestChatDemo` that nothing calls; one unused private val
(`turnNo`); three `using` parameters no body reads (`Transport` in
`ChatDemo.routes`, `Transport` and `Secrets` in `TwoNode`); and one
E176 in `Board.scala` where an `Option[Task]` was discarded inside a
`foreach` block — now said with a type, `apply(op, field): Unit`,
rather than dropped silently.

Nothing else: every edit is a deletion of something unused or an
explicit discard. The three signatures that lost a `using` parameter
lost one the compiler proved no body reads, which cannot break a
caller.

Not my module. Asked the owner in the room and offered first; the
operator then said to fix it.

Gate: clean compile 0 warnings; okayDemo 61 tests, 0 failures.

-first-segment-race — several processes may open one log at the same moment
Completed: 2026-09-05
Landed as fdef80b7. A shared log with several processes reading it is
the arrangement this module's own two-node story describes, and it had
a race at the very first moment: `FileAlreadyExistsException` on
`00000000000000000000.log`, seven openers of eight.

It survived because every test and every hand-run opens ONE store at a
time. Staggered opens never hit it — a few seconds apart and all
succeed. It took a demo test starting two real processes together to
show it, and then two heavier runs to find the rest.

THE RACE HAS THREE SIDES, and each was found by a heavier run than the
last. Worth recording as a shape: a fix that passes on an idle machine
has not been tested against the thing it fixes.

(1) THE CREATE. Each opener lists the directory, finds no segments,
and calls `newSegment(0)` with CREATE_NEW. One wins; the rest die.
Losing is NORMAL — the winner's segment is precisely what this opener
would have created — so the loser looks again.

(2) THE HEADER. Looking again immediately is not enough. The winner
has created the file and may not have written its header yet, a window
of microseconds that a loaded machine makes real. The loser reads a
zero-length file and reports "no header — not a segment", which is
true and is not the answer: the segment is being born. So it waits for
the header, bounded at two seconds; anything longer is a different
problem and is reported rather than spun on.

(3) THE OPENER WHO NEVER RACED. One that finds the file ALREADY in its
listing never attempted a create, so it never waited — and walks into
the same half-born segment. This one appeared only under a full
matrix, one loser in twenty-four. The wait belongs on the path that
READS, not only on the one that lost a create.

The test uses threads rather than processes: the window is between the
listing and the create, and that is the same window whether the loser
is another thread or another JVM. Twenty-four openers on one barrier,
because an idle machine hides sides two and three.

TestTwoNode no longer pre-creates its log, which it had started doing
to get past this. Two processes on one empty directory is what found
the defect and is what keeps it found.

Gate: 2328 tests, 0 failures, 0 warnings.

# Changelog

## channel-chunks-native — measured and declined; the last gap is the interpreter

The one row where zio genuinely led the idiomatic table — reading a
buffered channel one element at a time, **264.5us against
`ZStream.fromQueue`'s 139.1** — profiles as **62% effect machinery**
(`runFree` 26%, the `Async` handler 15%, allocating `Free` nodes 13%,
`resume` 8%) against **8% in the channel**. The queue, whose structure
was replaced twice over today, is not what is left.

So the obvious move was to batch the PROGRAM the way `popMany` batched
the queue: a `drainedChunks: Source[Chunk[A]]` stepping the tree once
per batch rather than once per element. It measured **447.9 against
the elementwise 264.5 — worse**, and the measurement said why: on this
path the producer delivers an average of **1.67 elements** per
`receiveMany`, max 64. There is nothing to chunk. `Channel.buffer`
feeds the channel through the effect system at one `Free` step per
element, so the consumer never falls behind and the buffer never
fills.

Which is the same finding as `channel-send-fastpath`, one layer up:
**batch size is set by how far the producer can run ahead**, and here
it cannot, because the producer's own per-element cost IS the
interpreter. Withdrawn rather than shipped with a story attached.

Filed as `channel-per-element-effect-cost` with the two routes that
might work: make a per-element `Writer` step cheaper, or give
`Channel.buffer` a chunk-native feed so the producer emits arrays —
the second is likely smaller, since `Channel[Chunk[A]]` already exists
and `mergeChunked` already uses it.

## intent-english-corpus-twins — the corpus every number is measured on had the defect too
Completed: 2026-09-05
Landed as 38009d08. The twin guard from `intent-slavic-collision`,
pointed at the corpus nobody had pointed it at: `labelled`, which the
shipped model is fitted on and against which every number this module
publishes is measured. Three pairs — two Proposals from one
"Suggest..." shape, two Requests that both say "send me the agenda",
two Notifications about a room change. Each second member is rewritten
to say the same thing a different way, and the guard ASSERTS on
`labelled` now rather than reporting.

| | before | after |
|---|---|---|
| composite, held-out English | 76.7% | **75.0%** |
| macro F1 | 0.756 | 0.740 |
| `Notification` F1 | 0.77 | 0.73 |
| `Other` F1 | 0.58 | 0.56 |
| NEAR half of held-out | 86.7% | 83.3% |
| FAR half | 66.7% | 66.7% |
| one typo | 66.7% | 63.3% |
| politeness removed | 65.0% | 63.3% |

The near half fell and the far half did not move at all — the shape a
duplicate-removal should have: the messages scoring well because
something like them was in training are the ones that lose. One
message is 1.7 points, so this is evidence for the SIGN and not the
size; the old number was slightly inflated by template redundancy,
which is the direction the theory predicts.

Every place it was quoted is corrected — the `Models` doc comment, the
module page, the `Router` doc, and the two tests that assert it.
"Expect 63-67% from a message somebody else wrote" replaces "65-70%".

AND A NUMBER THAT WENT UP AND IS NOT EVIDENCE OF ANYTHING: under the
"hedge in front" shift the composite reads 78.3%, ABOVE the unshifted
75.0%. One message, in the direction that makes no sense, and the
clearest available statement of what sixty rows can resolve — ±1.7
points is this instrument's floor, and every smaller difference in
that spec should be read as zero.

SUPERSEDED BUT NOT REWRITTEN: `intent-fitted-model-ships`,
`intent-one-entry-point`, `intent-per-class-not-aggregate` and
`intent-second-author` quote the old numbers and are correct for the
corpus they were measured on. Named here rather than edited in place.

Gate: okayIntentJVM 153, okayIntentJS 9, okayDemo 61 — 0 failures, and
okay-intent compiles clean. NOT clean overall, and not mine: okay-demo
carries 23 warnings that arrived on master with `match-moves-out`
(unused imports left behind when the marketplace moved out),
reproduced on an untouched master checkout and reported to the room.

es-out — the marketplace leaves, and the demo keeps its point
Completed: 2026-09-05
Landed as 8f2c56d7. The matching marketplace is a commercial product
and not a piece of infrastructure, so it moved to a private repository
— with its twenty-three commits of history, because the reasons for a
decision are worth more than the decision.

okay-match (15 files) and okay-demo-embed are gone. Nothing in the
library imports `okay.matching` any more, and every module it was
built ON — okay-rag, okay-agent, okay-sql, okay-persist — stays public
with every other caller it had.

THE DEMO STAYED, and that was the harder half. It was not "a chat demo
that used okay-match": it was the demo OF okay-match plus streaming,
and 800 of its 1365 lines were the marketplace. Stripping it would
have left a page that streams a reply and nothing else.

So the domain was REPLACED rather than removed. `okay.demo.Board` is a
shared task list — log-first, an owner and an assignee, and nothing
that searches for a counterpart. Deliberately NOT a two-sided market:
matching is somebody else's product now, and a demo that
re-implemented it would be demonstrating exactly the thing this
library no longer carries.

Every mechanism the marketplace was there to show survives, which is
the argument for replacing over deleting: an agent that can touch the
board ONLY through tools, so there is no path from a sentence to the
projection and a model cannot invent a task; a projection rebuilt from
a durable log; an assignment ringing the assignee's inbox and an open
page hearing it over SSE; the same tools behind MCP; two nodes over
one shared log with the follower deriving its own board; and the board
as a context parameter, so `main` wires the durable one and a test
wires a memory one.

What went with the marketplace, each being a claim about IT rather
than about the library: deals and their timeline, scenarios as data,
the two-gate disclosure model, the subscription gate at the intake,
and the attribute registry with its embedder.

THREE THINGS THE MOVE FOUND, none of them planned.

A FABRICATED VALUE IN THE NEW CODE, caught by its own test before it
shipped. `Board.apply` returned an empty task carrying the asked-for
id when no such task existed, so a caller could not tell "assigned"
from "there is nothing to assign" — and a replay would have invented
rows for records naming tasks the log never created.

A REAL okay-persist DEFECT. Two processes racing to CREATE the first
segment of a shared log both try to make
`00000000000000000000.log`, and one dies with
`FileAlreadyExistsException`. Staggered starts are fine; simultaneous
ones are not. The two-node test now creates the log before the nodes,
which is how a shared log exists in production, and the race is
reported rather than papered over.

AND A DESIGN POINT REMOVED BY ACCIDENT. `routes` taking its store as a
context parameter is what gives each test its own isolated world.
Replacing it with a global made two tests share one file and inherit
each other's state; the board is a context parameter again, which is
the demo's own ctx-wiring claim and not a detail.

specs/demo-chat.md keeps its record and says at the top what moved and
what replaced it. Those items were built, measured and shipped, and
rewriting the history to pretend otherwise would make every checkbox
in the file suspect.

Gate: 2326 tests, 0 failures, 0 warnings. The code was verified
building and passing in the private repository BEFORE anything was
deleted here — a module in two places for an hour costs less than a
module in neither for a minute.

# Changelog

## collection-foreach-pair — the fifth mismatched row, and what is left after it

`ZStream.fromIterable(list).runForeach` (96.9us) sat beside
`Source.of(list).runForeach` (169.8) and `toLazyList.foreach` (165.5)
— arrays against a program tree walked one element at a time. Asked
the same way, through `Chunks.fromIterator`, our side reads **12.8
±0.1: 7.6x ahead**.

That is five of the six rows where zio appeared to lead the idiomatic
table, all the same mismatch, all flipping when the pairing is fixed:
`runSum` 3.1x behind → 4.5x ahead, `runForeach` 1.7x behind → 7.6x
ahead, the matched-size chunk row, the many-producers table, and the
guarantee table before them.

**One genuine gap is left**: reading a buffered channel one element at
a time, 267.2 against 138.5, and today's channel work already
established that its remaining cost sits ABOVE the channel — in
`Drain`'s batching and the `LazyList` bridge — rather than in the
queue, whose own structure was replaced twice over.

The pattern is worth more than any single row. A library whose fast
path is chunk-native and a library whose surface is per-element will
compare as 3x apart in either direction depending on which lane is
chosen, and the honest table has to ask both the same question. Every
lane in this benchmark now carries its granularity in its name for
that reason.

## producers-chunked-consumer — the fifth mismatch, and this one was mine

`ManyProducersBenchmark` read every okay lane with `receiveBlocking`
per element while its zio lane pulled arrays through
`ZStream.fromQueue`. Written the same day I corrected that mismatch in
three other tables, which makes it the first of the five I introduced
rather than found.

With a chunked consumer on both sides (`Total=8000`, us/op):

| producers | one ring | relaxed, bounded | one growable | relaxed, growable | zio.Queue |
|---|---|---|---|---|---|
| 1 | 205.9 | 306.6 | **157.7** | 249.7 | 427.7 |
| 4 | 1242.0 | 366.9 | 677.0 | **196.0** | 1290.2 |
| 16 | 2073.6 | 362.2 | 680.2 | **129.7** | 2540.8 |

The row where zio led at a single producer was the mismatch, not a
result: asked the same question, the fastest okay lane leads at every
width — 2.7x, 6.6x, 19.6x. The elementwise lanes stay as diagnostics,
since `zio.Queue` has no per-element read to place beside them.

And the honest caveat, which the table earns rather than spoils: NO
SINGLE CONFIGURATION WINS EVERYWHERE. At one producer relaxation costs
(249.7 against a plain growable buffer's 157.7) because there is no
contention to relax and a part must still be chosen. That is what the
menu is for.
## intent-taxon-wired-to-tiers — a tier knows what it was fitted against, and the ensemble cannot silently disagree
Completed: 2026-09-05
Landed as 90acdb51. The last of the three frictions the first caller
exposed, parked twice for other work. Request 1 asked for one taxonomy
both tiers read; what had landed was one taxonomy NEITHER TRAINED TIER
read — every fit inferred its classes from whatever labels its rows
happened to carry, and a caller checked agreement by hand or not at
all.

Every `Trained` now carries the `Taxon` it was fitted against. `train`
still infers, because no caller should have to declare a taxonomy to
fit two classes; `against(taxon, rows)` DECLARES one and refuses a
label outside it at FIT time rather than as an invented class in a
confusion matrix later. `silent` names the classes a declared taxonomy
holds that the rows never taught.

THE LATENT BUG, which is why this was not tidiness. `NoModel.blend`
adds a cue's weight to a probe class BY STRING EQUALITY. Cues speaking
`Proposal` against a probe fitted on `MeetingProposal` never match:
every bonus is zero, the ensemble silently degrades to the plain
probe, and nothing says so — no error, no warning, and an accuracy
that looks entirely plausible because it IS the probe's. `NoModel.fit`
refuses it now, and the check is one way on purpose: every class the
PROBE knows must be in the cues' taxonomy, because those are the ones
a bonus could ever apply to.

AND A DEFAULT THAT WAS LYING. Enforcing the check broke ten existing
tests, all fitting a probe on abstract labels while inheriting `fit`'s
default cue set — and the default was the real defect:
`Patterns.meeting` attached to every fit whatever the corpus was
about, contributing nothing because the default weight grid is a
single zero. A default that is inert is a default that lies about what
it does. `cues` is `Option[Patterns.Cues] = None` now; a caller who
wants the blend names them, and only then must the taxonomies agree.
Behaviourally a no-op for every existing caller.

The taxonomy is deliberately NOT persisted: `Fitted` infers it from
the classes a model actually learned, so the wire format is unchanged
and no file written by an earlier build stops decoding. The cost is
stated where it can be read — a declared class no row ever taught does
not survive a round trip. Fit-time knowledge, not model knowledge.

Gate: clean compile 0 warnings; okayIntentJVM 153, okayIntentJS 9,
okayDemo 79 — 0 failures.

## channel-per-part-waiters — wake where the room appeared

`Queues.strong.relaxed(parts, each)` read **111546us** at sixteen
producers, against 2586 for a single ring — 35x worse than not
relaxing at all. It now reads **485**, a 230x fix, and is 5.3x past
the single ring and 5.5x past `zio.Queue`, scaling the right way at
last: 780 at one producer, 597 at four, 485 at sixteen.

The cause was a mismatch, not a setting. The channel kept ONE queue of
waiting senders while the resource is per part, so a consumer freeing
a slot in part 7 woke an arbitrary sender, who found its own part
still full and parked again — one useful wakeup in k, the rest churn.
Senders now wait per part, and `Buffer.lastRoute` reports which part a
take came from so the wakeup can be aimed. It is a hint by design: a
wrong answer costs one wasted wakeup, never correctness, and for
`MultiFifo` it is simply the consumer's cursor.

A single-order buffer keeps exactly one queue, so nothing about the
ordinary path changes.

**This is the fourth defect of one family, and they read as one
sentence: the channel asking about the buffer as a whole where the
question belongs to one part.** `isEmpty` instead of `hasReady` before
a receiver waits; `size < capacity` instead of `hasRoom` before a
sender waits; a producer's part taken on the waker's thread instead of
carried with the send; and the wakeup aimed at no part in particular.
Written into `docs/queues.md` as the lesson, because the next
partitioned structure will have the same four.

Also fixed here: a deprecation warning in `okay-intent`'s
`TestSecondAuthor` (`Char + String`), which is not this lane's code
but was failing the zero-warnings policy in the shared gate.

Gate 536, full matrix 2395, clean build, no warnings.
## intent-slavic-collision — every correction applied, and the experiment they implied did not confirm
Completed: 2026-09-05
Landed as 877a4e89. A native reader read all sixty Ukrainian and
Polish rows. Every correction goes in as given: two calques their
earlier Russian fixes had never reached, three Polish rows that
addressed only a man, one grammatical error, two register slips.

THE FIRST FINDING WAS ABOUT THE FIXTURE'S SHAPE, not any row. A
parallel arm is a translation of one English sentence, so a bad row is
eight bad rows, fixing one fixes an eighth, and nothing says so. Their
rule — review a ROW, not a language — is a test now:
`TestFixtureHygiene` flags near-twin rows within a class and language.
It caught six pairs at once in en, de, es, ja and uk, INCLUDING ONE I
HAD JUST CREATED — my fix for their collision finding rewrote a
Ukrainian Request into a twin of another Ukrainian Request, and the
guard caught it in the same run that introduced it.

THE COLLISION GENERALISES PAST THE LANGUAGES THEY READ. Whole-sentence
similarity cannot see it — the pairs they named score 0.047 and 0.121
by trigram Jaccard — so the guard measures edit distance between
OPENING WORDS across classes: en `would`/`could` at one edit, de
`können`/`könnten` at one, pl `może`/`moje`, uk `може`/`моє`. Those
are how English and German mark the distinction, so the list is a
printed DIAGNOSTIC of where the tier is blind, not a rule a corpus
must obey. English messages opening on a modal score 54.5% against
63.3% for everything else — eleven rows, directional.

AND THE EXPERIMENT FAILED TO CONFIRM, mine as much as theirs: fix the
collision and the Slavic numbers should move without adding a row.

| | en | fr | de | es | ru | ja | uk | pl |
|---|---|---|---|---|---|---|---|---|
| before | 86.7 | 53.3 | 46.7 | 40.0 | 33.3 | 40.0 | 33.3 | 46.7 |
| after | 86.7 | 66.7 | 40.0 | 26.7 | 40.0 | 46.7 | **33.3** | **26.7** |

Ukrainian unchanged, Polish thirteen points WORSE — and the languages
I never touched scatter by the same ±13, which is the finding inside
the finding: fifteen held-out rows makes one message 6.7 points, so
the instrument cannot resolve an effect of the size being looked for.

So the corpus-size conclusion stands with a fifth direction behind it:
it binds the MEASUREMENT as well as the model. Their mechanism is real
and English pays for it too; its cost cannot be measured on fifteen
rows a language, and neither can its repair.

Filed: `intent-english-corpus-twins` — the same guard found three
near-twin pairs in `labelled`, the corpus the shipped model is fitted
on, and fixing those moves every published number.

Gate: clean compile 0 warnings; okayIntentJVM 146, okayIntentJS 9,
okayDemo 79 — 0 failures. `okayDemo` was SIGTERMed once at load 10
with siblings on the box, and passed alone.

## intent-split-other — measured and DECLINED: carving the bin takes its recall from 46.7% to 6.7%
Completed: 2026-09-05
Landed as 8e9c12ad. `intent-other-is-a-bin` closed two remedies for
`Other` and left one open — give its members NAMES they can be learned
under. This is that one, measured, and it loses badly enough to close
the question at this corpus size.

Three groups, DERIVED rather than relabelled so every published number
stays comparable: `Social` (a pleasantry or personal news, no action
wanted), `Support` (something is wrong with a product or service),
`Errand` (a real request or question, out of domain).

| | accuracy | `Other` recall |
|---|---|---|
| unsplit, model alone | 61.7% | 46.7% |
| split, folded back | 55.0% | **6.7%** |
| unsplit, shipped composite | 76.7% | 46.7% |
| split, composite | 75.0% | **26.7%** |

And on the split taxonomy itself `Social`, `Support` and `Errand` all
score **F1 0.00** — the model never emits any of them, not once.

THE REASON IS NOT THE CARVING. The odd/even split leaves FIFTEEN
`Other` rows to train on, and three ways leaves 4-6 per class. Nothing
is learnable from four rows, so the classifier stops emitting those
labels and every message that was going to `Other` goes to a meeting
class instead — the exact production failure the lane set out to fix,
four times worse. A coarser two-way carve (`Social` 6 against
`Trouble` 9) also collapses: 55.0% accuracy, 13.3% recall. Row count,
not where the line is drawn.

Three remedies for `Other` are now measured and all three lost:
abstention (−20 points), splitting into names (here), and the status
quo — which is the best of the three.

Also worth stating plainly: `Other`'s F1 of 0.58 is not an outlier
beside `Notification`'s 0.77 and `Request`'s 0.80. The whole model is
weak at 60 training rows. What makes `Other` the one to worry about is
WHERE it fails, not how far — under-prediction routes out-of-domain
traffic INTO a meeting intent.

The remaining answer is rows, and this is the SECOND lane today to
arrive there from the other side (`intent-uk-pl-rows` reached it about
languages). Filed as `intent-other-more-rows`. The derived view and
the suite stay in the repository: the map is a usable artefact, and
the test is the guard that will say so if the answer ever changes.

Gate: clean compile 0 warnings; okayIntentJVM 141, okayIntentJS 9,
okayDemo 79 — 0 failures.

## script-temp-tests-watch-a-shared-directory — private temp root, not the shared one
Completed: 2026-09-05
Landed as 2dcd0c3e. `ScalaScript.compileOnly` created its compile
workspace against the JVM-wide `java.io.tmpdir`, and two tests proved
"no temp litter left behind" by snapshotting that SHARED directory's
`okay-script-*` entries before/after a run — sound property, unsound
check: any other process (a sibling worktree's own concurrent
okay-script tests, in a parallel matrix) creating a matching entry
between the snapshots failed the assertion for a reason unrelated to
either test's own cleanup. Fixed the same defect filed twice under two
names (`script-temp-snapshot-crosstalk`, `script-temp-tests-watch-a-
shared-directory`) with one change: `ScalaScript` and `Page` now take
an explicit `tempRoot: Path`, defaulting to the old lookup, so a
caller can point it at a private directory instead. Both tests do;
`TestScalaScript`'s (Live-tagged out of the default gate 2026-09-04)
is back in it.

## readme-docs-index-link — the documentation home page linked where it can be seen
Completed: 2026-09-05
Landed as 0fdcd57f. docs/README.md is the documentation's home page —
its first section is the "Start here" list — but README reached it
only through the last paragraph of the Documentation section, phrased
as an aside about module pages, so a reader going top to bottom met
the tables and never learned an index existed. It now opens the
section and says what it holds; the old mention becomes a
back-reference rather than a second link to the same place.

## readme-docs-first — the Documentation section moves to the top of README
Completed: 2026-09-05
Landed as 6e8f2017, on the operator's call. It sat at the very END of
README.md, after benchmarks and the upper layers, so a reader who
wanted the guide had to scroll past everything else to learn it
exists. It now follows the opening section directly and precedes
Architecture — where someone arriving at the repository is actually
looking. A pure move, nothing rewritten; one row added while there,
the from-scratch tutorial (docs/building-a-chat-app.md) joining
"Start here".

## intent-uk-pl-rows — two more languages, and the shipped model is measured to be English
Completed: 2026-09-05
Landed as 94dfe75e. Owed to a consumer who runs in Ukrainian and
Polish and asked twice. Thirty parallel meanings each, so the fixture
is eight languages wide.

STATED FIRST, because it is the disease this repository measured
yesterday: these rows are MY writing, and a native speaker should read
them before anything is claimed from them. They are worth adding
because a fixture row is CORRECTABLE — the consumer speaks both and
can fix a wording — and because the numbers are diagnostic even where
the prose is not. `TestSlavicRows` prints every Slavic row the tier
misreads, so the first thing a native reader meets is the list to
check, not a score.

THE SHIPPED MODEL IS ENGLISH, measured rather than cautioned:

| en | fr | de | es | ru | ja | uk | pl |
|---|---|---|---|---|---|---|---|
| 76.7% | 26.7% | 30.0% | 23.3% | 30.0% | 23.3% | 26.7% | 26.7% |

Four classes, so 25% is chance, and everything but English is at it.
The cue tier — the half that carries the composite — fires ZERO times
outside English because its cues are English phrases. Passing a
Ukrainian message to `Router.offline()` is a coin flip with a
confident face, and `Models` now says that in those words.

FITTING ON ALL EIGHT DOES NOT RESCUE IT: en 86.7%, pl 46.7%, fr 53.3%,
de 46.7%, es 40.0%, ja 40.0%, ru 33.3%, uk 33.3% on fifteen held-out
rows each — and in EVERY non-English language at least one class
scores F1 0.00, a class the tier never once produces. `CharGrams` is
language-agnostic BY CONSTRUCTION, which is true and is not enough:
the construction is free, the rows are not, and fifteen a language is
not rows.

Polish over Ukrainian is fifteen rows against fifteen and is NOT
evidence that Latin script beats Cyrillic; said so rather than left to
be read that way.

Gate: clean compile 0 warnings; okayIntentJVM 136, okayIntentJS 9,
okayDemo 79 — 0 failures.

## frame-walk-end-to-end — one message, one produced booking, and the walk caught its own defect first
Completed: 2026-09-05
Landed as 0e6c6d87. A consumer's third point, and the one I could not
argue with: both of their defects that day lived BETWEEN two correct
code paths with 237 unit tests green. One showed a person their OWN
address back — a two-argument contact lookup called with a fixed
direction. The other told the person who had asked, waited and been
accepted precisely nothing, because "they got your contact" was a
claim in a comment rather than a call.

`TestWalk` (in okay-demo, the module that can see both the router and
the suspension) goes from one message to one produced thing: classify
with the real tiers, fill from the message, park on the open question
in a journal, DIE, rebuild every object from scratch, answer, read
back, confirm, and ACT — asserting the act. Three things their shape
demands are asserted rather than implied: the value (`2026-09-08`),
the DIRECTION (the confirmation goes to the asker and names the other
party), and that anything was produced at all.

IT FAILED ON ITS FIRST RUN, FOR THE RIGHT REASON. The exchange
completed and `act` produced nothing — because `act` closed over the
descriptor value built at the top of the file while the frame came
from the one rebuilt after the simulated restart, and `valueOf`
matches a slot by IDENTITY. The walk's first act was to catch, in
itself, the exact failure it exists for: an exchange that ends
"complete" while the caller gets nothing. The rule is now written
where a caller meets it — one descriptor value per exchange, passed
alongside the frame rather than captured.

Filed rather than built: `frame-rebind`, which would make the restart
case ordinary. The hazard is why it waits — re-deriving an answer
means re-parsing its stored text, and "next Tuesday" against a new
reference day is a DIFFERENT DATE, which is the defect
`intent-frame-typed-values` removed.

Gate: clean compile 0 warnings; okayDemo 79, okayFrameJVM 17,
okayAgentJVM 121, okayIntentJVM 133 — 0 failures.

## intent-per-class-not-aggregate — the total was hiding a class, and it is the one that matters
Completed: 2026-09-05
Landed as 0fdb6d25. A consumer's finding, from their corpus rather
than this one: they filled a hole, one class reached 137 of 184 rows,
a probe leaned to the majority, "сегодня в москве шёл дождь" came back
as a REQUEST at 0.90 — and their headline accuracy ROSE, 95.8% to
96.2%, through the regression. Accuracy on an imbalanced corpus
rewards predicting the biggest class; their regression test caught
what the average never would.

Every aggregate this module publishes had the same exposure, starting
with the 76.7% now in a doc comment, a module page and two changelog
entries. `Eval` gained the numbers that close it — `support`,
`balance`, `majorityBaseline` on the matrix, `worst` on the report —
and the shipped model's tests now print per class and ASSERT rather
than describe.

THE GOOD HALF: the held-out set is 15 messages of each class, majority
baseline 25%, so 76.7% is not being carried by one class. Asserted
now, so a fixture drifting into imbalance fails a test instead of
quietly inflating a number.

THE HALF THE AGGREGATE WAS HIDING: `Other` at recall **0.47**.

| class | precision | recall | F1 |
|---|---|---|---|
| `Proposal` | 0.87 | 0.87 | 0.87 |
| `Request` | 0.70 | 0.93 | 0.80 |
| `Notification` | 0.75 | 0.80 | 0.77 |
| `Other` | 0.78 | **0.47** | 0.58 |

It misses more than half the messages that are not about meetings —
in production the worst class to be weak at, because out-of-domain
traffic routes INTO a meeting intent rather than out of the way. The
cue tier is right about every `Other` it fires on (precision 1.00) and
fires on half of them, so the recall is lost in the model tier, which
is what `intent-split-other` already said about a diffuse bin and now
has a number for. A per-class floor (F1 >= 0.50) is asserted alongside
so a class dying fails the suite.

Gate: clean compile 0 warnings; okayIntentJVM 133, okayIntentJS 9,
okayAgentJVM 121, okayDemo 76 — 0 failures. `okayDemo` failed once on
`TestChatDemo`'s canned-wire test with an HTTP EOF after 30s and
passed on a rerun — unrelated to this lane, and filed as the flake it
is rather than retried into silence.

## frame-choice-and-provenance — a closed choice, and an answer that knows where it came from
Completed: 2026-09-05
Landed as 60cf8f95. A consumer asked, blocked on the answer, whether
request 5's slot description was meant to carry value wordings and a
non-constant default. It was not — request 5 was a name, a question
per language and a parser, and that is exactly what shipped. Their
case was good enough that the library should hold it anyway: whether
a job can be done REMOTELY decides matching rather than wording, and
it is the same question on both sides of their market, which is what
makes it a type rather than a field.

`Slot.choice` takes a closed set of values, each with its wordings per
language. Those wordings do three jobs — offer the options
(`options(lang)`), read what a person typed in their own language, and
say the choice back in it (`show(v, lang)`) — which is why they belong
beside the value; the caller still writes every word. Reading accepts
a wording INSIDE the answer, because someone asked "on site or
remote?" replies "можно и удалённо, если так" and means it. Longest
wording wins so one value cannot swallow another, and every language's
wordings are matched rather than only the exchange's.

`Source` is the half their requirement demanded without naming it.
Their words: overridable by what they do say, and VISIBLE so they can
correct it. A default fires when NOTHING was said, so its value has no
evidence behind it and would otherwise be indistinguishable from one a
person typed. `Answered` now carries `Said` / `Found` / `Assumed`;
`assume` fills a slot, `assumed` lists them, `filled` shows them back
in the reader's own language so they can be corrected, and `words`
leaves them out — "what you told me" must not contain what nobody told
it. A person's answer beats an assumption in either order, tested both
ways.

Left out deliberately, and said so to them: the rule that reads "можно
и удалённо" out of a CITY answer, and what to assume when a question
goes unanswered. Both are domain knowledge. The library gives them a
place to be recorded honestly and no opinion about what they are.

Their other two points from the same message are FILED rather than
half-done — `frame-walk-end-to-end` (both of their defects today lived
between two correct code paths with 237 unit tests green) and
`intent-per-class-not-aggregate` (their headline accuracy ROSE, 95.8%
to 96.2%, while a class died, because accuracy on an imbalanced corpus
rewards predicting the biggest class — and every aggregate this module
publishes has the same exposure, starting with the shipped model's
76.7%).

Gate: clean compile 0 warnings; okayFrame JVM+JS 17 each, okayAgentJVM
121, okayIntent JVM 131 and JS 9, okayDemo 76 — 0 failures.

## idiomatic-headline-honest — section 6b's headline said the opposite of its own table
Completed: 2026-09-05
Landed as 879bb156. `docs/benchmarks.md` §6b opened with *"forced onto
equal footing, okay is ahead in every shape"* — which its OWN
matched-16 row contradicts (ZIO leads, 127.2 against 223.7) and which
the paragraph beneath it then walked back. The row leading the table
was `ZStream` at `chunkSize = 1`, a mode nobody writes.

`like-for-like-lanes` named this section as the first of three places
one mistake appeared, and fixed the other two. This is the third.

Rows reordered so the three that COMPARE come first and the DIAGNOSTIC
last, each labelled in a column — §6c's own rule (name the
granularity, name whether it memoises) carried into a table that
cannot rename its lanes. The reading is rewritten around what each row
can support: chunk-native compares and okay is 5.7x ahead; matched-16
compares and okay loses; timed flush is okay's by 20x; the forced
per-element row diagnoses what `chunkSize = 1` costs a library with no
per-element representation. `docs/guide.md` repeated the 12x as a
headline and now says what it is. No number was re-measured and none
changed — the reading was what was wrong.

TAKEN OVER from `idiomatic-api-compare`, claimed 2026-09-03 18:09 and
untouched for 29 hours, announced in the room before starting. Its
holder's worktree was left alone; while this lane ran, they returned
and landed `like-for-like-lanes`, which supersedes the piece I had
been about to land — so what would have been my finding is recorded as
what it now is: an independent REPLICATION of theirs.

That replication is one of two history rows. Their channel result
reproduced at 3 forks × 8 iterations on a quiet box — 215.3 ±7.6
against 220.7 ±7.0, control within 0.3% of theirs — confirming that
staying inside the program does not close the ~2x to `zio.Queue`.

The other row is a WARNING, not a result. My first run overlapped a
sibling's JMH on the same machine and reported `zioStepWhole` at 597.5
±430.1 where the same lane had measured 276.9 ±8.4 the day before. A
contended box does not fail — it widens the interval — so any ratio
taken from that window is fiction. Recorded so nobody quotes it, and
as the argument for announcing "box: taking it" in the room before a
benchmark run.

Gate: `compare/Jmh/compile` after `rm -rf compare/target`, 0 warnings.
No main or test source was touched.

## tutorial-chat-app — building a chat application on okay, from an empty directory
Completed: 2026-09-04
Landed as 1dac481f. docs/building-a-chat-app.md: the page a person
OUTSIDE this repository needed and did not have — an empty directory
to a running streaming chat, as a user of the library rather than a
contributor. Project scaffold, backend, frontend, tests, run.

Written by doing it. The whole application was built in a scratch
project first, every command executed in the order it appears and
every output quoted the one that came back; the build.sbt in the page
is the file that project was built with, copied back out of the page
verbatim and clean-built once more (uiJVM 2 green, app 3 green, the
bundle linked at exactly the path the page tells you to serve).

The awkward step is first and gets the most words, because a reader
cannot guess it: okay is not published, so `sbt publishLocal` —
measured at 38 seconds for 86 artifacts across JVM, JS and Native —
plus the coordinates (dev.okay, 0.1.0-SNAPSHOT, `%%` against `%%%`),
the TASTy floor (the consumer's Scala must be at least the library's
3.7.4), and the two roads not taken with reasons: a `ProjectRef`
source dependency is right while you are changing okay itself,
unmanaged jars lose the transitive dependencies.

The rest follows the shape the library has: a route is a
PartialFunction value composed with `orElse`, `Chat.Model` is the
whole model seam (scripted / OpenAI-compatible / Anthropic behind one
type), `provide` is the wiring, `Jetty.serve` answers a Resource. The
frontend splits the way okay-ui means it to — a pure cross-compiled
fold tested on the JVM with `Frame.render`, browser glue that decides
nothing — so the tests need no browser, no mock server and no docker:
a scripted model and port 0.

Two failures met while writing are kept as Troubleshooting rather than
silently fixed: `Transports` exists in both okay.llm and okay.http and
only one satisfies the model seam; and a forked `run` takes the
PROJECT directory as its working directory, so the relative path to
the linked bundle misses until `run / baseDirectory` says otherwise.

Linked from docs/README.md's "Start here" and from
docs/modules/okay-demo.md.

## intent-second-author — the corpus is not scoring itself, and the number still moves ten points
Completed: 2026-09-04
Landed as 4851a8c1, with a doc correction in 5001c226.

Every accuracy this module quotes was measured on a corpus written by
one hand, and this evening that became load-bearing: `Models.meeting`
ships and quotes 76.7% in a doc comment, a module page and two
changelog entries. A consumer demonstrated the failure four times
today, most sharply with three of twelve real answers taken by the
wrong class while leave-one-out did not move.

That cannot be fixed by writing more messages — a second corpus by the
same author measures the same thing twice — so this measures the GAP,
two ways that need no new data.

WHERE THE SCORE LIVES. Nearest-training similarity across the held-out
half has median 0.152 and maximum 0.328 by character-trigram Jaccard:
no near-duplicates, so 76.7% is not the fixture recognising its own
sentences. But split that set at the median and the far half scores
66.7% against the near half's 86.7% — twenty points inside ONE
author's corpus, and a different author is further out than the far
half is.

WHAT A MECHANICAL SHIFT COSTS, through `Router.offline()`: as written
76.7%, lowercased 76.7%, a hedge in front 76.7%, a trailing clause
73.3%, one deterministic typo 66.7%, the politeness frame removed
65.0%.

Three findings, two of them about tiers rather than the corpus:

1. The cue tier trades RECALL, never precision. Strip "Could you
   please" and it fires on 19 of 60 instead of 32 — and is right about
   all nineteen. That is the argument for putting it first and for
   never letting it be the only tier.
2. Character n-grams are NOT typo-robust here, though that is the
   usual argument for them: one transposition takes the model from
   61.7% to 55.0%. At 60 training rows the hashed 3-5-grams are too
   sparse for the redundancy that virtue depends on. Filed as
   `intent-typo-robustness`.
3. Casing and a hedge cost nothing — worth knowing before someone
   normalises input that did not need it.

CORRECTIONS TO EARLIER ENTRIES. `intent-fitted-model-ships` and
`intent-one-entry-point` quote a bare 76.7%. Read it as the ceiling:
`Models`, `Router` and `docs/modules/okay-intent.md` now say 65-70%
for a message somebody else wrote, with 76.7% named as what the model
scores on prose of its own register.

AND A COMMENT THAT DESCRIBED REMOVED BEHAVIOUR. `Router`'s class
comment still said "Under the last margin nobody guesses — a person
sees the candidates", which stopped being true inside the lane that
wrote it. The paragraph meant to replace it never landed: a scripted
edit matched no anchor and failed silently, and I did not check.
Found while trying to add the register numbers on top of it, which had
just failed the same way. Both are in now, and the anchor is asserted
before the edit.

A perturbed corpus is a LOWER BOUND, not the gap: a real second author
differs in vocabulary, length, structure and intent distribution at
once. `intent-second-author` stays open for the part no measurement
replaces — a corpus this repository did not write.

Gate: clean compile 0 warnings; okayIntentJVM 131, okayIntentJS 9,
okayDemo 76, okayDeploy 9 — 0 failures.

## frame-said-is-content — the words a parser cannot read are still what a person said
Completed: 2026-09-04
Landed as 6cf17f49. The one finding from the consumer's review of
`conversation-over-frame`, which they did BY MIGRATING — four
languages, a live intake, 226 tests green, all three of their asks in
use rather than merely compiling.

`said` read as an escape hatch and in their domain it is the other
half of the answer. A price slot parses money, and "negotiable", "по
договорённости", "договорімось" are things a listing legitimately says
that no parser will ever read. A caller reading only `filled` drops
them silently — their read-back did, and a test caught it.

So the doc comment says what `said` is, and `words` is the door they
had to write by hand: everything the person said, parsed or not, in
one map. `filled` stays the parsed half; `valueOf` stays the typed
one. Their own verdict on the semantics that caused it: mine are right
and storing unparsed words AS a `Money` was the defect — the case was
still worth naming, and it was.

Filed rather than fixed: `frame-language-with-grammatical-gender`. A
language whose question differs by the grammatical gender of the
ADDRESSEE needs more than a language code, or needs the caller to key
by `"pl-formal-f"` and own that choice. They raised it and could not
test it — their Polish addresses informally and dodges it.

Gate: clean compile 0 warnings; okayFrame JVM+JS 11 each, okayIntent
JVM 126 and JS 9, okayAgentJVM 121, okayDemo 76 — 0 failures.

## relaxed-queues-builder — relaxed FIFO, a builder, and the fourth defect of one family

`MultiFifo`: k independent buffers, a producer bound to one, a
consumer taking from whichever has something. What it gives up is the
order BETWEEN producers; what it keeps — and what makes it usable as a
channel's buffer — is that one producer's own elements stay in order,
which is exactly the law the suite states.

**It scales, and only over growable parts.** `Total=8000`, us/op:

| producers | one ring | relaxed, bounded | one growable | relaxed, growable | zio.Queue |
|---|---|---|---|---|---|
| 1 | 414.9 | 415.8 | 231.3 | 354.3 | 241.6 |
| 4 | 862.4 | 3873.0 | 401.0 | **177.9** | 740.4 |
| 16 | 3149.8 | **111546.4** | 741.8 | **169.9** | 2967.2 |

At sixteen producers the growable form is **17.5x** past `zio.Queue`
and faster than it was at one — 354 → 178 → 170. Adding producers
makes it quicker, which is the whole point of a relaxed queue and the
first time that scaling has appeared in this repository.

The bounded form is 35x WORSE than not relaxing at all, and the number
is kept rather than hidden because the cause is a design mismatch, not
a setting: the channel keeps ONE queue of waiting senders while the
resource is per part, so a freed slot wakes an arbitrary sender who
finds its own part still full and parks again — one useful wakeup in
k. Filed as `channel-per-part-waiters`.

`Buffer` grew four operations, each because something concrete broke
without it: `parts` (a relaxed buffer that quietly passes a global
FIFO test is a test that is not testing), `seal` (with k orders a
single end mark is met while other parts still hold accepted
elements — and a FULL part cannot take its mark, so sealing is
retried until every part has one), `route` with `pushDecidingAt` and
`hasRoomAt` (a parked send resumes on the WAKER's thread, so binding a
producer to a part by thread of execution scatters exactly the sends
that had to wait), and `hasRoom` (a sender asking `size < capacity`
about a partitioned buffer wakes itself forever, because the sum
having room says nothing about its own part).

Those are the second, third and fourth of one family, after
`isEmpty` vs `hasReady`: **the channel asking about the buffer as a
whole where the question belongs to one part.** The third was hidden
by the second — a sender that spun instead of parking never exposed
the routing bug, and it surfaced the moment the spin was fixed.

`Queues` is the builder: pick a contract, pick a mechanism. Sixteen
combinations, each of them built and run in `TestQueues`, because a
menu nobody orders from is a menu nobody checked. `AbruptChannel` now
takes a `Buffer` too, so the weak contract has the same menu as the
strong one rather than a hardcoded ring.

`docs/queues.md` is the long-form explanation: what a channel is, why
termination is the hard part (with the race written out step by step
and why the obvious in-flight-counter patch fails), the stamp scheme
traced by hand, how to choose, the measured tables, and the
literature — Vyukov, Michael & Scott, Okasaki, Koch–Sanders–Williams,
Herlihy & Shavit.

Gate 536, full matrix 2300, clean build, no warnings.
## intent-one-entry-point — the composed door, with the demo as its caller
Completed: 2026-09-04
Landed as 53ae71d3. The tier order was measured over twenty lanes and
then lived in ONE FILE in `okay-demo`, so a caller outside that demo
had to re-derive it by reading twenty Results sections.
`okay.intent.Router` is that composition: cues first (90.6% where they
fire, cost nothing), the vector tier next (85-88%, needs an embedder),
the shipped model last (61%, needs nothing). `Router.of` refuses a
tier whose classes are not in the taxonomy. `Action.Ask` carries how
many questions remain — the same fix the conversation consumer asked
for the same day, arriving in the other caller.

`okay.demo.IntentRouter` is a caller now and is SHORTER for it, which
is the shape a correct extraction leaves behind: what stayed is what a
caller owns — its taxonomy, its names, the frames its classes need,
the day the conversation is happening.

THE FLOOR, CHOSEN TWICE, AND THE SECOND MEASUREMENT THAT DECIDED IT.
On held-out English, raising the last tier's floor from 0.0 to 0.5
lifts precision among answered messages from 76.7% to 83.7% while
coverage falls from 60/60 to 43/60 — four tenths of a point per
abstention. That argued for zero, and setting it broke a demo test
asserting that "zzz qqq xxx" escalates to a person. The break was the
useful part: the held-out set is all IN-DOMAIN and cannot measure the
case a floor exists for. So I measured that case — margins on nonsense
run 0.13-0.89, median 0.437, against median 0.434 on real English. The
model is exactly as confident about garbage as about English, so no
threshold separates them and a non-zero floor buys the LOOK of caution
and none of it.

The default stays zero, the doc comment says why with both numbers,
and what replaces the property is a caller's explicit choice: load the
last tier for coverage, or leave it out and the tier below it is a
person. The demo now pins both — one test that nonsense gets a class
when the model is loaded, named so nobody mistakes it for an
endorsement, and one that a router built without the model still
escalates. Calibrated abstention already exists and is not a margin:
`NoModel`'s conformal threshold, whose promise is `None` when the
sample cannot support it.

`CharGrams.renamed` came with it: the demo's taxonomy is
domain-bearing and the shipped model speaks canonical names, so
without it the reference caller could not use the model this
repository ships. Same rule as `Cues.renamed` — total in both
directions, a partial map is an error rather than a silent bucket, and
only the labels move, not the weights.

Gate: clean compile 0 warnings; okayIntentJVM 126, okayIntentJS 9
(`TestRouterCross` runs on both), okayDemo 76, okayDeploy 9 — 0
failures.

## intent-fitted-model-ships — a model a caller can load, with the numbers it earns
Completed: 2026-09-04
Landed as d42c03ca. Nine tiers measured and none shipped: every fitted
model existed inside the test that fitted it, so a caller had the
types, the accuracy tables and no route from a pile of messages to a
working classifier.

`CharGrams` is the only tier that CAN ship. The vector tiers need an
embedder — a gateway on the network, or a distilled table somebody
builds first — so shipping one ships a dependency rather than a model;
the cue tier ships already but is not fitted at all.

`Models.meeting` is that fit, and `Fit` is the door a caller uses for
their own corpus: `Fit.grams(rows)`, `Fit.save`, `Fit.grams(json)`.

DIM CHOSEN BY MEASUREMENT, against the module's own default of 4096:
held-out English 61.7% at 1024, 63.3% at 4096, 58.3% at 8192 — more
dimensions stop helping at this corpus size — and 42KB against 170KB.

WHAT IT DELIVERS. Alone, 61.7%, which would not be worth shipping.
Behind the cue tier at FULL COVERAGE on 60 held-out English messages:
**76.7%**, with no network, no gateway and no fitting on the startup
path. The cues answer the 53% they fire on at 90.6%; the model answers
the other 28 messages at 61%. Both halves are asserted in tests, so
the doc comment cannot drift from the code.

NOT SHIPPED, DELIBERATELY: the six-language fit. Fifteen held-out rows
per language give fr 53-67%, de 40-47%, es 33-53%, ru 33-40%, ja
53-60% — noise at that size — and it costs English three points.
`CharGrams` is language-agnostic by construction and this fixture
cannot demonstrate it; `intent-language-fixture-growth` is the lane.

REPRODUCIBLE, NOT A BLOB: `MakeModel` writes the artifact and a test
asserts the committed bytes are exactly what the generator produces
from the same corpus — which holds because `CharGrams.train` is
deterministic. It is a generated SOURCE rather than a classpath
resource because this module is cross-built.

AND A GATE CORRECTION, found while proving that last point.
`TestModelsCross` was written to run on JS, so I ran `okayIntentJS/
test` — it finished in one second having run ZERO tests. This module's
`.jsSettings` point the JS test scope at `src/test/scala-cross`, which
did not exist. So today's entries for `intent-frame-typed-values`,
`intent-cues-for-a-taxonomy`, `intent-slot-extraction` and
`conversation-over-frame` say "okayIntent JVM+JS" where the JS side
was a COMPILE and no tests. (The okay-agent and okay-frame JS numbers
in those entries are real — those modules have cross suites.)
`scala-cross` is now non-empty and on both platforms' test paths.

Gate: clean compile 0 warnings; okayIntentJVM 121, okayIntentJS 4
(really run, this time), okayDemo 75, okayAgentJVM 121, okayDeploy 9 —
0 failures.

## conversation-over-frame — one slot model, the frame under the suspension
Completed: 2026-09-04
Landed as 99b3344c. Two slot models appeared in this repository on the
same day, and the consumer of the older one asked for the merge rather
than a rival. Each half had exactly what the other lacked:
`okay.intent` had the FRAME — typed values, an answer addressed by
name, a list of what is still unanswered — and
`okay.agent.Conversation` had the SUSPENSION, a straight-line intake
parked in a journal across a restart.

NEW MODULE `okay-frame`, with no dependencies at all. Neither of the
two may depend on the other: okay-intent's test scope reaches for
okay-agent's journal to replay recorded model answers, and sbt rejects
the cycle — verified rather than assumed (`recursive lazy value
okayAgent needs type`). The shared half being dependency-free is also
the honest description of it: a frame is data, and the things that
fill it — a date parser, a journal, a model — are not.

The three defects the consumer named, all closed:

1. Their `Slot.read` returned `Option[Json]`: it parsed an answer to
   check it was acceptable, stored the TEXT, and parsed it again
   later — the same defect `intent-frame-typed-values` had just closed
   in the other module. `Outcome.Filled` carries the FRAME now, and
   `valueOf(price)` is a `Double`.
2. `Conversation` answered only the pending question, so "Wrocław, and
   remote works" took the city and then asked about the terms it had
   just been told. `Frame.take` answers the named slot and offers the
   same sentence to every other slot's extractor, and the loop
   recomputes `missing` each round.
3. `Say.Ask` carries `remaining` — `Option[Int]`, because a journal
   outlives a deploy: an entry parked by the previous build decodes as
   `None`, meaning "not written down" rather than "none left", and a
   caller renders no count instead of a wrong one. Tested with that
   entry shape.

AND THE WARNING, WHICH CHANGED THE DESIGN. The consumer's: a language
must be an argument of the whole conversation, not a parameter of
every call — they measured an intake flipping language on a three-word
answer, on the second-to-last question of a profile. Both halves took
one per call. The frame carries it now: `in(lang)` once, where the
exchange begins, and no method takes another. `intake` has no `lang`
parameter at all, and the router takes it where it takes the day,
`Meeting(today, lang)`. `untranslated` names the slots that would
silently fall back to English, so a four-language intake can assert it
empty before it ships.

What the merge cost, stated for the review: the opaque caller-defined
`L` language type gives way to a code, because a language that must
survive a RESTART has to be writable to a journal — which is exactly
why the old runtime stored every rendered question as text.
`Frame.opening` gives way to a slot's own extractor. And
`Outcome.Filled` no longer means every slot parsed: it means the
exchange ended with a yes, `complete` says whether everything was
read, and `said(name)` keeps the words for a slot asked twice and
still unread — instead of storing them AS the value, which is how a
field typed as a number held a sentence.

Gate: clean compile 0 warnings; okayFrame, okayIntent, okayAgent on
JVM and JS, okayDemo and okayDeploy (the docs-index guard) — 342
tests, 0 failures.

## intent-slot-extraction — a frame is filled from the message it arrived in
Completed: 2026-09-04
Landed as 99af30b1. The end-to-end extractors: a slot may carry an
extractor, and `Frame.fillFrom` runs the unanswered ones over the
message BEFORE anyone is asked anything. The router now acts where it
used to ask — "Could you send me the agenda?" no longer ends in "What
would you like done?", asked of someone who had just said, and "Shall
we meet on Tuesday?" comes back as an action carrying `2026-09-08` as
a `Temporal.When`, with `Tuesday` beside it as the words it rests on.

`Found[A]` keeps that span because a value a person did not TYPE has
to be echoable, and the whole message is not an echo.

The value stays `Temporal.parse`'s own verdict over the whole message
and only the EVIDENCE is searched for — the shortest window of words
reproducing that same value — so extraction cannot disagree with
asking. Sliding a window and taking the first or longest hit would
have been a second parser with its own answers, and a frame filled by
extraction would then hold a different date from the same frame filled
by asking. The minimum is also shorter than a person would quote:
"next thursday at 2pm" yields `thursday at 2pm`, because a bare
weekday already resolves to the coming one.

FOUND A FAULT IN THE PREVIOUS LANE. `valueOf` identifies a slot by
identity, which is what makes its cast true — and `IntentRouter` built
`Slots.when(today)` INSIDE `frameFor`, where no caller could reach it,
so the typed value was unreachable through the very door
`intent-frame-typed-values` had opened (`Frame.slots` hands back
`Slot[?]`, and a wildcard cannot be asked for a type). Slots are now
held as values in a `Meeting(today)` the caller keeps, and `route`
takes that instead of a bare date. Found by writing a test with
`private def when` instead of `private val when` and watching `None`
come back.

Measured and filed rather than claimed: 5 of the 30 parallel meanings
carry a date in their English reading, and extraction finds 5/5 in
English, 0/5 in fr, de, es, ru and ja. `Temporal` is English; the
router degrades the right way, by asking in the reader's own language;
`intent-temporal-multilingual` is the lane, filed with the shape of
the fix. `intent-extract-more-slots` filed beside it.

`intent-taxon-wired-to-tiers` was claimed and released UNSTARTED to
take this lane — no code was written, and it stays in the backlog
unclaimed.

Gate: clean compile 0 warnings; okayIntent JVM+JS, okayDemo, okayAgent
— 304 tests, 0 failures.

## intent-cues-for-a-taxonomy — a cue set carries the taxonomy it decides
Completed: 2026-09-04
Landed as ae449ebd. `Patterns.Cues` pairs the cues with a `Taxon` and
is built only through `Cues.of`, which refuses a cue naming a class
the taxonomy does not hold. `renamed` moves a set onto another
taxonomy and is TOTAL IN BOTH DIRECTIONS — every class the cues use
must be named, and every name given must be a class the destination
holds.

That totality is the whole point, and it is what a `Map` would not
have bought. `IntentRouter` translated the canonical names in a
`match` ending in `case _ =>`, so a class the author forgot — or one
added upstream later — went to `NotAboutMeetings` silently and
forever; `getOrElse` is the same defect in different syntax. A match
over strings is total, so no test can see the hole. Now the router
holds a renamed set and has DELETED both the translation and the
`.filter(taxonomy.has)` standing downstream of it: the filter existed
only because the translation could produce anything.

`Cues.silent` names the classes no cue can reach — empty for the
shipped set, and worth having measured rather than assumed, since a
cue set that cannot produce one of its own classes has a recall
ceiling nobody would find by reading it. `Cues.unsafe` is used once,
by `Patterns.meeting` itself, where a failure would be a bug in that
file rather than a caller's mistake.

Gate: clean compile 0 warnings; okayIntent JVM+JS, okayDemo, okayAgent
— 296 tests, 0 failures. Re-run after rebasing onto the sibling's
`fifo-array-front`, which had touched core `Fifo`/`Channel`.

The two SIGTERMs this lane hit were a sibling's sbt at `-Xmx6g` with
load average 27, not the repo's own broken matrix: the same command
passed at load 17 with nothing else running.

## fifo-array-front — two persistent buffers, and the seams opened to callers

`Fifo` becomes a seam with two implementations, measured side by side
because separate runs could not settle it: `zioStrongChunk`, whose
code nobody touched, read anywhere from 116 to 138 in one day, so a
host with sibling builds on it hides a 5% effect completely. Both
variants now run as lanes in the SAME invocation, with an untouched
zio lane as the control.

`ListFifo` is the banker's pair of lists. `ArrayFifo` carries
`Segments`' idea into a persistent structure — the front an immutable
chunk plus an index, each version keeping its own — so turning the
back round allocates one array instead of n cons cells, `dequeue` is
index arithmetic, and `drop` within the front only moves the index.

Elementwise **271.4 → 253.0**, a 7.3% gap whose error bars still
overlap; chunked is a tie (131.7 vs 129.4), exactly as predicted,
since the batched path stopped materialising a reversed list in
`stm-fifo-post-cas`. Three independent measurements all favour the
array, which is the evidence: a difference of zero would flip sign
about half the time. `ArrayFifo` is the default, `ListFifo` stays
named.

**The pieces are now public.** `Buffer`, `Ring`, `Segments`, `Fifo`,
`ListFifo` and `ArrayFifo` were all `private[okay]`, so the menu this
design had become could not be ordered from: a caller could pick a
channel by name and nothing else. `SentinelChannel.over` takes a
POLYMORPHIC buffer factory, because the channel holds more than the
caller's element type — termination travels as a mark through the same
buffer — so only the channel knows what it needs to allocate, and
`Mark` stays private, which means no caller can forge an end of
stream.

```scala
SentinelChannel.over[Int](1024)([T] => (n: Int) => Ring[T](n))
```

Gate 500, full matrix 2259, clean build, no warnings.
## intent-frame-typed-values — a filled frame hands back the value, not the text again
Completed: 2026-09-04
Landed as 6a5b8e4d. The blocker the first caller found, and the one
that made "a label cannot be acted on; a filled FRAME can" untrue in
the code that promised it. `Slot[A]` knew its type, parsed an answer to
prove it was acceptable, and `Frame` stored the raw TEXT — so
`IntentRouter`, having just established that "next thursday" was a
date, got the string back and parsed it a SECOND time, with the same
reference day, which nothing in the type told it to remember.

`Frame` keeps `Answered` now: the slot, the text a person typed, and
the value it parsed to. `valueOf` takes the SLOT rather than a name,
and that is the mechanism rather than a convenience — the slot is the
evidence that this answer has type `A`, so there is no way to ask for a
type the slot never had.

ON THE ONE CAST. `valueOf` contains an `asInstanceOf`, and this
repository's rule permits one only with a real necessity. This is the
shape the rule names as its own exception — a heterogeneous map keyed
by identity — isolated in a single function whose guard is what makes
it true: the value comes back only when `a.slot eq s`, so it was
produced by THIS slot's parser and no other. The guard is tested rather
than asserted, with a second slot of the same NAME and a different type
handed nothing.

`filled` survives as the text view, because a frame shown back to a
person should show what they typed rather than what it parsed to. And
the caller's test that recorded the defect now pins the property, which
is the shape a fixed friction should leave behind.

Gate: clean compile 0 warnings; okayIntent JVM+JS, okayDemo and
okayAgent, 290 tests, 0 failures.

## intent-end-to-end — the first caller, and the three things it broke
Completed: 2026-09-04
Landed as 848f7a3a. Twenty lanes measured these tiers and nothing used
them: inside okay there was no path where a message arrives and a
decision leaves. A consumer had their own router; `okay-intent` had no
caller of its own, and a library with no callers has the wrong API and
cannot find out.

`okay.demo.IntentRouter` is the caller — deliberately a ROUTER rather
than a classifier demo, because the interesting part is what happens
AFTER the class: the frame it needs, the question it is missing, and
the decision to ask a person instead of guessing. Its tier order is the
one the measurements argued for, and it runs in the default gate with
no model and no network.

WHAT MATTERS IS THE THREE FRICTIONS IT EXPOSED, none of which any test
had found.

A FILLED FRAME HANDS BACK TEXT, not the parsed value. The router knows
the meeting is on the 10th — `Temporal` parsed "next thursday" to prove
the answer was acceptable — and `Frame.filled` can only return the
string the user typed. To act, the caller parses it AGAIN, with the
same reference day, and nothing in the type says so.

THE PATTERN TIER SPEAKS CANONICAL NAMES, so a caller with a
domain-bearing taxonomy writes a mapping. `IntentRouter` carries a
private `canonicalToTaxonomy`, and every caller after it will write the
same one.

`Taxon` IS CONNECTED TO NONE OF THE TIERS THAT CLASSIFY. Request 1
asked for one taxonomy both tiers read; what landed is one taxonomy
neither reads — `Classify` takes a `Schema[I]`, `Patterns` takes cues,
`Centroid` takes whatever labels it was fitted on — with the caller
checking `taxonomy.has` by hand afterwards.

None is fixed here. The lane existed to find what a caller has to work
around, and quietly repairing them would have hidden the answer: they
are filed as `intent-frame-typed-values`, `intent-cues-for-a-taxonomy`
and `intent-taxon-wired-to-tiers`, and the router keeps its workarounds
visible so the next reader can see the shape of what is missing.

Gate: clean compile 0 warnings; okayIntent, okayDemo and okayDeploy,
180 tests, 0 failures.

## intent-russian-rows-fixed — the fixture was flattering itself, and fixing it cost 13 points
Completed: 2026-09-04
Landed as dfafec09. The last of what the consumer's review left owed,
and the one where the defect was mine. All three hazards they warned
about were present in my thirty Russian rows.

THE PERSON MARKER CARRIED THE CLASS, ON ONE LETTER. "Не могли бы ВЫ
забронировать" (Request) against "Не могли бы МЫ начать" (Proposal);
"МОЖЕТЕ проверить" (Request) against "МОЖЕМ встретиться" and "МОЖЕТ,
встретимся" (both Proposal) — three spellings of one word across two
classes, and the third not even the same part of speech.

TEMPLATE DUPLICATION. Eight Requests in three shapes, four opening
"Пожалуйста, <imperative>", two of those the same sentence with the
object swapped — plus a true near-duplicate pair inside `Other`, so
leaving one out leaves its twin in the training half.

TRANSLATIONESE. "Переговорная изменена на B2" (a room is not "changed
to"), "С этого момента четверги удалённые" (a calque), "Подойдёт ли
пятница утром". None wrong enough to fail a reader; all three wrong
enough that the row is evidence about my English source sentence rather
than about Russian.

Ten rows rewritten by CONSTRUCTION rather than by word — swapping "вы"
for a synonym would keep the class resting on one letter — and one
MEANING replaced across all six languages, since the duplicated
complaint could not be fixed in Russian alone without the parallel set
ceasing to be parallel. After: no "не могли бы" rows, both remaining
"мож-" openings inside ONE class, maximum pairwise Jaccard 0.20, zero
pairs above 0.5.

AND THE NUMBER WENT DOWN, WHICH IS THE POINT: Russian 86.7% → 73.3%,
English 73.3% → 80.0% (every language's arm moved, since the replaced
meaning is in all six). Russian lost 13 points by being fixed. That is
what a fixture defect looks like from the inside — the twins and the
dominant template made the task easier than the task is, and 86.7% was
measuring my fixture rather than the classifier.

Both arms still train on fifteen rows, below the thirty-two where the
probe stabilises, so the swings carry noise; the DIRECTION is the one
the review predicted, and a fixture edit that changed nothing
measurable would have been one nobody could check.

The provenance problem is unchanged and not pretended otherwise: I
rewrote my own rows, so it is still one hand's Russian. The review
bought the removal of the defects; it cannot buy a second author, which
is filed as `intent-second-author`.

Gate: clean compile 0 warnings; okayIntent JVM+JS, okayAgent and
okayDeploy, 223 tests, 0 failures.

## stm-fifo-post-cas — the transaction needs the final state, not every state on the way

`StmChannel`'s batched receive rebuilt its persistent queue ONCE PER
ELEMENT inside a single transaction — four thousand intermediate
buffers nobody would ever look at, 23% of the lane's profile in
`Tuple2` and 8% in `Queue.dequeue` — and it did all of it BEFORE the
CAS, so a losing transaction paid for the lot.

Two changes, one idea. `Fifo` replaces `immutable.Queue` as the
buffer: the same two lists, but it exposes them, so a batched take can
be written against them instead of through `dequeue`. And the O(k)
work — allocating the chunk and filling it — moves into the action,
which `transact` already runs only after the CAS has won. The state
transition is then arithmetic: taking the whole buffer, the common
case, leaves the empty one.

`List.reverse` does not get faster, it stops being needed. The back is
newest-first, so its FIFO order is its reverse; `fill` walks the back
FORWARD and writes at DESCENDING indices, and the order falls out of
the arithmetic with no list allocated. Reading the old lists after the
CAS is safe: they are immutable, and the CAS already made the take
exclusive.

**128.0us → 114.5**, which puts the last lane past `zioStrongChunk`'s
123.8 — every pair in the table is now ours. Elementwise 235.7 → 223.4.

The estimate was wrong in a way worth recording: I predicted ~70us
from removing 45% of the profile, and got 10.5%. Profile shares are of
samples across all threads, not of the lane's wall time — the receive
transaction was never the sole serial bottleneck, so work removed from
it partly hid behind the producer's.

Inherent and staying: one `State` per transaction, which is the price
of STM composability, and the amortized reverse on the elementwise
path when the front runs out.

`TestFifo` checks the structure against a plain list. Its first
property run caught a real defect — the remainder of a PARTIAL take
came back reversed — on a branch the channel's own laws never reach,
because they ask for more than is buffered.

Gate 500, full matrix 2247, clean build, no warnings.
## intent-slot-descriptor — what a slot is, proposed for review
Completed: 2026-09-04
Landed as 0cc3afac. Request 5 of the consumer's seven, the last still
open, PROPOSED rather than declared finished: they said write the shape
and they would bring real usage to the review instead of specifying
from outside a second time.

The Overview has promised since the first lane that a label cannot be
acted on and a filled FRAME can, and no type held a frame. `Temporal`
parsed one slot in one language and nothing said what a slot IS, so a
second language was a rewrite and a learned tagger would have been a
rival design.

The shape is theirs: a slot is a NAME, a QUESTION per language, and a
PARSER whose failure is a re-ask.

    final case class Slot[A](name: String, ask: Map[String, String],
                             parse: String => Option[A], required: Boolean = true)
    final case class Frame[I](intent: I, slots: Vector[Slot[?]],
                              filled: Map[String, String] = Map.empty)

Three things follow that did not before. `Temporal` becomes one
implementation of `parse` rather than a special case (`Slots.when` is
it, wearing the descriptor); another language is another `ask` entry
and another parser; and `intent-crf-slots`, when it comes, is an
alternative `parse` behind the same seam.

`read` returns the QUESTION on failure rather than an error, because
the caller's next move is to ask, and an error string would have to be
turned into a question at every call site in every language.
`Frame.answer` returns the frame UNCHANGED when a parse fails — the
property the consumer asked for by name: a slot that cannot read an
answer must not store it, since keeping the raw string is how a field
typed as a date comes to hold "next thursday".

`missing` is why the type exists: the distance between having a class
and being able to act is a list of unanswered questions in the reader's
language, not a boolean.

IT IS DELIBERATELY NOT A CONVERSATION. The descriptor describes — no
session state, no knowledge of what has been asked, no decision about
when to ask. The classifier stays a pure function of a message, which
is what keeps it testable, cacheable and foldable, and that is worth
more than the convenience of putting a dialogue here. Suspension stays
`Conversation`'s, in okay-agent, on `Durable`.

Gate: clean compile 0 warnings; okayIntent JVM+JS, okayAgent and
okayDeploy, 223 tests, 0 failures.

## intent-state-the-framing — a row cannot be written without its terms
Completed: 2026-09-04
Landed as 336368c7. The defect behind this afternoon's retraction, not
a nice-to-have. Two measurements an hour apart disagreed by ten points,
the disagreement was read as a finding and published, and a re-read
then showed the runs had embedded their messages differently — one
bare, one with a classify instruction — with neither row saying which.

A CONVENTION WOULD NOT HAVE HELPED, because a convention is exactly
what there was. `Conditions` makes the terms part of writing a row, and
there is deliberately no `line` overload that omits them:

    human + 40 distilled   probe 86.7%  centroid 90.0%
      [embedder=Qwen3-Embedding-0.6B  framing=bare  train=100 test=60
       corpus=both  distilled=40]

That is the retracted cell, now printing the `framing=bare` whose
absence made it look comparable to a framed one.

Live-scope on purpose: a deterministic test carries its conditions in
its own source, and only a measurement against a moving world — a
gateway, a model, a corpus on disk — needs to say what the world was.

One thing the first version got wrong, which is the same class of error
a level down: the distilled count was DERIVED as `rows.length -
trainH.length` and printed `distilled=260` for the arm that has no
human rows at all. A condition that lies is worse than one that is
missing, because it invites exactly the comparison it misdescribes.
Counts are passed now, not inferred.

Gate: clean compile 0 warnings; okayIntent JVM+JS, okayAgent and
okayDeploy, 216 tests, 0 failures. Full matrix still broken on master.

## intent-other-is-a-bin — the diagnosis holds, the remedy does not
Completed: 2026-09-04
Landed as 4d97b233. A consumer's observation, promised in the room and
owed: `Other` holds social pleasantries and support complaints, which
share nothing, so a centroid over them is a point between two clouds.
Every per-class number in this spec counted `Other` as a class, so the
answer reaches back through the programme.

TWO THIRDS OF THE PROBE'S LEAD OVER THE CENTROID LIVES IN `Other`: +2
of a +3 overall lead, on 15 of 60 rows. A quarter of the rows carrying
two thirds of the gap means several lanes of tier comparison were
partly a comparison of how two models cope with one incoherent class.

IT IS INCOHERENT, AND IT IS NOT TWO CLUSTERS. Mean pairwise cosine:
within pleasantries 0.645, within complaints 0.560, ACROSS the two
halves 0.551 — against 0.782, 0.705 and 0.684 for `Proposal`,
`Request` and `Notification`. Every real class is tighter than anything
inside `Other`, and the across-halves figure sits inside the
complaints' own spread, so the proposed split is not the structure
either. One cloud with no shape.

AND YET AS A REJECTION IT IS TWENTY POINTS WORSE. Fitting only the
three positive classes and answering `Other` below a confidence floor
peaks at 68.3% for both tiers, against 88.3% with `Other` as a class.
The reason is the diffuseness that suggested the remedy: an incoherent
bin can still be LEARNED when its members are individually distinctive
— "charged twice", "the app crashes", "password reset link expired"
have their own vocabulary — and rejection-by-threshold discards that
evidence for the weaker signal "none of the three fit".

So `Other` is not a class in the geometric sense the other three are,
its incoherence explains most of the centroid's disadvantage, and the
fix is not abstention. What remains is the consumer's other option,
filed as `intent-split-other`: split it into named classes that are
individually coherent.

One measurement defect of mine, recorded because it nearly became a
finding: the first sweep gave both tiers the same floors, though a
probe margin is a difference of PROBABILITIES and a centroid margin a
difference of COSINES. That made the centroid abstain on everything
above 0.1 and report 25% accuracy as though it meant something.

Gate: scoped as the split lane's — clean compile 0 warnings; okayIntent
JVM+JS, okayAgent and okayDeploy, 213 tests, 0 failures. The full
matrix remains broken on master (exit 143 at ~1449 tests on an
untouched checkout), reported to the room.

## intent-module-split — okay-intent, and the JSON Schema algebra goes where it belongs
Completed: 2026-09-04
Landed as 62e92751. The only one of the consumer's seven requests
asking for a BOUNDARY rather than a type, and the one they turned from
a preference into a decision by volunteering to eat the migration.

Fourteen files that turn a message into a class and a frame moved from
`okay.agent` to `okay.intent`. Nothing else moved: `Agent`,
`Provider`, `Stepper`, `Durable`, `Rerun`, `ToolSpec` and
`Conversation` all stay, so a caller importing any of those is
untouched.

ONE FACTORING TURNED A CIRCULAR DEPENDENCY INTO A SPLIT, and it
improves okay-codec on its own terms. Only `Classify` reached back into
okay-agent, for `ToolSpec.jsonSchema` — the Schema → JSON Schema
algebra, the FOURTH over `Schema[A]` after Json, Cbor and YAML, which
never had anything to do with agents. Moved to `okay.codec.JsonSchema`
with `ToolSpec` delegating in one line, okay-intent depends on
okay-codec and okay-rag and NOT on okay-agent. The live suites keep
test-only dependencies on okay-agent (its journal, for replaying
recorded answers) and okay-llm (a gateway), named in the build rather
than left implicit.

`Conversation` STAYS, on the consumer's argument rather than my
instinct: it is built on `Durable`, whose journal is its state, so
moving it would have recreated exactly the circularity the `JsonSchema`
move had just removed. Their formulation is the better one — a
suspension mechanism belongs with the runtime it suspends, not with the
classifier that happens to sit beside it in a caller's code.

Two things the CLEAN compile and the docs guard caught that a module
compile did not: the moved suites summon a `Handler[Async]` needing a
`CanBlock` that JS lacks, so they fail to COMPILE there — okay-intent
now scopes its JS test sources the way okay-agent already did. And
`okay-deploy`'s `TestDocsIndex` failed because a new module had no page
and no index entry, which is that guard doing exactly its job.

GATE, STATED HONESTLY: scoped, not full. okayIntent JVM+JS, okayAgent,
okayCodec and okayDeploy — every module this touches plus the docs
guard — are 293 tests, 0 failures, in 17 seconds, on a clean compile
with 0 warnings. The FULL matrix could not be got green because it is
currently broken on master: `sbt test` on an untouched main checkout
dies at exit 143 after 1449 tests, against 1445 on this branch. Same
failure four tests apart on trees differing by one module, so it is not
the module; reported to the room with the measurement rather than
diagnosed, since two earlier attributions of a 143 today were refuted
by controlled experiment.

## segments-two-consumers — the read pass must keep the segment the scan found

A correctness defect in `channel-ring-unbounded`, found by dumping a
process instead of killing it.

`Segments.popMany` re-derived its segment AFTER winning the head CAS,
through `segmentFor(headSeg, ...)`. With more than one consumer the
other advances `headSeg` past the claimed run in between — and the
hint-is-ahead fallback added the day before cannot help here, because
the hint IS `headSeg`. The read pass then walks a later segment and
finds a null slot. The `NullPointerException` died unseen inside a
virtual thread, the consumer vanished, and the law waiting on its
counter spun for nineteen minutes.

The fix is to keep the segment the SCAN used. It is the segment of
`pos`, obtained before the CAS was won, and segments are never freed,
so holding the reference is the whole guarantee.

Proved rather than argued: with the defect, 2 rounds in 40 fail with
that NPE; with the fix, 40 of 40 are clean.

The test took two tries to become a test. The first version passed
WITH the defect present — caught only because reverting the fix is now
a step, not an afterthought. The second caught it only on a loaded
box, and a test that needs a busy machine is not a test. What catches
it is contention for the hint: four slots to a segment, so a
64-element run crosses sixteen boundaries, and eight consumers, so
someone is nearly always advancing `headSeg` while someone else is
reading. The defect fails it; the fix passes it in 0.437s.

Gate 495, full matrix 2239, clean build, no warnings.

## intent-centroid-reconsidered — the 90.0% headline does not reproduce, and I did not hold the framing fixed
Completed: 2026-09-04
Landed as 0b964d29. Three conclusions in this programme turned on the
PROBE's number specifically, so all three were re-run with the centroid
as the subject. One of them was mine from an hour earlier, and it did
not survive.

| distilled added | probe (framed) | centroid (framed) | centroid (bare, as reported) |
|---|---|---|---|
| 0 | 88.3% | 83.3% | 80.0% |
| 20 | 88.3% | 86.7% | — |
| 40 | 85.0% | 85.0% | **90.0%** |
| 120 | 80.0% | 75.0% | 86.7% |

THE DIFFERENCE IS A CONDITION I FAILED TO HOLD FIXED. The distillation
lane embedded messages BARE; this one embeds them with the classify
instruction an earlier lane had already shown to be better. Framed, the
centroid's peak gain from distilled rows is +3.4 at 20 rows rather than
+10 at 40. On sixty held-out messages that is two messages against six,
so both sit near the noise floor and a single cell was over-read.

What survives in BOTH conditions, well outside the noise, is the
DECLINE at larger doses — 75.0% at +120 framed, 78.3% at +320 bare.
Distribution shift is real; the small-dose gain is not established.
Corrected claim: distilled data does not lift the centroid to the model
tier's level, and the probe remains the better tier on this fixture
(88.3% against 83.3%).

The other two re-readings stand. The 4B embedder is worse for the
centroid too (80.0% against the 0.6B's 83.3%), so that verdict was not
an artifact of the probe's parameter count. And the classify
instruction helps the centroid MORE than the probe (+3.3 against +1.6),
so the default was recorded with the weaker of its two reasons.

THE REAL DEFECT is that two measurements an hour apart differed in a
condition neither printed. Filed as `intent-state-the-framing`: every
live arm prints its embedder, framing, split and corpus beside its
number. The retraction is the symptom; the free variable is the cause.
Retracted to the consumer in the room before landing, since they had
said they would adopt the centroid if the result held.

Gate: clean compile 0 warnings; full matrix 2238 tests, 0 failures.
(One killed run in between failed `okay.persist.TestRaftWire`'s leader
election — the `raft-wire-election-flake` already on the board, green on
the rerun.)

## channel-ring-unbounded — every capacity gets a ring

`Segments`: a linked list of fixed arrays behind one pair of position
counters. A claim is still one atomic — `getAndIncrement`, since an
unbounded buffer can never refuse — and `popMany` still takes a run of
published positions with one CAS, across segment boundaries. A
`Buffer` seam lets `SentinelChannel` take either store, so
`Channel.apply` now gives every capacity a ring: bounded a fixed one,
unbounded a segmented one. `StmChannel` keeps capacity below two,
which is a rendezvous the stamp scheme cannot express, and keeps being
the one implementation with STM composability.

Segments are never reused and never freed by hand. A thread reaches
one by holding a reference, and a segment nobody holds is garbage —
so the hazard a segmented queue usually carries, freeing a segment
while a batched scan still walks it, has nowhere to live. The stamp is
simpler than a ring's for the same reason: each position is used
exactly once, so a slot needs one bit of state rather than a lap
number.

Three defects, none where I first looked.

`fail` closed the channel; `StmChannel` only records. The difference
shows in `merge` — one source failing must not lose the healthy
source's elements, so the failure belongs at the END of the stream.
Now a law, and split in two by tier: `AbruptChannel` cannot promise
the failure arrives last, because it promises nothing arrives after
close.

`receiveAsync` rechecked `isEmpty` before parking, which counts a
claimed-but-unpublished position as ready — so the consumer spun
instead of waiting, and the CPU it burned came from the publisher it
was waiting for. Two stores wide on a ring; a whole segment allocation
wide here. `Buffer` now separates `isEmpty` from `hasReady`. The
sender side has the same shape, filed as `channel-sender-livelock`.

`segmentFor` walked forward from a hint that could be AHEAD. Producers
claim with one increment and publish out of order, so a thread at
position 800 could find the hint at the segment for 1100 and, with no
backward link, write its element into another position's slot: 2000
sent, 1502 received, **498 consecutive lost**. The head is the anchor
that is never ahead, since it only advances past published positions.

`TestSegments` covers the buffer directly — small segments, many
producers. Reverting the fix fails two of its three tests, the first
in 0.136s. That matters: the full matrix passed WITH the defect
present on one run, so a green matrix was not evidence.

Gate 494, full matrix 2207, clean build, no warnings.

**The numbers, on a quiet box** (`ChannelGuaranteeBenchmark`, N=4000,
cap=1024, us/op; only lanes sharing a guarantee AND a granularity
compare):

| pair | okay | zio | |
|---|---|---|---|
| unbounded, chunked | **49.1** | 383.8 | 7.8x |
| unbounded, elementwise | **110.5** | 439.1 | 4.0x |
| bounded strong, chunked | **56.2** | 125.9 | 2.24x |
| bounded strong, elementwise | **248.6** | 286.9 | 1.15x |
| weak, chunked | **54.8** | 116.1 | 2.12x |
| weak, elementwise | **237.6** | 304.0 | 1.28x |
| `StmChannel`, elementwise | **235.7** | 286.9 | 1.22x |
| `StmChannel`, chunked | 128.0 | 125.9 | zio, 1.6% |

Every lane `Channel.apply` can hand you is ours. The one that is not
is `StmChannel` at chunk granularity, now within 1.6% — a hair rather
than the 6.9% it was — and it is no longer the default for any
capacity of two or more. It stays for the rendezvous case and for the
STM composability nothing else offers.
## intent-distil-for-probe — forty distilled rows are worth ten points to the CENTROID
Completed: 2026-09-04
Landed as c0702617. The learning curve found the probe flat past 32
examples and this spec concluded labels are not its constraint. That
was drawn on ONE author's sentences in one register, and the
distillation lane had since shown the generated corpus is distributed
differently — so the flatness might have been homogeneity rather than
quantity. The corpus already existed; it only had to be embedded.

| trained on | rows | probe | centroid |
|---|---|---|---|
| the human fixture alone | 60 | 86.7% | 80.0% |
| + 40 distilled | 100 | 86.7% | **90.0%** |
| + 80 distilled | 140 | 83.3% | 88.3% |
| + 120 distilled | 180 | 83.3% | 86.7% |
| + 320 distilled | 380 | 73.3% | 78.3% |
| distilled alone | 320 | 50.0% | 63.3% |

90.0% IS NOW THE BEST NUMBER IN THE PROGRAMME — matching the model
tier's ~90%, from the SIMPLEST tier there is, at one embedding call and
no generation. The probe, the headline for two lanes, does not move.

MORE IS WORSE, MONOTONICALLY, for both: 90.0 → 88.3 → 86.7 → 78.3 as
the distilled share grows. Distribution shift doing what it does — a
mean broadened by a few diverse examples is a better mean, one dragged
by three hundred is a mean of the wrong population. The probe suffers
more because it fits a boundary, where the generated labels' noise
lands, while a centroid averages it away.

BOTH OF MY EARLIER READINGS WERE HALF RIGHT. "The probe is data-bound"
was wrong: it is register-bound, and different data does not help it
either. "Labels are not the constraint" was wrong for the centroid,
which gained ten points from forty of them. The quantity that mattered
was small, and the tier that mattered was the one I had stopped looking
at.

60 held-out messages, so ten points is six of them, and the claim rests
on the monotone shape of the column rather than the best cell. These
are the UNFILTERED 320; whether the self-consistency filter moves the
optimum is filed (`intent-distil-dose`), with re-reading the programme
around the centroid (`intent-centroid-reconsidered`).

Gate: clean compile 0 warnings; full matrix 2216 tests, 0 failures.

## nomodel-real-distribution — the losing classes were a share split evenly, not a ranking
Completed: 2026-09-04
Landed as fdcf0d97. `NoModel.blend` had no way to ask the probe about
every class at once, so it asked about one at a time and invented the
rest: `probabilityOf` gave the winner `p(best)` and handed every other
class the SAME `(1 - p(best)) / (n - 1)`.

Exact for two classes. Fiction for three or more — and fiction in
exactly the part `intent-consumer-seams-a` had just built for two named
consumers. `Verdict.ranked` was ordered arbitrarily below rank 1
because every loser tied; `runnerUp` was whichever of those ties
`sortBy` met first; and the cue bonus landed on top, so a pattern could
lift the class the probe ranked LAST past the one it ranked second. An
interface offering a person the two candidates it could not separate,
and an example-selector ranking on uncertainty, read precisely that.

`Probe.ranked` (2052e5a3) is the seam it lacked, and `blend` now reads
the distribution in one pass rather than n calls to `score`.

WHY THE SUITE WENT GREEN THROUGH ALL OF IT. `TestNoModelCalibration` is
built on two classes, A and B, and at two classes `(1 - p) / 1` is
arithmetically right. Every assertion passed honestly while the ranking
below the winner was made up. The new fixture is three corners; on the
old code it prints `north 0.0018245361588968279, up
0.0018245361588968279` — bit-identical, the fabrication in plain sight.
A property about a DISTRIBUTION needs three of something before there
is one to have a property.

REQUEST 3 OF THE CONSUMER'S SEVEN TOOK THREE LANES, and the spec now
says why it could not take fewer: one added the field, one exposed the
value, and this one connected them. The middle was a seam nobody could
reach for until it existed — which is the argument for filing a seam
request early even when the consumer can work around it.

A note for persistence: `fit` calibrates `patternWeight` and
`threshold` through `blend`, so anything fitted from here is
calibrated against the corrected geometry. A model fitted BEFORE this
and written out through `Fitted` carries a threshold chosen against
the fabricated one, and should be re-fitted rather than loaded.

Gate: 2218 tests, 0 failures, 0 warnings, on a quiet box. An earlier
run died at 1456 with SIGTERM 143 — the okay-cluster hazard, still
open, still not anyone's branch.

## intent-label-distillation — a supplement for the no-network tiers, 60.0% → 66.7%
Completed: 2026-09-04
Landed as 13b1d668. Scoped by this programme's own learning curve: NOT
for the probe, which is flat past 32 examples, but for the tiers still
climbing when the fixture ran out — the ones that need no network at
all.

The model is used ONCE, offline, in two passes: it writes messages for
a class, a second pass classifies them back with the shipped prompt,
and only agreements survive. Evaluation never touches generated data —
the held-out half of the human fixture is the only thing scored.

| trained on | rows | accuracy on held-out HUMAN data |
|---|---|---|
| the fixture alone | 60 | 60.0% |
| the distilled corpus alone | 182 | 50.0% |
| both | 242 | **66.7%** |

DISTILLATION IS A SUPPLEMENT, NOT A SUBSTITUTE. Trained only on what
the model wrote, chargrams score ten points BELOW a human fixture a
third the size: the model's own writing has a different distribution
from real messages, so it adds coverage rather than replacing evidence.
Together they beat either, and 66.7% is the best zero-network number
this programme has reached — above the static table's 63.3%.

THE FILTER'S OWN NUMBER IS THE STRIKING ONE: 182 of 320 survived, 57%.
The model disowns 43% of what it just wrote — asked to produce a
Proposal and then asked what that message is, it frequently says
something else. Two readings, not exclusive: the classes genuinely
overlap where the fixture said they do, and a model asked to WRITE is
doing a different task from one asked to JUDGE. Either way it is the
argument for having the filter, since without it 43% of the corpus
would carry labels its own labeller disowns.

Generation had to be made RESUMABLE, which is a lesson about the
harness rather than the method: thirty-two model calls do not fit in
one command's budget and the first version lost the whole corpus to a
cut-off run. Each batch is now written as it arrives, every run adds to
what the last left, and a time budget makes the exit clean rather than
a kill.

Gate: clean compile 0 warnings; full matrix 2216 tests, 0 failures.

## intent-taxonomy-and-language — one taxonomy both tiers read, and a fit that knows its languages
Completed: 2026-09-04
Landed as 0cf1f7c5. Requests 1 and 2 of the consumer's seven, taken
together because both are about what a fit KNOWS.

(1) ONE TAXONOMY VALUE, TWO DOORS. The model tier took its classes from
`Schema[I]`, a fitted tier inferred them from whatever labels its rows
carried, and nothing connected the two. `Taxon` is a value with `of[I]`
reading it from a `Schema` and `parsed` building it from strings.

The sharper half was about DATA: a taxonomy arriving as a corpus could
not reach the model tier at all, and a corpus is what
`intent-label-distillation` produces — able to define examples but
never a class. `Taxon` derives a `Schema`, so it round-trips and can be
edited without a compiler. `Taxon.check` refuses an unknown label
rather than letting a typo become a class that `Eval` would then score.

Named `Taxon` and not `Taxonomy`: the precedence lane shipped and
withdrew a `Taxonomy[I]` typeclass, and a name that means one thing in
the history and another in the code is worse than a slightly odd one.

(2) LANGUAGE AS A KEY IN THE FIT. A row was `(text, embedding, class)`,
so a multilingual corpus pooled every language into one boundary. `Row`
carries a `lang` and `ByLanguage.fit` groups by it, with a pooled
fallback for a language below `minRows` — defaulting to 32, where the
learning curve put the probe's stabilisation. An untagged corpus is
unchanged, since `Row.Any` is both "no language" and the pooled key.

THE MEASUREMENT IS DELIBERATELY NOT RUN: per-language fitting needs 32
rows and the parallel set holds 30 messages per language, so an arm
would train on fifteen — below where the probe means anything, which is
why the previous lane's per-language table was unreadable. The seam is
built and tested; the measurement waits on
`intent-language-fixture-growth`.

Gate: clean compile 0 warnings; full matrix 2212 tests, 0 failures.

## probe-ranked — the distribution the probe already computed, handed back
Completed: 2026-09-04
Landed as 2052e5a3. `Probe.score` reported `best`, `probability`,
`margin` and `runnerUp` from a softmax over every class, and then
dropped the softmax. `ranked` returns it — class and probability, in
descending order — and `score` is now a thin wrapper that reads the
head and the first gap, so the two cannot disagree about what the model
said.

Asked for by a consumer wiring the probe into a router: an operator
diagnostic that lists what the classifier CONSIDERED cannot be built
from a winner and a runner-up, and the alternative is re-implementing
the softmax outside, against `weights` — a private shape whose
normalisation would silently drift from this one.

IT WAS ALREADY BEING RE-IMPLEMENTED, AND WRONGLY. Landing this, I
found `NoModel.probabilityOf`: with the distribution unavailable, it
called `score` once per class and gave every non-winner the SAME
fabricated share, `(1 - p(best)) / (n - 1)`. Exact for two classes and
fiction for three or more — which makes the `ranked` list that
`intent-consumer-seams-a` added for active learning and for showing a
person the choice arbitrary below rank 1, makes `runnerUp` a coin
flip among ties, and lets a pattern cue promote the class the probe
liked LEAST past the one it actually ranked second. Reported to the
lane that holds the file rather than fixed here; the seam it needed
now exists.

The tests assert the classifier, not the arithmetic: that the ranking
covers every class, sums to one, is ordered, and that `score`'s verdict
is exactly its head and first gap. Four cases over hand-built corner
vectors, so they carry no encoder and are not Live.

## intent-model-persistence — a fitted model is data, so fitting leaves the startup path
Completed: 2026-09-04
Landed as c2fc1949. Request 4 of the consumer's seven, and the piece
that makes "no generation on the request path" also mean NO FITTING on
it. Without a codec, fitting lives wherever loading lives: every
process start re-fits, re-fitting needs the teacher, and an embedding
server is dragged into the STARTUP path of a service whose request path
was carefully kept clean.

`Fitted` gives `Probe.Trained`, `Centroid.Trained`, `CharGrams.Trained`
and `Static.Table` a record apiece with a derived `Schema`, so a model
is fitted at build time and loaded at boot.

THE SCHEMAS ARE HAND-BUILT, AND THE REASON IS SMALLER THAN THE FIRST
COMMENT CLAIMED. Weights are `Array[Double]`, vectors are
`ArraySeq[Float]`, and a derivation sends each as a JSON array of
numbers — which is how an embedding once travelled as `List[Double]`.
Numbers ride as bytes instead. Measured: a two-class probe over 1024
dimensions is 21KB as bytes against 36KB as decimal literals — 1.7x,
not the order of magnitude I had written, because base64 hands back a
third of what binary saves. The comment now carries the measured figure
and the reason that survives it: no boxing on the way through, and a
matrix that carries its width so a reader can check it, rather than a
nested list whose rows might disagree.

WHAT THE TESTS ASSERT IS THE CLASSIFIER, NOT THE BYTES. Round-tripping
fields is the easy half; a caller needs the loaded model to ANSWER what
the fitted one answered. Every case compares predictions across the
trip, the probe's compares probabilities to 1e-12 — identical rather
than merely agreeing — and a ScalaCheck property does the same over
random fits.

One thing a table cannot carry is its splitter: `Static.Table` holds a
`String => Vector[String]`, and a function is not data. `load` takes it
back as an argument rather than defaulting, because handing
`Static.tokens` to a table distilled over `Static.units` is a silent
accuracy loss — pairs stop being looked up and nothing errors.

Gate: clean compile 0 warnings; full matrix 2205 tests, 0 failures.

## intent-consumer-seams-a — name the dependency, hand back the abstention's ranking, and one flake out of the gate
Completed: 2026-09-04
Landed as 8fe8e809. Two of the seven requests a consumer wrote into
this spec (0fc7386b), taken first because one of them was misleading
readers today — plus an operator call that arrived mid-lane.

Recorded because it is the part worth not repeating: I rebased over
that consumer's commit SIX TIMES before reading it, looking only at my
own Results sections rather than at the file, and one of its notes —
that the language key was worth doing BEFORE the embedding bake-off —
was advice I had already walked past by the time I read it.

(6) THE DEPENDENCY IS `String => Embedding`, NOT "A SERVER". The
bake-off tables said "needs a server", which describes the reader's
deployment rather than the tier, and it argued for the wrong one: where
the encoder is in process, the probe at 86.7% is the CHEAPEST row on
the table rather than the dearest. The column is now `dependency`, the
vector tiers name the function they require, and the 12ms figures are
labelled as this machine's HTTP round trip rather than a property of
the method.

(3) AN ABSTENTION HANDS BACK WHAT IT COULD NOT SEPARATE.
`NoModel.Verdict` kept `best` and dropped the runner-up and ranking
that `Probe.Verdict` had already computed, so declining told a caller
only THAT it declined. `Verdict` now carries `runnerUp` and the full
`ranked` list, and `NoModel.decide` returns the answer (or `None`)
together with the verdict it CONSIDERED, from one call so the two
cannot disagree. Wanted by an interface that shows a person the choice
it could not make, and by active learning, which ranks on uncertainty
rather than on the winner.

AND A THIRD KIND OF INTEGRATION TEST. A full matrix failed on exactly
one test — `okay.script.TestScalaScript`'s "leaves no temp file behind"
— which snapshots every `okay-script-*` path under the system temp
directory before and after, and so fails whenever a sibling suite in
the same module creates one in between. The diff named two files the
test never touched; alone the suite passes 9/9. On the operator's call
it is now `Live`-tagged, AT THE TEST rather than at the suite, since
the other eight are deterministic and belong in the gate.
`specs/integration-test-gate.md` gains it as a third kind: not a test
that reaches outside the JVM, nor one that depends on a live model, but
one whose OBSERVATION is shared. The real fix — scoping the snapshot to
the paths the test itself creates — stays owed by whoever owns
okay-script (`script-temp-snapshot-crosstalk`).

Gate: clean compile 0 warnings; full matrix 2198 tests, 0 failures.

## intent-second-embedder — the bigger vectoriser does not lift the ceiling, and the first reading of why was wrong
Completed: 2026-09-04
Landed as ada1b28f. The experiment `intent-embedding-choice` was
blocked on: `Qwen3-Embedding-4B` is now served alongside the 0.6B —
2560 dimensions against 1024, genuinely different vectors — so the
vectoriser could finally be the only thing that changes.

| model | framing | probe | centroid |
|---|---|---|---|
| 0.6B | bare | 86.7% | 80.0% |
| 0.6B | classify instruction | **88.3%** | **83.3%** |
| 4B | bare | 76.7% | 76.7% |
| 4B | classify instruction | 85.0% | 80.0% |

BIGGER IS NOT BETTER HERE, AND THE FIRST READING OF THAT WAS WRONG.
Bare, the 4B scores ten points below the 0.6B, which reads as a verdict
on the model and is not one: Qwen3-Embedding is instruction-tuned, and
the larger model is far more sensitive to being told what the vector is
for — the classify instruction is worth +8.3 to it against +1.6 to the
small one. Framed properly it reaches 85.0% and still does not beat the
framed 0.6B.

THE MECHANISM IS THE LEARNING CURVE'S. At 2560 dimensions the probe
fits two and a half times as many weights on the same sixty examples,
in a regime that curve already showed to be data-bound. A richer
representation is a LIABILITY in small data — the opposite of the
intuition that sent me looking for a bigger embedder — and it costs six
times the wall clock (2000ms against 345ms for 120 messages).

So 88.3% is this TASK at this data size, not this vectoriser: two
independent embedders, one four times the size, land within three
points of each other, while the model tier reaches ~90% and shares none
of the probe's errors.

The per-language table is under-powered and explicitly not interpreted
— fifteen training examples per arm against the thirty-two where the
probe stabilises, with numbers swinging 46.7% to 86.7%. Filed:
`intent-language-fixture-growth`, and `intent-4b-with-more-data` (the
4B's disadvantage is a prediction about small data, so find where the
two curves cross).

Gate: clean compile 0 warnings; full matrix 2197 tests, 0 failures.

## intent-static-embeddings — no gateway at request time, 63.3% once pairs are in the table
Completed: 2026-09-04
Landed as 64910bc9. A classifier with no external server when a message
arrives. Rather than downloading `model2vec`, this does what model2vec
DOES: distils a static table from the teacher already in use — embed
each unit once, offline, then tokenize, look up, pool. Array arithmetic
at request time, so it crosses to JS and Native where a native runtime
could not follow, and no foreign tokenizer has to be matched.

| table | units | sees of an unseen message | probe | centroid |
|---|---|---|---|---|
| words, training half only | 301 | 66.0% | 43.3% | 41.7% |
| words, full dictionary | 1019 | 100.0% | 51.7% | 43.3% |
| words + adjacent pairs | 1303 | — | **63.3%** | 58.3% |
| (teacher, live vectors) | — | — | 86.7% | 80.0% |

VOCABULARY WAS PART OF IT AND NOT MOST OF IT: complete coverage bought
8.4 points and left the method at 51.7%, below even chargrams, so the
dictionary was not the limit.

THE LIMIT WAS THE BAG OF WORDS, and this line has met it before. A
word-only table cannot tell "could you" from "we could" — one requests,
the other proposes, and a bag holds the same tokens either way. That is
exactly the mechanism that sank the BM25 tier, arriving a second time
by a different road. Adjacent PAIRS are a unit the teacher embeds like
any other, and they are worth 11.6 points to the probe and 15.0 to the
centroid.

Where that leaves the zero-infrastructure goal:

| option | accuracy | needs |
|---|---|---|
| patterns | 51.7% (89% where a cue fires) | nothing |
| chargrams | 60.0% | nothing |
| static, words + pairs | 63.3% | a 5MB table |
| teacher | 86.7% | an embedding server |

So no external gateway is reachable at 63%, and the remaining 23 points
are CONTEXT: a static table gives a unit the same vector wherever it
appears, and representing a word differently in two sentences is most
of what a transformer is for. A property of the method, not of this
implementation.

Filed: adjacent triples, and `model2vec`'s PCA step — 1303 units is
already 5.2MB at float32 and a 30k vocabulary would be 120MB.

Gate: clean compile 0 warnings; full matrix 2197 tests, 0 failures.

## intent-embedding-choice — the ceiling is representational, framing moves it 6.6 points, and the swap is blocked on installation
Completed: 2026-09-04
Landed as aa6b0e10. Promoted ahead of distillation by the learning
curve, and half blocked by the machine: the central experiment needs a
second embedding model and exactly one is installed.

STATED CORRECTLY THIS TIME. `/v1/embeddings` VALIDATES the `model`
field and refuses a non-embedding id with HTTP 400; naming the
embedding model or omitting it returns byte-identical vectors, and the
response names what answered (`Qwen3-Embedding-0.6B-4bit-DWQ`, 1024
dimensions). An earlier note in this programme said the gateway
"ignores the model field", inferred from two requests that both
returned 1024 dimensions — two models can share a dimension, and the
vectors should have been compared rather than their shapes. Every
number in this spec was measured with that one model.

IS THE CEILING THE REPRESENTATION OR THE TASK? The learning curve ruled
out capacity; the recorded journal rules out the task, for free — it
already holds the model tier's answer for every fixture message.

| | wrong of 60 |
|---|---|
| model tier | 4 |
| probe | 8 |
| both | **0** |

Not one shared mistake. Inherently ambiguous messages would trip both;
instead each has its own blind spots, so the signal the probe misses IS
in the text and its representation is losing it.

FRAMING MOVES THE SAME MODEL 6.6 POINTS. One model, four ways of asking:

| framing | probe | centroid |
|---|---|---|
| bare message | 86.7% | 80.0% |
| "Classify the intent of this message: " | 88.3% | 83.3% |
| long e5-style task instruction | 81.7% | 65.0% |
| "Represent this message for intent classification: " | 81.7% | 78.3% |

The gain over bare text is at the noise floor on sixty messages; the
SPREAD is not, and both models move together. The rule this line has
found everywhere else holds here too — a short instruction helps, a
long one costs.

Concatenating chargrams with the embedding did not help (86.7% alone,
85.0% together), but gluing a weak signal to a strong one is a poor
test of orthogonality and refutes little.

WHAT REMAINS IS INSTALLATION, NOT CODE. Filed: `intent-second-embedder`
(Qwen3-Embedding-4B/8B as the same-family swap, BGE-M3 and
multilingual-e5-large for the Russian arm, jina-embeddings-v3 for its
classification adapter), `intent-static-embeddings` (model2vec/potion —
a transformer distilled into a lookup table, no neural inference at
request time, the only candidate offering embedding-grade accuracy with
chargrams' zero-network property), and `intent-instruction-prefix`
(re-measure the +1.6/+3.3 on the grown fixture before defaulting it).

Gate: clean compile 0 warnings; full matrix 2197 tests, 0 failures.

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
