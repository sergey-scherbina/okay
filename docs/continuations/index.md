# Continuations: a working book

**The plan, the theses, and the reading order.** All 28 chapters are
written; this page is the contract each of them was written against,
and the place to start. Every runnable snippet in the book compiles —
the suites are named `TestBook*`.

---

## Who this is for, and what it owes them

Three readers, and the book fails if it serves only one.

| reader | what they need by the end |
|---|---|
| **the engineer** who has never used a continuation | to recognise the four shapes in their own code, and to write one correctly the first time |
| **the engineer who has** | the limits, the costs with numbers, and how to build new effects rather than new special cases |
| **the manager or architect** | what capability this buys, what it replaces, what it costs to adopt, and when the answer is "don't" |

The manager's chapters are not a summary of the engineer's. They ask a
different question — *what does the team stop building* — and they are
answered with the same evidence.

## What this book is NOT

- **Not a tutorial for `Cont`.** The type is chapter 10's business.
  Parts I and II are about problems and shapes, and a reader who stops
  after them has still gained something usable.
- **Not a survey.** Scheme's `call/cc`, Haskell's `ContT` and Racket's
  prompts appear where they explain a decision, not for completeness.
- **Not marketing.** Every number in it is measured on this
  repository's benchmarks, and every "you cannot do X" is a test that
  fails when the claim stops being true.

## The rule this book is written under

**Code that appears in a chapter compiles.** The same rule the rest of
these docs live by (`TestWorkflowGuide` compiles the workflow guide's
snippets) applies here: each chapter's runnable examples live in a
suite named for it — `TestBookLeaveEarly` for chapter 5, and so on —
in the tests of whichever module the chapter is about, so a page
cannot drift from the library without a gate going red. (An earlier
draft of this page said the suites live under `okay-docs`. They do
not: that module is a document STORE, and the name misled its own
author.) Where a snippet is deliberately illustrative
rather than runnable — pseudocode for a competitor's API, a shape that
does not compile ON PURPOSE — it is marked as such in the text.

---

# Part I · The case

*Why anybody should care. Readable by somebody who will never write
Scala.*

### [1. Five programs that are hard to write](01-four-programs.md) ✓

**Thesis.** There is a family of ordinary requirements — not exotic
ones — whose straight-line form the language refuses, and every team
meets at least one of them. Name them before naming any solution.

The five: leave a deep computation early with an answer; consume a
producer that insists on pushing; stop in the middle and continue
tomorrow, in another process; run something on the way back out; and
let somebody else decide, deep inside a computation that must not be
unwound while they think. Each shown as the code a reasonable person
writes first, and why it does not work — and, inside the third, the
question everybody asks next: *can't we just save it?*

### [2. What teams build instead, and what it costs](02-what-teams-build-instead.md) ✓

**Thesis.** Every one of the four already has a standard workaround,
and each workaround has a characteristic bug that the workaround
itself cannot rule out. That, not elegance, is the argument.

- the hand-written state machine → the transition nobody wrote
- `Option`/`Either` threading → the layer that forgets to propagate
- the callback tree → the error path that is only on one branch
- a workflow engine as a service → the operational surface, and
  the fold that must be kept in step with the code by hand

Accounting per workaround: what it adds to a diff, what it adds to
onboarding, and which bug class it leaves open. This is the manager's
chapter.

### [3. A continuation, in one page](03-a-continuation-in-one-page.md) ✓

**Thesis.** "The rest of the program, as a value you can hold" is the
whole idea, and it can be taught with one picture and no theory.
Delimited vs undelimited belongs here only as "how much of the rest".

### [4. Deciding: reach for it, or don't](04-deciding.md) ✓

**Thesis.** The honest default is DON'T, and a book that cannot say
when its subject is the wrong tool is advertising. A checklist, the
three smells that mean "an effect would be simpler", and the two
questions that settle it.

---

# Part II · The four shapes

*Recipes. A reader should be able to copy one and adapt it.*

### [5. Leave early with an answer](05-leave-early.md) ✓

**Thesis.** `exit` replaces a sentinel return threaded through every
layer; the win is measured in the layers that stop mentioning the
failure at all.

### [6. A push producer, read as a pull](06-push-as-pull.md) ✓

**Thesis.** A generator is a prompt and a capture. This is where
"inversion of control" stops being a phrase and becomes four lines.

### [7. Stop in the middle, carry on later](07-stop-in-the-middle.md) ✓

**Thesis.** The pause is the shape with the largest consequences: it
is what makes a program survive its process, and Part VI's engine is
built on nothing else.

### [8. Do something on the way back](08-on-the-way-back.md) ✓

**Thesis.** The smallest shape, and the one most often written by hand
as a `finally` that is subtly in the wrong place.

### [9. Composing the shapes](09-composing.md) ✓

**Thesis.** The four do NOT compose naively, and this repository
learned it the hard way: the halves that nest are named
(`scope`/`collecting`/`pausing`), and the wrong spelling is a compile
error rather than a runtime surprise. The chapter shows the failure
first and the fix second.

---

# Part III · The machine

*What is actually happening. For the reader who wants to build, not
only to use.*

### [10. Prompts, and why they are first class](10-prompts.md) ✓

**Thesis.** A typed, first-class prompt is what lets an inner scope
abort ACROSS its own boundary to an outer one — the thing nested
handlers cannot express — and it is why this library has `Delim` at
all.

### [11. Four captures: `shift`, `shift0`, `control`, `control0`](11-four-captures.md) ✓

**Thesis.** They differ in what they do to the delimiter, and the
difference is visible in three-line programs. A table nobody has to
memorise, plus which one every recipe in Part II actually uses.

### [12. One machine, one prompt stack](12-one-machine.md) ✓

**Thesis.** A prompt lives in the machine that pushed it, so a second
machine in one row is always a mistake — and here it is a COMPILE
error. The chapter explains the rule by showing what went wrong before
the guard existed.

### [13. Multi-shot: a continuation is a value](13-multi-shot.md) ✓

**Thesis.** Resuming twice is not a curiosity: it is what makes a
debugger able to ask "what if the tool had answered differently", and
this repository ships one that does. What it costs, and what it does
to state and resources, is chapter 18.

---

# Part IV · Building with them

### [14. A new effect from a prompt](14-a-new-effect.md) ✓

**Thesis.** The library's own effects are not privileged. A generator,
a validating cut, a cancellable scope — each is a prompt plus a
capture, written in user code, and shown here end to end.

### [15. Resumable exceptions: signalling instead of unwinding](15-resumable-exceptions.md) ✓

**Thesis.** The oldest and most convincing application, and the one
that shows most clearly what "the rest of the program as a value"
buys: an error mechanism in which the handler runs **while the failing
computation is still alive** and may put it back to work.

An ordinary exception UNWINDS. By the time a `catch` runs, the frame
that failed is gone, along with every local it had — so the only
question the handler can answer is "what shall we do instead of the
whole thing". A condition SIGNALS: the handler runs on top of a
computation that is still standing, and can choose to **resume** it,
supplying a value, skipping an item, or substituting a default.

That difference is exactly the continuation. Resuming means returning
control into the middle of a computation, which is only possible if
the middle is a value somebody is holding.

**What the chapter shows.** A parser meeting a malformed record that
does not decide policy: it signals, offering named restarts (`skip`,
`useDefault`, `abort`), and the CALLER — who knows whether this is a
nightly import or an interactive session — picks. Neither side learns
the other's vocabulary: the parser has no `strict: Boolean` parameter
and no error enum threaded through it, and the caller writes no
parsing.

**Why it matters beyond error handling.** It is the general shape of
"a decision that belongs to the caller, taken at a point deep inside
the callee" — logging policy, retry policy, missing-value policy. The
condition system is the case where that shape is most obviously
needed, not the only one where it fits.

**And the traps, which are real.** A restart is a protocol between two
pieces of code that never see each other, so a badly chosen set of
restarts is an untyped API in disguise; resuming into a frame that
holds a resource has all of chapter 19's consequences; and an
interactive restart chosen by a human is a pause (chapter 7) wearing a
different hat, not a third mechanism.

### [16. Continuations and monads](16-continuations-and-monads.md) ✓

**Thesis.** Filinski's result — given delimited control, ANY monad
runs in direct style — is the reason `!` exists, and it is the most
practically important theorem in this book. Shown, not cited.

### [16b. Two monads at once](16b-two-monads-at-once.md) ✓

**Thesis.** Monads do not compose by themselves: two of them need a
"swap" nobody wrote. One real program — a basket over several shops,
with errors and a log — written three ways: cats' transformers (the
composite is a type, every operation lifted to its position), layered
reflection with Biernacki's `$` and `shift0` (a delimiter per monad,
the order is the nesting of the blocks), and algebraic effects
(operations in a row, the order chosen by the handlers where it runs).
The last two are one mechanism.

### [17. In the effect system](17-in-the-effect-system.md) ✓

**Thesis.** Where a capture sits relative to a handler decides what it
can see. Rows, handler order, and the one question to ask when a
capture and an effect disagree.

### [18. What belongs in a library, what in an application](18-what-belongs-in-a-library.md) ✓

**Thesis.** The test is whether more than one interpreter exists.
Recipes that pass it become effects; the rest stay call sites.

---

# Part V · The limits

*The half that makes the rest trustworthy.*

### [19. What a capture does to everything else](19-what-a-capture-does.md) ✓

**Thesis.** State, resources, `try`/`finally`, exceptions and depth
each behave differently under a capture, and all of it is pinned by
tests that fail if the behaviour changes. Including the two that are
compile errors on purpose.

### [20. The costs, measured](20-the-costs-measured.md) ✓

**Thesis.** Numbers, from this repository's benchmarks: what a push
costs, what a capture costs, what a guard costs per operation (2.0–2.3x
on the work inside it), and what a label costs (8 bytes, no time). And
the trap: a claim with real numbers NEXT to it that measure a
different shape.

### [21. The disciplines that make it safe](21-the-disciplines.md) ✓

**Thesis.** Three constraints carry their weight in types rather than
in prose — `Replayable`, `At`, `OneMachine` — and each exists because
something went wrong without it.

### [22. Saving and restoring: checkpoints, and what cannot be one](22-checkpoints.md) ✓

**Thesis.** "Can I snapshot a paused program and restore it later?" is
the first question everybody asks, and the answer has a shape worth
learning: **no, and you do not need to.** The continuation is a
closure — it cannot be serialised, and any library claiming otherwise
is either restricting what you may write or lying about restarts.

What CAN be saved is everything the program was TOLD, and from that
its position is re-derived. The chapter lays out the four mechanisms
in this repository side by side, because they are routinely confused
and they cut different costs:

| | what is stored | what it cuts | durable |
|---|---|---|---|
| the journal | every answer | nothing — replay from the start | yes |
| chapters (`Snapshots`) | a journal PREFIX and its offset | **reading** | yes |
| `continueAs` | one seed that supersedes the history | **running** | yes |
| a resume cache | the live paused program | replay between calls | **no** |

The distinction that matters and is most often missed: **a snapshot
cuts reading, not running.** The program is still executed over every
answer in the prefix, because that is the only way to arrive at the
place it stands. Only `continueAs` shortens the execution, and it does
so by declaring the old history superseded.

Also: what makes a checkpoint SAFE — the answers must be the program's
only source of non-determinism, or a restore lands somewhere the
original run never was; why a checkpoint carries the program's NAME,
and what a reader does when it does not match; and the checkpoint that
is a trap — storing derived state beside the answers, so two sources
of truth exist and drift.

---

# Part VI · In production

*Real systems in this repository. Each chapter: the problem, the
shape used, what it replaced, what it cost, what went wrong.*

### [23. Durable workflows](23-durable-workflows.md) ✓

**Thesis.** A paused program is a closure and cannot be written down —
so nothing tries to. The answers are journalled and the place is
re-derived by replay, which is event sourcing whose fold IS the
program. The engine, its eleven lanes, and its honest limits.

### [24. A debugger for agents](24-a-debugger-for-agents.md) ✓

**Thesis.** Multi-shot pays for itself: fork an agent run at a tool
call, feed two answers, compare. Also the refuted expectation — this
does NOT come with durability, and the type says why.

### [25. Cutting a model mid-sentence](25-cutting-a-model.md) ✓

**Thesis.** A validator standing in a token stream, aborting across
the streaming boundary. And the measured cost that corrected the
comment which claimed the guard was free.

### [26. Cancellable flows in a UI](26-cancellable-flows-in-a-ui.md) ✓

**Thesis.** The smallest production use, and the clearest: no `Option`
threading on the steps between.

### [27. Everything that typically goes wrong](27-what-goes-wrong.md) ✓

**Thesis.** A catalogue, not a memoir. The mistakes available to
somebody using continuations — and to somebody implementing them — are
a small, recurring set, and naming them is worth more than any
particular incident. Each entry: what it looks like, why it is easy to
make, what it costs, and the cheapest way to rule it out.

Written as a checklist so it can be read at any point in the book, and
referenced from chapter 4 (deciding) and chapter 19 (the limits).

**A · Choosing wrongly.** Reaching for a capture where an ordinary
effect is simpler; using one to avoid learning a combinator that
already fits; adopting the mechanism for one call site.

**B · Boundaries and evidence.** Letting a prompt escape the
delimiter that created it and using it afterwards; two machines in one
program, so a prompt pushed in one is aborted from the other;
identifying prompts by name or string, so two boundaries collide;
letting an implicit search pick WHICH boundary you meant, silently;
catching at the nearest delimiter when you meant an outer one.

**C · Effects under a capture.** Expecting hand-written cleanup after
a capture point to run when the continuation is abandoned; expecting
`try`/`finally` to mean what it usually means; mixing a
bracket-shaped resource with a capture; assuming state is shared, or
assuming it is not — both are true, depending on which side of the
delimiter the handler sits; assuming exception handlers and captures
compose in an obvious order.

**D · Resumption.** Assuming one-shot when the continuation can be
resumed twice, so a side effect happens twice; assuming multi-shot
when the runtime gives you one; expecting a paused program to be
SERIALISABLE — it is a closure, and this misunderstanding is the
single most expensive one in the list; performing I/O, reading a
clock or a random number inside a program that will be replayed;
resuming a continuation that no longer matches the state it was
captured against.

**E · Implementing.** Reading an identity or index before an
operation and using it after the structure changed underneath;
enforcing a rule at the one call site where the symptom appeared while
several others implement the same rule; quadratic re-walking of a
captured segment; renumbering a persisted enum by inserting a case in
the middle; conflating "cannot proceed NOW" with "cannot proceed
EVER" in what you report to an operator.

**F · Claims.** Asserting a cost without measuring the shape the
claim is about — including the subtle form where real benchmarks sit
beside the claim and answer different questions; documenting a limit
that a later fix removed; leaving a promise in a doc that the code
stopped keeping.

---

---

# Part VII · Where it came from

### [28. A short history, and why each step was a narrowing](28-a-short-history.md) ✓

**Thesis.** The idea is sixty years old and almost everything
interesting about it is a story of **giving power back**. The
unrestricted version was discovered first, found unusable at scale,
and every step since has been a restriction that made it safer without
making it weaker for the jobs people actually have.

The line, told through what each step fixed rather than as a list of
names: the continuation as a semantic device for describing what a
program means; `call/cc` and the discovery that a capture with no
boundary takes hold of code it has never met; prompts and delimiters,
which made the captured piece finite and therefore typeable;
`shift`/`reset` and the family of four captures; multi-prompt control,
which made boundaries first-class so an inner one can be crossed to
reach an outer; algebraic effects and handlers, which is the same
power arriving from the other direction and meeting in the middle;
the Common Lisp condition system, which had the resumable-handler
shape decades before the theory tidied it; and one-shot continuations
in modern runtimes — Loom's virtual threads, OCaml 5's effects —
which is the restricted version again, chosen deliberately for the
same reason.

**Why the chapter is last and not first.** The history explains the
design decisions of Part III, and it is much more convincing after the
reader has met the problems. Read first, it is trivia; read here, it
is the reason the API looks the way it does.

With the papers, for whoever wants them.

---

# Appendix

### [A. If you really want to, you can](appendix-a-if-you-really-want.md) ✓

**Thesis.** Chapter 22 says a paused program cannot be written down and
chapter 23 builds an engine around not trying; readers do not accept
this, and they are right that "cannot" is too strong. Every route that
actually exists, with its bill:

- **the static one** — an applicative's shape is an index, so a cursor
  replaces a replay; the price is the language;
- **walking the tree** — you can, and the machine does, until the first
  `Bind`, past which walking *is* running;
- **serialising the JVM lambda** — Spark does it; it dies at a deploy;
- **defunctionalisation** — complete, and it turns your program into a
  DSL;
- **replay** — which *is* defunctionalisation with one coarse tag, the
  tag being the program's name;
- **TASTy for the code plus CBOR for the data** — the lambda route
  repaired: a designed format instead of `$anonfun$foo$1`, with both
  halves already in this tree. It fails for different reasons, and the
  deciding one is that it spends a compiler at restore to avoid
  replaying microseconds;
- **SKI combinators** — the most general possible defunctionalisation,
  three tags instead of *n*;
- **serialising the source** — real, and it means the source *plus its
  dependency closure plus the compiler*, which is a container image.

Also the correction that surprises people: **the opacity is not the
continuations.** `Segs` is already a walkable list of frames; the
closure is `Bind`'s `f`, which predates every capture in this library.

And the objection that reframes the whole appendix: **the point was
never speed.** Holding a program's state means the prefix never runs
again, so its side effects cannot repeat — a claim about the
programming model, since replay is safe here only because
`Replayable` forbids things. Followed properly, that is a
transactional design: the journal is a write-ahead log, replay is
recovery, chapters are checkpoints, and a database keeps both. So the
disagreement is one question — **what goes into the commit record, the
answer or the state?** — and `continueAs` is the engine already
choosing "the state", once.

The unifying claim: a position is only meaningful relative to code, so
you either freeze the code, enumerate its shapes, or re-run it. The two
radical proposals land in the first two columns rather than outside
them, which is what makes the trichotomy exhaustive rather than merely
convenient.

## Reading orders

| you are | read |
|---|---|
| deciding whether this is worth the team's time | 1, 2, 4, then 23 |
| about to write your first one | 3, then Part II, then 19 |
| building a library on top | Part III, Part IV, 21 |
| reviewing somebody's use of it | 4, 9, 19, 20 |
| curious about the failure modes | 27 |
| wondering why the API looks like this | 28, then Part III |

## A note for whoever edits this

Chapters are referred to BY NUMBER in the prose, which makes inserting
one a rename across files. Two insertions have already happened and
both broke references in chapters that were already written. The sweep
is mechanical, so do it rather than trusting memory:

```
grep -rn "chapter [0-9]\+" docs/continuations/*.md
```

Read every hit and decide what it MEANT — twice now a reference has
become more accurate after a renumbering, because the chapter it
should have pointed at did not exist when it was written.

## It repeats itself on purpose

**This book is self-contained.** A reader who opens it at chapter 1
and reads forward needs nothing else open — so when a later chapter
needs the rule from an earlier one, it says the rule again rather than
sending anybody back. Definitions are restated where they are used,
and the important claims appear more than once, in different words,
because that is how a thing is learned rather than met.

The cost of that is repetition, and it is paid deliberately. The
alternative — a page of cross-references — is cheaper to write and
worse to read.

Other pages on the same subject exist and are worth reading, but none
of them is required by this one:

- [Continuations in practice](../continuations-in-practice.md) — the
  four shapes compressed into a single page.
- [Durable workflows](../durable-workflows.md) — the engine's guide,
  with the operational detail this book summarises.
- [theory/2 · Continuations and delimited control](../theory/02-continuations.md)
  — the textbook treatment, with the papers.

This book is the road between them: longer than the page, gentler than
the textbook, and answerable to a manager at the start.
