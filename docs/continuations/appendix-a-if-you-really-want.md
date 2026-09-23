# Appendix A · If you really want to, you can

> **After the last chapter, because it is the question that comes
> after the last chapter.** Chapter 22 says a paused program cannot be
> written down, and chapter 23 builds an engine around not trying.
> Readers do not accept this, and they are right not to: "cannot" is
> too strong. This appendix is the full answer — every route that
> actually exists, and what each one charges.
>
> The short version: **you can, three ways, and each one takes
> something away that you probably wanted to keep.**

---

## The static route: never be a monad in the first place

The first route, and the one chapter 22 raises: if a monadic program
cannot be written down, what about an applicative one?

The reason a monadic program cannot be written down is one line of
`Free`:

```scala
case Bind[F[+_], A, B](a: Free[F, A],
                       f: A => Free[F, B]) extends Free[F, B]
```

`f` is a host closure. What the program does after its first operation
cannot be read without running it. That opacity is the price of
`flatMap`, and most programs are worth it.

An applicative has no such function. Its structure is fixed before any
value exists, which is why `Static` — the free applicative with
`select` on top — can offer something `Free` cannot:

```scala
def leaves: Vector[F[Any]]    // every operation the program MAY perform,
                              // before it runs
```

Those leaves are operations **as values**. They serialise.

### So: run it as a monad, save it as an applicative?

No, and the reason is one sentence in `Static.scala`:

> The bridge is one-way and cheap: `toFree`.

You can turn a static program into a monadic one in order to run it.
You cannot recover a static program from a monadic one, because
recovering it would mean reading the closure. The moment a `flatMap` is
written, the shape is gone — not hidden, **gone**, in the sense that no
amount of inspection recovers it.

The direction that works is the other one: **start static**.

### What starting static would buy

Everything about checkpointing changes if the structure is known in
advance:

> If a program's shape is static, "where it stands" is an **index**,
> not a closure.

A checkpoint becomes a cursor plus the answers, and a restore is O(1) —
**no replay at all**. That is not a hypothetical design; it is exactly
why BPMN engines, step functions and workflow DSLs look the way they
do. A static graph with a cursor is checkpointable for free, and the
whole family of tools arrived at that shape because it is the only one
that does not have to re-run history.

### What it costs — and this section was REFUTED the next day

> **Read the correction below before the argument.** What follows was
> right about `Selective` and wrong about the conclusion it drew, and
> the refutation is in this repository: `okay.Proc`, the free ARROW,
> which does exactly what this section says cannot be done.

The original argument. No data-dependent structure: `Select` buys a
conditional with **both sides written down**, and the source names the
approximation honestly:

> `leaves` reports both sides of every `Select`, because which side
> runs is decided by a value that does not exist yet. It is an upper
> bound.

An upper bound is what a batcher, a capability list and a dry run all
want. It is not what a workflow wants. You cannot write

```scala
val nights = !w.pause("how many nights?")
for _ <- 1 to nights do !w.pause("which room?")
```

because the *shape* now depends on an answer.

### The refutation: the objection was about `Selective`, not about static shapes

That paragraph is true of the applicative and selective rungs and of
nothing else. `whileS` — the selective literature's loop — is a
**recursive definition**, and in a strict free structure a recursive
definition is an infinite term. That is the whole of why the shape
could not depend on an answer.

**Elgot iteration is a NODE, not a definition:**

```scala
case Iter(body: Proc[F, X, Either[X, Y]])   // Left goes round, Right leaves
```

so the term stays finite and the trip count is a value. The loop above
is written today, in a block, and compiles to it:

```scala
val rooms = Proc.direct: _ =>
  val n = !ask("nights?")
  var got = List.empty[String]
  while got.length < n.toInt do
    got = got :+ !ask(s"room ${got.length + 1}?")
  got
```

`leaves` reports two questions — the body is counted once, because how
often it runs is decided by a value that does not exist yet, which is
the same honest upper bound the section above describes and not an
obstacle to having the loop at all.

So the trade is real but the third column was wrong, and the corrected
table has a row this appendix did not imagine:

| | position | restore | language |
|---|---|---|---|
| monadic | a closure — cannot be named | replay, O(answers) | anything |
| applicative / selective | an index into a known shape | a cursor, O(1) | no data-dependent structure |
| **arrow (`Proc`)** | **a PATH into a finite term, with a counter per loop** | **replay, and `walk` re-derives the position with no runtime** | **anything short of `ArrowApply`** |

What the arrow rung still cannot do is the thing Hughes proved it
cannot: run a step chosen by a value the program binds. That is `app`,
and an arrow with `app` is a monad — so "the shape may depend on an
answer" is true for branching and iteration and false for *which
program to run next*, which is a much smaller loss than this section
claimed.

This engine's DEFAULT is still the left column, because the code
between pauses is straight-line and replaying it is cheap — chapter 22
measured it: 40 answers, 40 steps. What changed is that the right-hand
column is now available for the programs whose shape really is fixed,
and what it buys is in
[docs/static-workflows.md](../static-workflows.md): the questions known
before the run, a deploy check that asks live journals whether they
still fit the new code, and a position that can be drawn.

**Why the mistake is worth leaving in place rather than editing out.**
The argument was sound and the conclusion was too wide, and it was too
wide because it reasoned from ONE rung of the ladder to every rung
below the monad. That is the most ordinary way to be wrong about a
design, and the fix was not cleverness — it was noticing that the
literature's `whileS` and an iteration NODE are different things.

### The caveat that survives either choice

An applicative does not make the *program* serialisable either.
`Return(a)` may hold any value, functions included, and the spine of
combining lambdas is host code whichever functor you chose. What
serialises is the **leaves**, not the spine.

The code still has to be on disk, and it is still addressed by name —
which is what `"booking/1"` in the journal has been doing all along.
That is the deep version of chapter 22's first answer:

> Nobody serialises the code. Not for monads, not for applicatives.
> The difference is whether you can name your **position** in it
> without running it.

### And the hybrid that is already here

`continueAs` is the third answer, and it sits between the two columns:
it does not make the past inspectable, it declares it **superseded**. A
seed replaces the history, so the next replay starts from a value
rather than from an epoch. Not a cursor, not a full replay — a way to
stop paying for a history that no longer describes anything.

## But can we not just walk the tree?

The next question, always, and it deserves a long answer because the
short one ("no") is false.

**Yes, you can walk it, and the machine already does.** `Free` has four
cases:

```scala
case Pure(a: A)                                   // a value
case Inject(a: F[A])                              // one operation
case Bind[F[+_], A, B](a: Free[F, A],
                       f: A => Free[F, B])        // ...and a function
case Delay(thunk: () => Free[F, A])               // ...and a thunk
```

`Pure` and `Inject` are data: an operation is an enum case and it
serialises. `Bind`'s **left** side is a program and walks fine. What
stops the walk is `f`, and it stops it completely, for a reason worth
stating precisely:

> Past a `Bind`, walking **is** running. The only way to learn what `f`
> produces is to apply it to a value.

That is not an implementation limit. It is what a function *is*.

## The surprise: it is not the continuations

The reflex is to blame delimited control, and it is wrong. In this
implementation a captured continuation is **already a data structure**:

```scala
private enum Segs[F[+_], A, Z]:
  case Done[F[+_], Z]()
  case K[F[+_], X, Y, Z](f: X => Y ! (Delim + F), rest: Segs[F, Y, Z])
  case Mark[F[+_], X, Z](p: Prompt[X], rest: Segs[F, X, Z])
```

A list of frames. You can walk it, count the frames, and see which
prompts are installed — `Mark` is a delimiter, sitting in the
continuation as an ordinary node. A continuation here is **more**
inspectable than a native stack, not less, and that is exactly why it
is multi-shot (chapter 13).

The opacity is `K`'s `f`. An ordinary host function — the same one
`Bind` was already carrying before `Delim` existed.

> **The problem was never continuations. It is `flatMap`.** Captures
> only made it visible.

## Three ways to serialise a function

Three ways. All three are real, all three are in production somewhere,
and each pays a different price.

### (a) Serialise the JVM lambda

It works. **This is what Spark does** — it ships closures to executors
as serialised objects, and has for a decade.

*Costs:* the deserialising side needs the same class with the same
synthetic name (`$anonfun$foo$1`). Insert a line above it and the
numbering shifts; recompile with a different compiler and it moves
again.

So it survives **a network hop between identical binaries**, and dies
at **a deploy**. Which is precisely the axis a durable workflow exists
to survive. Spark's problem is space; a workflow's problem is time, and
the same mechanism answers one and not the other.

**But read that as a fact about Java serialisation, not about the
idea.** `$anonfun$foo$1` is a name the compiler invents; a designed
format would not have that defect. See *TASTy for the code, CBOR for
the data* below, which is this route repaired — and which fails for
entirely different reasons.

### (b) Defunctionalise

Reynolds, 1972, and it is the complete answer. Replace every function
with a **data tag plus its captured environment**, and write one
interpreter:

```
apply(tag, env, argument)
```

Every continuation becomes a case of a finite enum. Nothing is opaque,
everything serialises, a position is a tag and a restore is a lookup.

*Costs:* you must enumerate every continuation shape **in advance**. No
host lambdas — you are writing terms of a language you interpret, not
embedding Scala.

That is not a hypothetical either. **BPMN, step functions and every
workflow DSL are defunctionalised programs.** The boxes in the diagram
are the tags, and the engine is `apply`. They can checkpoint without
replay because they gave up being a programming language.

### (c) Name the code, journal the answers

What this engine does — and here is the reframing worth carrying away:

> **Replay is defunctionalisation with one coarse tag.**

The tag is `"booking/1"`. The environment is the journal. The
interpreter is the JVM, re-running the program. We did not escape
defunctionalisation by being clever; we chose a granularity of **one
tag per program** instead of one per continuation, and paid for the
coarseness by re-running the spine.

Seen that way, the three options stop looking like different ideas.

## The trade, in one table

| | what is fixed | what is stored | survives a deploy |
|---|---|---|---|
| serialise the lambda (Spark) | **the binary** | the closure | no |
| defunctionalise (BPMN) | **the shapes** | a tag + environment | yes |
| replay (here) | **nothing** | the answers | yes |

And the claim that unifies them:

> A position is only meaningful **relative to code**. So you either
> freeze the code, enumerate its shapes, or re-run it. There is no
> fourth option — not for engineering reasons, but because a position
> with nothing to be a position *in* does not mean anything.

Worth knowing: **Temporal replays too.** The reference implementation
of this whole category does not serialise stacks either. When a
mechanism that looks like an obvious win is missing from every mature
system in a category, that is usually evidence rather than an
opportunity.

---

## TASTy for the code, CBOR for the data

The strongest form of the whole question, and it repairs the exact
defect that the lambda route was dismissed for above. That dismissal
needs narrowing, and this section is where it happens.

A closure is **code plus environment**. Give each half a stable format:

| half | format | why it is the right one |
|---|---|---|
| the code | **TASTy** | Scala 3's typed AST, versioned, emitted beside every classfile |
| the environment | **CBOR** | ordinary data, with a `Schema` to say its shape |

Both exist in this repository — `okay-codec/Cbor.scala`, and
`okay-staging/RuntimeStaged.scala`, which already runs
`scala.quoted.staging.run` in production with the compiler dependency
isolated in its own module and a documented way to switch it off.

### The correction this forces

The lambda-serialisation route was dismissed above because "the
deserialising side needs the same class with the same synthetic name
(`$anonfun$foo$1`)". That is true of **Java serialisation of a JVM
lambda**. It is not true of the idea.

`$anonfun$foo$1` is a name the compiler invents and nobody controls.
TASTy is a designed, versioned interchange format. So the objection
shrinks from

> it dies at a deploy

to

> it is **forward-** but not **backward-**compatible

which is a real constraint with a real window, not a wall. A claim
about a route should not rest on the weakest implementation of it.

### What it does not repair

**Getting the term is the hard part, not storing it.** TASTy is emitted
for *definitions*, at compile time. There is no TASTy for a closure
*instance* at run time, so you cannot take an existing captured
continuation and ask for its tree. The capture site would have to be a
macro that reifies `'{ ... }`, which means the program must be authored
so that the term exists at all. That is a research-grade project, not
an afternoon.

**Restore costs a compiler.** `okay-staging` is the evidence that this
is workable and also the price list: a module that carries the
compiler, off by default, falling back to an interpreter when
generation fails. Seconds per restore, and a compiler on the
production classpath.

**Symbols still resolve against a classpath.** The term refers to
`okay.Delim.pause` and to your own definitions by name. It is not a
self-contained blob — though note that **replay is in exactly the same
position**, so this is not a cost *relative to replay*. It is a cost
relative to the fantasy of a program in a bottle.

**The journal becomes executable.** Unchanged, and still the objection
that ends the conversation in most organisations.

**The code is pinned.** Same inversion as everywhere else in this
appendix: an old run keeps its old term, and patching becomes a term
migration.

### The argument that actually decides it

What does this buy over replay? Precisely one thing: **you do not
re-run the prefix.**

Chapter 22 measured that prefix. Forty answers, forty program steps,
microseconds. So the trade is:

> pay a **compiler at restore** (seconds) to avoid **replaying a
> straight-line prefix** (microseconds).

The benefit is inverted for the ordinary case — **if restoring faster
is what you wanted.** It usually is not, and *"The point is not speed"*
below is the correction: the reason to hold a program's state is that
the prefix then never runs again, so its side effects cannot repeat,
which is a claim about the programming model rather than about the
clock. Read that section before taking this paragraph as the verdict.
It becomes attractive only when replay is genuinely expensive: a
history long enough to matter, or steps that are costly to re-derive.
And for that case the tree already has `continueAs`, which collapses
the history into a seed at no cost at all.

### Where this mechanism does pay: a cluster

The inversion above is about *durable workflows*. Move the same
mechanism to **distributed execution** and every term of it flips.

Shipping a computation to a hundred workers is the case where a
serialisable closure is not a curiosity but the entire product. Spark
exists to do it. And each objection this section raised turns into its
opposite:

| | in a workflow | in a cluster |
|---|---|---|
| a compiler at restore | seconds, to save microseconds | **amortised** — compile once per stage, run over millions of rows |
| symbols need a classpath | a real coupling | **free** — every worker runs the same artifact by construction |
| an executable payload | a persisted journal, so RCE | an authenticated internal control plane — a different threat model |
| the code is pinned | you cannot patch a running instance | **a feature** — version skew inside one job is a bug |

And what it buys is no longer "skip a microsecond prefix"; it is
**arbitrary code running in parallel**, which is the whole point.

This repository's own dataflow engine states the trade in the open.
Its Claim 3 is *nothing ships a closure* — a task is a registered
**name** plus `Schema`-carried parameters, which is why `Task not
serializable` cannot happen here — and it names the price: *you cannot
type a lambda into a REPL and have it run on the cluster.*

TASTy-plus-CBOR is how that price could be lifted **without** losing
what Claim 3 bought, because a typed AST plus CBOR data is not a
serialised JVM lambda: there is still no Kryo, no registration list,
and no synthetic name to go stale. The Spark capability without the
Spark mechanism. It is filed as `shipped-terms` in BACKLOG.md, with
the one genuinely hard part — a macro reifying `'{ ... }` at the
submission site — and the deciding test stated before anything is
built.

The general lesson is worth more than the proposal: **the same
mechanism is a bad trade and a good one depending on what it is asked
to survive.** For a workflow, the enemy is a deploy six months from
now. For a cluster, the enemy is a network hop this second. Code as
data loses the first fight and wins the second, and a book that
recommended it in both places would be wrong in one.

### When it is nevertheless the right answer

One case, and it is not about performance:

> **When the code must be pinned as an audit requirement.** "Prove that
> this run executed exactly this logic" is a question a journal of
> answers cannot answer, because the answers do not contain the
> program.

In a regulated setting that is not an optimisation, it is the
requirement, and then TASTy-plus-CBOR is the right shape and the
seconds-per-restore do not matter. Note what changed: the reason to
store the term is no longer *to restore faster*. It is *to be able to
say what ran*. Those are different features, and only the second one
justifies the machinery.

## And what about SKI combinators?

The best version of the question, because bracket abstraction is the
classical answer to exactly this: **every λ-term can be translated into
a tree over `S`, `K` and `I` with no variables left in it**, and a tree
of three atoms is obviously data.

This library already contains the identity, in the theory chapter. The
applicative of functions from an environment *is* the combinator basis:

```scala
pure(a)      =  _ => a                    //  K
f.app(x)     =  e => f(e)(x(e))           //  S
identity     =  e => e                    //  I
```

`Reader.ask` is `I`, `pure` is `K`, `app` is `S`. So the algebra is
here. What it does not give is a way to serialise the closures already
in the tree — for four reasons, and the last one is the one that
decides it.

### One: bracket abstraction needs a term, and a closure is not one

The translation is a **source-to-source** transform. At run time a
Scala lambda is a JVM object with an `apply` method: bytecode, with its
free variables already captured in fields. There is no term to abstract
over, so there is nothing to feed the algorithm.

You cannot *convert* an existing closure. You would have to write the
program in a term representation from the beginning — which means:

> **SKI is not a fourth route. It is defunctionalisation, in its most
> general form.** Instead of enumerating your program's continuation
> shapes as tags, you enumerate *three* combinators that can express
> every shape.

Maximally general, and correspondingly the furthest from the host
language.

### Two: the primitives stay opaque, and they are most of the program

`S`, `K` and `I` express pure λ-terms. A real program also contains
integer addition, string concatenation, an effect operation
(`Inject(fa)`), a clock read. Each becomes an extra **atom** in the
graph, and each atom needs a name that is stable across deploys.

Notice what happened to the problem rather than what solved it. The
synthetic-name problem from the lambda-serialisation route did not go
away; it **moved** — from `$anonfun$foo$1`, chosen by the compiler, to
a symbol table chosen by **you**.

That is a real improvement, and it is why the route is viable at all: a
name you control can be content-addressed by its hash. **Unison does
exactly this** — code addressed by the hash of its structure — and is
the existence proof that a language can be built this way.

### Three: the code is now pinned to the checkpoint

If the term travels with the data, a deploy cannot invalidate a
checkpoint. That is the win, and it is genuine.

It is also the loss. **You can no longer fix a running workflow by
deploying.** An old run goes on executing the old term, faithfully,
forever. Temporal's versioning and this engine's `Sys.Patch` exist
precisely so that a deploy *can* change what old runs do — "a branch
that new runs take and old runs do not" (chapter 23). With code as
data, patching becomes a **term migration**, which is strictly harder
than a branch.

So the trade does not disappear. It inverts:

| | your code | your position |
|---|---|---|
| replay | always current | must be re-derived |
| serialised term | frozen at capture | exact |

Which of those two you want is a real question with no general answer.
It is worth noticing that the systems people actually operate chose the
first one.

### Four: graph reduction is slow, and that is why nobody does it

Turner's 1979 implementation compiled λ-terms into combinators and
reduced the graph. It was elegant and it was slow, and essentially the
whole history of functional-language implementation since is about not
doing that — supercombinators, the STG machine, and everything after.

Turner himself is the first data point: he added `B` and `C` for the
cases where the argument occurs on only one side, because pure `S`/`K`
expansion produced graphs too large to reduce. The theory chapter
records this, and records the same move surviving in this codebase's
emitted code.

For a workflow whose step is a database round trip, interpretation
speed is irrelevant. For anything in a loop it is fatal, and chapter
20's numbers are the scale to measure it against.

### What SKI actually proves

Not that the appendix is wrong — **that its three columns are
exhaustive.**

`S`, `K` and `I` are the most general possible "enumerate the shapes",
and they still land in that column. They do not freeze the artefact and
they do not re-run it; they replace the code with data you can name.
The bill is the one that whole column charges, paid in full and in
advance:

> You stop writing Scala and start writing terms of a language you
> interpret.

That is a legitimate thing to build. It is what Unison is, it is what
every workflow DSL is, and it is what this library deliberately is not
— because the argument of the entire book is that the program should
be the straight-line code you already wrote.
## The radical version: serialise the source code

The end of the escalation, and it deserves a straight answer: **this
one works.** It is not a thought experiment, and Scala 3 already emits
the format.

**TASTy is serialised source code** — the typed AST, written beside the
classfiles — and `scala.quoted.staging` compiles an `Expr` at run time.
The precedents are real too: Smalltalk and Lisp images, where the world
including its source is the artefact; Erlang's hot code loading, with
two versions of a module live at once; Unison, whose definitions are
addressed by the hash of their tree.

So the question is not whether code can be data. It is what you have
actually bought.

### The environment does not come with it

A continuation is code **plus captured values**. Serialise the source
and, on restore, you compile a fresh function — and you still have
nothing to feed it. The values the closure captured are not addressable
from outside it; they are, in effect, a stack frame.

You can make the environment explicit, by CPS-transforming at the
source level so that every continuation takes its environment as an
argument. That works. It is also **defunctionalisation again**, written
in syntax instead of in an enum.

### Source alone does not determine behaviour

This is the part that decides where the idea belongs. The source
references libraries; the result depends on the compiler version. To
make a stored program mean the same thing later you must store the
source **plus the transitive dependency closure plus the compiler**.

That is a container image.

> **Serialising the source is not a fourth column. It is the first
> one — "freeze the artefact" — taken seriously.**

Which is worth stating plainly, because the two radical proposals in
this appendix land in different columns and neither lands outside:

| radical idea | where it lands |
|---|---|
| SKI combinators | **enumerate the shapes** — the most general form of it |
| serialise the source | **freeze the artefact** — with its whole closure |

Three columns, probed from three directions, and each probe comes back
inside. That is the argument for the trichotomy being exhaustive rather
than merely convenient.

### Three bills, named

- **Restore costs a compile.** Seconds, not microseconds. Fine for a
  run that resumes once a day; fatal for anything else. Chapter 20 is
  the scale to measure against.
- **The journal becomes executable.** A compromised journal is remote
  code execution. A journal of *answers* has a blast radius of "wrong
  data"; a journal of *code* has a blast radius of your process. This
  is the objection that ends the conversation in most organisations,
  and it should be raised early rather than discovered in review.
- **Versioning inverts**, exactly as it does for SKI: the code is
  pinned, so a deploy cannot fix a running instance.

### The question that closes it

What did you gain over storing the **name** of the code — which is what
replay does?

One thing: immunity to deploys. And that same immunity is available
from versioned workers — keep the old artefact running for old runs,
which is ordinary operational practice and what mature engines already
support.

> **Serialising the source is per-run artefact versioning, done the
> expensive way.**

If what you want is "this run keeps its old code", the cheap version of
that wish is a worker pool that still has the old code. If what you
want is "this run keeps its old code *and* I never have to operate two
versions", you are asking for the code to live in the database, and
this section is the price list.

## "The point is not speed — it is not repeating side effects"

The objection that corrects this appendix, and it corrects it at the
root. Everything above weighed the mechanism against **replay's
speed** — seconds of compiler against microseconds of straight-line
prefix. That is the wrong scale. Nobody wants this mechanism in order
to restore faster; they want it so that **the prefix is never
re-executed**, and therefore nothing in it can happen twice. Do the
work, write it down, continue. Until the next write.

### That is transactions, and it is already what this is

The framing is exactly right, and following it all the way is the
quickest route to the real disagreement.

| a database | here |
|---|---|
| the write-ahead log | **the journal** |
| recovery: redo from the log | **replay** |
| a checkpoint, to bound recovery | **chapters (`Snapshots`)** |
| the log is truth, the checkpoint a shortcut | chapter 22, in those words |

A database does not choose between a log and checkpoints. It keeps
both, for different jobs: the log makes each commit durable, the
checkpoint bounds how much of the log a restart must read. This engine
has the same two things, with the same division of labour — and
chapter 22's "the log is the truth; a chapter is only a shortcut" is
that doctrine restated.

So "transactions instead of replay" is not the disagreement, because
replay **is** the recovery half of a transactional design. The
disagreement is one question, and it is narrower and more interesting:

> **What goes into the commit record — the answer, or the state?**

Everything below is about that one choice.

### What writing the STATE would buy, stated properly

Replay is safe here only because `Replayable` **forbids** things. A
durable program's row may hold nothing whose re-execution is
observable: no `Async`, no `Writer`, no `Resource`, nothing that
reaches outside. Every interaction with the world must go through
`pause` or an activity.

That is not an implementation detail. **It is a restriction on the
programming model**, and it is the price chapter 23 charges without
quite calling it a price.

A program restored from a checkpoint does not re-run its prefix, so
the restriction has no reason to exist. You could call a service in a
straight line, in the middle of the program, with no ceremony, and a
restart would resume after the call rather than before it.

**That is a real benefit and the rest of this appendix undersold it.**

### Why the commit record holds the answer instead

Four reasons, in descending order of how much they actually matter.
Speed is not among them.

**One — state migrates badly across a deploy, and answers do not.**

A journal of answers is a **domain-level** interface: *which city*,
*how many nights*, *did the payment clear*. It changes rarely and on
purpose, and when it changes somebody notices, because the questions
are the program's contract with the world.

A checkpoint of program state is an **implementation-level** interface:
the local variables at the save point. Those change whenever anybody
refactors, and nobody files a migration for renaming a local. After a
deploy you are restoring old state into new code, which is a schema
migration over variables that were never designed to be a schema.

This is the one that decides it. A journal survives a refactor; a
snapshot of locals does not.

**Two — a save costs O(state); an answer costs O(1).**

Worth being precise here, because the exactly-once intuition is
slightly off on both sides.

Checkpointing does **not** give exactly-once by itself: die between the
side effect and the save, and the effect repeats on restore. Neither
does journalling: die between performing an activity and journalling
its result, and the activity is retried. **The floor is the same for
both** — at-least-once, with idempotence or a transactional outbox as
the only ways below it.

What differs is the **window**. Journalling closes it per effect;
checkpointing closes it per checkpoint. To make the windows equal you
must checkpoint after **every** side effect — and then each effect
costs one full serialisation of everything live, where the journal
costs one appended record. For a program that has accumulated state,
those are not the same order of magnitude.

**Three — the discipline is replaced, not removed.**

`Replayable` says *do not reach outside between pauses*. Its
replacement would be *do not be holding anything unserialisable at a
save point* — no open connection, no file handle, no pool, no lazily
built cache, no thread.

That is a constraint on **values** instead of on **effects**, and it is
not obviously the lighter of the two. It is also harder to check: an
effect is in the row and the compiler can see it; a live object's
serialisability is a property of the heap at one instant.

**Four — the checkpoint does not remove the term problem.**

To continue you must know *where* to continue. `Segs` hands you the
frames as data, and `Mark` even shows the installed prompts — but each
`K` frame still carries a host function. Saving the state relocates the
problem; it does not dissolve it. Everything earlier in this appendix
still applies to the "where", however well the "what" is handled.

### The engine already writes state — in exactly one place

Which is the strongest thing that can be said for the proposal, so it
should be said.

`continueAs` writes a **seed**: a value that supersedes the history, so
the next replay starts from it rather than from the beginning. That is
a commit record holding *state* rather than *an answer*, sitting in a
design whose every other record holds an answer.

So the two designs are not opposites. **Writing the state at every step
is the generalisation of `continueAs` to every step**, and the engine's
position is not "never write state" but "write state where the history
has stopped describing anything, and write answers everywhere else".

That also says exactly when the generalisation earns its keep: when the
history is long enough, or expensive enough to re-derive, that paying
O(state) per commit beats paying O(1) per commit and O(history) per
recovery. `continueAs` is the cheap way to reach that point without
changing the model — and if a program needs it after every step, that
is a signal worth reading, because a program whose every step
invalidates its history is not really a dialogue.

### Where this design does work, and why

Not a rhetorical concession — there is a class of runtime where the
whole argument comes out the other way:

> **When the runtime's entire state is already data.**

WebAssembly's linear memory is a byte array. It holds no pointers to
host closures, no class references, no file descriptors in the sense a
JVM heap does. You can take the whole of it, write it down, and resume
somewhere else — and durable-execution platforms built on WASM can
therefore do exactly what this section describes, with no replay and
no `Replayable`.

The obstacle on the JVM is **not** speed and **not** the idea. It is
that a JVM heap is a graph of objects pointing at classes, lambdas and
native resources, so "the state" is not a thing you can pick up.

That is the honest shape of the answer: the design is sound, it is
implemented in the wild, and whether it is available to you is decided
by your runtime's memory model rather than by any argument in this
book.

## So what would you actually build?

If, having read all that, you still want checkpoint-without-replay,
here is the honest design advice.

**Do not try to recover the shape of a monadic program.** It is gone,
and every hour spent there is spent re-deriving why `Bind` carries a
function.

**Do decide, per program, which half it belongs to.** Most systems have
both kinds and gain by saying which is which:

- A program with no input whose shape is fixed — fetch these twenty
  keys, apply this rule pack — is **applicative**, and `Static` gives
  you `leaves`, batching, a dry run and a capability list.
- A program that branches and loops but never chooses WHICH PROGRAM to
  run next is an **arrow**, and `Proc` gives you all of the above plus
  a position that is a path, a deploy check, and a picture. `if paid`
  and `while retries < n` are both on this side — see the correction
  above, which is where this list used to send them to the monad.
- A program that chooses its next step from a VALUE — "read the
  workflow's name from an answer and run it" — is **monadic**, because
  that is `ArrowApply` and `ArrowApply` is a monad. Replay is what it
  costs. Keep the code between pauses straight-line so replay stays
  cheap; chapter 22 measured 40 steps for 40 answers.

**Do put the boundary between them somewhere you can name.** They
compose in one direction: `Static.toFree` and `Wf.Proc.program` turn a
static half into a program the monadic half can run, and a `Proc` can
be an activity of a monadic workflow. Not the other way — recovering a
term from a closure is the thing this appendix opened by saying is
impossible, and that part is unchanged.

**And if you find yourself defunctionalising by hand** — writing an
enum of "what to do next" cases and an `apply` over it — stop and
notice what you are building. It is a workflow DSL, it will work, and
the reason it works is the reason it is not Scala any more. That is a
legitimate choice; it is just not a free one, and it is better made on
purpose than arrived at after six months.

## The verdict, which the title promised

This appendix reads, in places, like a list of refutations. That is the
wrong register to end on, because the honest conclusion is the one its
title claims:

> **Done sensibly and carefully, with the limitations known and the
> expectations moderate, all of it works.**

Every route here is implemented somewhere by people who are not
confused. Spark ships closures. Unison content-addresses code. WASM
platforms snapshot linear memory. BPMN engines carry a cursor through a
static graph. None of these is a mistake; each is a trade taken with
open eyes, and the reason this engine took a different one is its own
set of constraints, not a defect in the others.

What separates the versions that work from the ones that do not is
never cleverness. It is four habits:

**Know which half your program is in** — and there are three halves,
which is the joke this appendix had to learn. A fixed shape with no
input is applicative. A shape that branches and loops is an ARROW, and
`leaves`, a path-shaped position, a deploy check and a picture come
with it. Only a program that picks its next step from a value needs a
monad, and replay is its price. Most systems contain all three, and
gain most from saying out loud which is which.

**Put only what the program was told into the commit record** — or
state, where the history has stopped describing anything. That line is
the difference between the journal and `continueAs`, and it is what
keeps chapter 22's trap shut: derived state stored beside the answers
drifts silently, and both copies look right while it does.

**Make the discipline a type and the breach a named method.**
`Replayable` does not work because it is correct. It works because it
cannot be forgotten, and because `unchecked` appears in a diff.

**Ask, per route, what it must survive.** A deploy six months from now,
or a network hop this second. The same mechanism answers those two
questions oppositely, and in this appendix that was the only question
that ever decided anything.

Moderate expectations are the last of it, and the least discussed. None
of these designs gives exactly-once; all of them give at-least-once
with a window, and idempotence or a transactional outbox is what gets
you below that. None of them makes a program survive arbitrary change
to itself. None removes the need to know what your runtime can hand
you. A design that promises otherwise is not more advanced — it has
just not met its second year in production.

## The one-paragraph answer, for when somebody asks again

A continuation is a closure, and a closure is code plus captured
values. The values serialise easily. The code can be serialised too —
TASTy is exactly that — but a term is only meaningful against a
classpath and a compiler, so storing it does not give you a program in
a bottle; it gives you a program pinned to the world it was written
for. That is why the three ways to keep a position are to freeze the
artefact (Spark, and TASTy-plus-CBOR, and serialised source), to
replace the code with data you enumerate (BPMN, any free applicative,
SKI at its limit), or to keep only the inputs and run the artefact
again (replay, and Temporal, and this engine). Everything else is one
of those three wearing a different name — and the deciding question is
rarely "can it be done" but "what does it buy over re-running a
straight-line prefix that takes microseconds".

---

← [28 · A short history](28-a-short-history.md) ·
[Contents](index.md)
