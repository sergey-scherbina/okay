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

### What it costs, which is the reason this engine did not

No data-dependent structure. `Select` buys a conditional with **both
sides written down**, and the source names the approximation honestly:

> `leaves` reports both sides of every `Select`, because which side
> runs is decided by a value that does not exist yet. It is an upper
> bound.

An upper bound is what a batcher, a capability list and a dry run all
want. It is not what a workflow wants. You cannot write

```scala
val nights = !w.pause("how many nights?")
for _ <- 1 to nights do !w.pause("which room?")
```

because the *shape* now depends on an answer. The five-line booking of
chapter 23 ends on `if !w.patch("promo")`, and the reason its programs
are worth writing at all is that the line between two pauses is
ordinary Scala.

So the trade is real and it is a trade:

| | position | restore | language |
|---|---|---|---|
| monadic | a closure — cannot be named | replay, O(answers) | anything |
| applicative / selective | an index into a known shape | a cursor, O(1) | no data-dependent structure |

This engine took the left column deliberately, because the code between
pauses is straight-line and replaying it is cheap — chapter 22 measured
it: 40 answers, 40 steps.

### The caveat that survives either choice

An applicative does not make the *program* serialisable either.
`Pure(a)` may hold any value, functions included, and the spine of
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

## So what would you actually build?

If, having read all that, you still want checkpoint-without-replay,
here is the honest design advice.

**Do not try to recover the shape of a monadic program.** It is gone,
and every hour spent there is spent re-deriving why `Bind` carries a
function.

**Do decide, per program, which half it belongs to.** Most systems have
both kinds and gain by saying which is which:

- A program whose shape is fixed — fetch these twenty keys, run these
  five stages, apply this rule pack — is **static**, and a `Static`-like
  type gives you `leaves`, batching, a dry run, a capability list and,
  if you want it, a cursor.
- A program whose shape depends on its answers — anything with `if
  paid` or `while retries < n` in it — is **monadic**, and replay is
  what it costs. Keep the code between pauses straight-line so replay
  stays cheap; chapter 22 measured 40 steps for 40 answers.

**Do put the boundary between them somewhere you can name.** The two
halves compose in one direction: `toFree` turns the static half into a
program the monadic half can run. Not the other way.

**And if you find yourself defunctionalising by hand** — writing an
enum of "what to do next" cases and an `apply` over it — stop and
notice what you are building. It is a workflow DSL, it will work, and
the reason it works is the reason it is not Scala any more. That is a
legitimate choice; it is just not a free one, and it is better made on
purpose than arrived at after six months.

## The one-paragraph answer, for when somebody asks again

A continuation is a closure, and a closure is code plus captured
values. The values serialise; the code does not, because "this code"
means "this compiled artefact", and the whole point of a durable system
is that the artefact changes. So the three ways to keep a position are
to freeze the artefact (Spark), to replace the code with data you
enumerate (BPMN, and any free applicative), or to keep only the inputs
and run the artefact again (replay, and Temporal, and this engine).
Everything else is one of those three wearing a different name.

---

← [28 · A short history](28-a-short-history.md) ·
[Contents](index.md)
