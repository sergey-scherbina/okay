# Several instances of one effect in a row

A row lists the effects a program may perform: `State % Int + Writer %
String + Async`. A natural question follows almost immediately — **can
it list the same effect twice?** Two counters. A cache in front of a
cache. One `State % Int` for the parser's position and another for its
depth.

The short answer is yes, three different ways, and which one to reach
for is decided by *where the instances come from*: named in the type,
made at run time, or nested dynamically. The long answer starts with
why the question is interesting at all, because the reason is the same
mechanism that makes rows cheap.

## Why a bare row holds one of each signature

A row is not a tagged union built at compile time. It is split at RUN
time: when a program suspends on an operation, the runner asks each
member of the row "is this yours?", and the member answers with a
`Typeable`-shaped test — a pattern match on the operation's class.

That test is the whole mechanism, and it is why an okay row costs
nothing to carry. It is also its one limit: **two members can be told
apart exactly when the operation carries something the test can
compare.**

Look at `State`:

```scala
enum State[S, +A]:
  case Get() extends State[S, S]
  case Set(s: S) extends State[S, S]
```

`Get()` is a case with no fields. At run time it is one object of one
class, and `S` was erased before it ever existed. So in a row of
`State % Int + State % String`, the test for the first member answers
"yes, mine" to an operation that belonged to the second. The first
handler answers the ask with an `Int`, the `String` continuation
receives it, and the program fails with a `ClassCastException` at the
first wrong answer.

Two things are worth saying about that failure.

**It is loud, not silent.** The row misroutes into a cast error rather
than into a plausible wrong answer — `TestRowIdentity` demonstrates
exactly this, so the behaviour is pinned rather than hoped for.

**It is not universal.** `Writer % W` escapes without trying, because
its operation IS the value told: `Say("x")` carries a `String`, so the
test has something to compare, and `Writer % Int + Writer % String`
routes correctly. The rule is not "one instance per effect" but **one
instance per signature that carries no identity** — the effects it
binds are `State`, `Reader`, `Take` and `Throws`, and each says so in
its own scaladoc.

## Route 1: `Tag` — the instances are NAMED

If you can name the instances when you write the type, give each
operation a key and the test has something to compare:

```scala
type Small = Tag.Of["small", State % Int]
type Big   = Tag.Of["big",   State % Int]
```

`Tag[K, F, A]` is a case class wrapping an `F[A]` together with a key,
and its `Effect` instance tests **by key** — everything else about `F`
is already erased, so the key is the only thing worth testing.

What makes this worth having is not the wrapper but `tag`:

```scala
val twice: (Int, Int) ! (Small + Big) =
  for
    a <- Tag.tag["small", State % Int](count).plus[Big]
    b <- Tag.tag["big",   State % Int][Int, Pure](count).at[Small + Big]
  yield (a, b)
```

`tag` **walks a finished program** and puts every `F` operation under
the key, leaving the rest of the row alone. So `count` — an ordinary
function written against a plain `State % Int`, by someone who never
heard of tags, possibly already compiled — runs twice in one program
at two different states. That is the whole point: the caller decides
there are two instances, not the author of the function.

Handling needs no new handler:

```scala
State.run(0)(Tag.untag["small", State % Int](twice))
```

`untag` strips one key and hands the plain signature back to the
effect's own runner. There is also `Tag.handler`, which lifts a
comonadic `Handler[F]` to `Handler[Of[K, F]]` for the same reason.

**Cost:** none at run time beyond one wrapper object per operation,
and nothing casts. The row lists the instances, so the compiler knows
how many there are.

**The one syntactic wart**, worth knowing before you meet it: in the
second line above, `G` is inferred from the program, and a program of
`State` alone infers `G = State % Int` rather than `Pure`. `.plus`
accepts that and `.at` does not, which is why the second line writes
its type clause out (generalized-method-syntax, 2026-09-11).

## Route 2: `Refs` — the instances are MADE

A key cannot name what does not exist yet: one cell per request, one
per node of a walk, one per element of a list nobody has read. For
that, `Refs` says only *this program uses refs*, and identity is the
cell itself:

```scala
val p: (Int, String) ! Refs =
  for
    a <- Refs.ref(1)
    b <- Refs.ref("ada")
    _ <- Refs.write(a, 2)
    x <- Refs.read(a)
    y <- Refs.read(b)
  yield (x, y)
```

One row member however many cells there are. `TestRefs` pins the cases
that matter: two cells in one row, cells made **in a loop** — what a
type could not have listed — two cells of the same value type staying
separate, and other effects forwarding through the heap untouched.

**Cost, stated plainly:** the row no longer says which states exist,
and the handler's heap is keyed by identity, so reading a cell returns
an `Any` that ONE cast turns back into its type. That cast is sound
because a `Ref[S]` is only ever made by `New(init: S)` and only ever
written by `Write(c: Ref[S], s: S)`, so what comes out of a slot is
what the same `S` put in. It is the same cast `TMap` makes, for the
same reason.

And a measured footnote, because the obvious fix does not work:
Scala 3's generalized method syntax lets a type clause follow a term
clause, and its own documented example is a heap `get` whose value
type depends on the key. Tried here (refs-typed-heap, 2026-09-11) it
does not remove the cast — `Refs.scala` carries the detail.

## Route 3: a fresh PROMPT — the instances are NESTED

`Delim` already offers the third route: each handler installation
creates a fresh prompt, and a prompt is a first-class tag. An instance
then has an identity that no type has to name and no key has to be
invented for.

It is the most scoped of the three — instances nest and separate
dynamically, which neither a key nor a cell gives you — and the most
invasive, because the program carries the prompt.

## Choosing

| the instances are… | route | identity is | cost |
|---|---|---|---|
| named when you write the type | `Tag` | a compile-time key | a wrapper per operation, no cast |
| made at run time | `Refs` | the cell | a heap and one cast |
| nested and separated dynamically | `Delim` prompt | the installation | the program carries the prompt |

Use a key when the instances can be named, cells when they are made, a
prompt when they must nest.

## What this means for the bare rule

You will still find the bare limitation stated in the sources — in
`State`'s scaladoc, in `typepedia.md`, in `TestRowIdentity`. It is
still true *of a bare row*: `State % Int + State % String` misroutes,
and the test that proves it should keep running. What changed is that
the limitation is no longer the end of the story — `Tag` generalised
what `Keyed` once did for `State` alone, and `Refs` covers what a type
cannot list. Every one of those places now points here.

## Where the code is

- `src/main/scala/Tag.scala` — 84 lines: the case class, the
  key-testing `Effect` instance, `one`, `tag`, `untag`, `handler`
- `src/main/scala/Refs.scala` — the cells, the heap, the one cast and
  why it is sound
- `src/test/scala/TestTag.scala` — one function at two states, a key
  telling apart what carries nothing, the comonadic handler
- `src/test/scala/TestRefs.scala` — two cells, cells in a loop, same
  type twice, other effects forwarding
- `src/test/scala/TestRowIdentity.scala` — the bare rule, still pinned
