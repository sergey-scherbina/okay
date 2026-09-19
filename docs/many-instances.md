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

**The one thing you must get right: the keys must be DISTINCT within a
row.** The test is by key and by key alone — that is what lets one
signature appear twice — so two members sharing a key have nothing
left to compare:

```scala
type A = Tag.Of["same", Reader % Int]
type B = Tag.Of["same", Reader % String]   // same key: back to square one
```

That row misroutes into exactly the `ClassCastException` a key exists
to prevent, which `TestTag` pins (tag-key-collision, 2026-09-11).

**A shared key across DIFFERENT signatures is fine**, though — the test
asks the key and the signature both (tag-test-the-signature-too), so
`Of["k", Beep] + Of["k", Buzz]` is an ordinary row. What no runtime
test can fix is the case above: same signature, same key, with the type
parameter erased before anything could compare it.

Nothing checks distinctness today; a compile-time check is filed as
`tag-distinct-keys`. Until it exists, the rule is yours to keep — and
it is cheap to keep, because keys are literals you can read side by
side in the type alias block.

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

## Route 3: `Instances` — any effect, instances MADE at run time

`Tag` gives any effect, named at compile time. `Refs` gives instances
made at run time, but only of state. The corner they leave open is the
one real systems ask for: **one `Users` per tenant, one `Cache` per
shard, where the tenants come out of a config file** nobody has read at
compile time.

`Instances` is `Tag` with the key read at run time:

```scala
val tenants = List("alpha", "beta", "gamma")      // data, not literals
val handles = tenants.map(Instances.handle)

val p: List[String] ! Instances.Of[Store] =
  handles.foldRight(pure[Instances.Of[Store], List[String]](Nil)) { (h, rest) =>
    Instances.at[Store](h)(Store.Get()).flatMap(x => rest.map(x :: _))
  }
```

One row member per SIGNATURE, however many instances of it — the same
trade `Refs` makes, for the same reason: a type cannot list what does
not exist yet. `route(h)(p)` sends an already-written program's
operations to one instance, exactly as `Tag.tag` does with a literal.

**Handling, two ways.** `Instances.handler(pick)` chooses a handler by
handle and runs every instance in one pass — `pick` is an ordinary
function, so per-instance state is the caller's to keep. When an
instance wants the effect's OWN runner instead, at its own initial
state, `only(h)` strips that handle back to the plain signature and
leaves the others in the row:

```scala
val afterSmall = State.handle(1)(Instances.only[State % Int](small)(p))
val done       = State.handle(10)(Instances.only[State % Int](big)(afterSmall))
Instances.exhausted[State % Int, ...](done)   // asserts none survived
```

`exhausted` is the honest end of that: a type cannot know the instances
are used up, so the assertion is made where the caller believes it, and
a survivor names the handle that was never stripped.

**The test asks the SIGNATURE first and the handle never**, which is
one more than `Tag` does — so `Of[Store] + Of[Reader % Int]` is an
ordinary row, and only a shared handle within one signature can confuse
anything. A handle is a fresh object, so sharing one is deliberate.

**Cost:** one wrapper per operation, a row that no longer says which
instances exist, and — unlike `Refs` — no cast: the handle is compared
by reference and the operation is already typed.

## Route 4: a fresh PROMPT — the instances are NESTED

`Delim` already offers the third route: each handler installation
creates a fresh prompt, and a prompt is a first-class tag. An instance
then has an identity that no type has to name and no key has to be
invented for.

It is the most scoped of the three — instances nest and separate
dynamically, which neither a key nor a cell gives you — and the most
invasive, because the program carries the prompt.

## Choosing

| the instances are… | of which effect | route | identity is | cost |
|---|---|---|---|---|
| named when you write the type | any | `Tag` | a compile-time key | a wrapper per operation, no cast |
| made at run time | state | `Refs` | the cell | a heap and one cast |
| made at run time | any | `Instances` | the handle | a wrapper per operation, no cast |
| nested and separated dynamically | any | `Delim` prompt | the installation | the program carries the prompt |

Use a key when the instances can be named, `Refs` when they are cells,
`Instances` when they are instances of something bigger made from data,
and a prompt when they must nest.

## What this means for the bare rule

You will still find the bare limitation stated in the sources — in
`State`'s scaladoc, in `typepedia.md`, in `TestRowIdentity`. It is
still true *of a bare row*: `State % Int + State % String` misroutes,
and the test that proves it should keep running. What changed is that
the limitation is no longer the end of the story — `Tag` generalised
what `Keyed` once did for `State` alone, and `Refs` covers what a type
cannot list. Every one of those places now points here.

## The compiler will now tell you

`summon[Distinct[R]]` refuses a row whose members cannot be told apart
at run time, and says which two and what to do about it. It is the
statement `TestRowIdentity` demonstrates, moved to the time you can
still fix it:

```scala
summon[Distinct[Reader % Int + Writer % String]]        // fine
summon[Distinct[Tag.Of["a", Reader % Int] + Tag.Of["b", Reader % Int]]]  // fine
summon[Distinct[Reader % Int + Reader % String]]        // refused
```

What it compares is **the test, not the type**, and that distinction is
the whole design. `Writer % String + Writer % Int` is a good row — the
two writers collect the right elements, because `writerK` reads the
told VALUE with a `Typeable[W]` rather than testing the class. The
same shape over `Reader` is broken, because `Ask()` is one class
whatever `R` is. Nothing about the two TYPES says which is which; only
the instance knows, so the instance declares it:

```scala
given writerK[W](using t: Typeable[W]): TypeableK.ByValue[Writer % W]
```

Unmarked means "tests by erasure", which is the safe default: an
instance that really is finer gets refused until someone adds the
word, while the opposite default would pass a row that misroutes.

The wrappers are read structurally. `Tag.Of[K, F]` collides only with
the same key over a colliding `F`; `Instances.Of[F]` collides only
with another `Instances.Of[F]`, which the language already forbids:
`+` is a union, `F | F` is `F`, so a row cannot repeat a member at
all, and two `Instances.Of[Ping]` are the single member it was written
to be. What the check is for is the pair that is two *different types*
with one runtime identity. An
abstract member is always allowed: a residual `G` in an interpreter is
unknown where it is written and checked where it is instantiated,
which is the only place the answer exists.

## Where the code is

- `src/main/scala/Distinct.scala` — the compile-time check: the row
  walk, the identity a member is compared by, and the error
- `src/test/scala/TestDistinct.scala` — every row `TestRowIdentity`
  runs, at compile time: the two readers refused, the two writers
  allowed, keys, `Instances`, an abstract member, `Pure`

- `src/main/scala/Instances.scala` — the run-time handle: `at`,
  `route`, `handler`, `only`, `exhausted`
- `src/test/scala/TestInstances.scala` — two instances of a signature
  that carries nothing, instances made IN A LOOP, an already-written
  program routed to one, `only` handing one instance to `State.handle`
  at its own state, and two signatures as an ordinary row
- `src/main/scala/Tag.scala` — 84 lines: the case class, the
  key-testing `Effect` instance, `one`, `tag`, `untag`, `handler`
- `src/main/scala/Refs.scala` — the cells, the heap, the one cast and
  why it is sound
- `src/test/scala/TestTag.scala` — one function at two states, a key
  telling apart what carries nothing, the comonadic handler
- `src/test/scala/TestRefs.scala` — two cells, cells in a loop, same
  type twice, other effects forwarding
- `src/test/scala/TestRowIdentity.scala` — the bare rule, still pinned
