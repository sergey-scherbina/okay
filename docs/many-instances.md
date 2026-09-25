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

The compiler checks it: `Distinct[R]` (below) refuses a row with two
members that cannot be told apart, and the runners that split such a
row (`Reader.run`, `Handler.union`, …) require it (tag-distinct-keys,
2026-09-11). Keys are literals, so the collision is also easy to see
by eye in the type alias block.

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

Each handler INSTALLATION creates a fresh prompt, and the body reaches
it through the instance value the installation hands it. This is the
design of "Binders by day, labels by night" (Biernacki, Piróg, Polesiuk
and Sieczkowski, POPL 2020): a lexical binder in the types, a fresh
label at run time. `okay.Lexical` is the API:

```scala
val lex = run(Lexical.State.deep[Int, Int, Delim + Pure](0) { outer =>
  Lexical.State.deep[Int, Int, Delim + Pure](10) { inner => outer.get.flatMap(o => inner.get.map(i => o * 100 + i)) }
    .map(_._2)
})
assertEquals(lex, (0, 10), "outer answered 0, inner answered 10")
```

Two things set it apart from the other routes. **The effect never
enters the row**: the program is `A ! Delim + G`, so there is nothing
to split, test or misroute, and two `State[Int]` are two names. And
**there is no accidental handling**: `outer.get` names its
installation, so the capture passes the inner handler of the same
effect untouched. On a row that program cannot be written at all,
because `get` can only mean the innermost `State % Int`.

Any handler can be written this way from its clauses, including one
that is not tail-resumptive:

```scala
case Flip.Coin() => k(true).flatMap(xs => k(false).map(xs ++ _))
```

**The strategy is yours to name, and there is a default.** Three
strategies run the same body, because the body only sees `Inst[F, G]`:

- `Lexical.tail` answers each operation IN PLACE (evidence passing,
  Xie et al., ICFP 2020), with the handler's state in a cell made per
  run. Its one unsafe shape is a capture from outside the installation
  resuming its body twice. That shape throws `MultiShotAcrossTail`
  rather than sharing the cell.
- `Lexical.deep` runs the clauses with shift0 under a `dollar` whose
  return function is the return clause. It suits anything, including
  clauses that call `k` twice or never. It costs about 4x a row
  handler.
- `Lexical.shallow` uses control0, and the clause re-installs the
  handler if it wants to.

**You pay for what the row can do.** The instance's type carries the
body's whole row, `Inst[F, G]`. `deep` and `shallow` need `Delim` in it.
`tail` on a row WITHOUT `Delim` cannot be crossed by a capture, so it
gets no guard and no machine, and the program is an ordinary one:

```scala
val p: (Int, Int) ! Pure = Lexical.State[Int, Int, Pure](5)(s => s.get.flatMap(v => s.set(v * 3)))
```

The guard and the `Delim` machine appear only when the row has `Delim`.

**One more strategy, by name only: `walk`.** The installation walks its
body the way a row handler does. Its operations are inert
`Inject(Local.Op(owner, e))` nodes of one shared signature, `Local`, and
the state is threaded through the walk, with no cell and no `Delay` per
operation. It costs 1.29x a row handler's bytes, where `tail` costs
1.65x. The row gets one `Local` however many walk instances there are,
and `Lexical.runLocal` goes at the top:

```scala
assertEquals(!.run(Lexical.runLocal(Lexical.State.walk[Int, Int, Pure](5)(s => s.get.flatMap(v => s.set(v * 3))))), (15, 15))
```

A walk sees the program's spine. An instance operation performed inside
a `Delim` delimiter's body, such as a `reset`, a `dollar` or a
`Layered.reify`, reaches the machine and not the walk. `runLocal` then
throws `LocalEscaped`: it does not answer wrongly. Put `Delim.run` inside
the walk and the walk sees those operations in order. It is not the
default because of that rule and because `Local` has to appear in the
row.

`Lexical.handle` picks by what the clauses are: `TailClauses` run tail,
`Clauses` run deep, `ShallowClauses` run shallow. `Lexical.State(s0)`
is tail. `Lexical.Stacked` has deep and tail instances whose use
outside their installation does not compile. Details and numbers are
in specs/lexical-instances.md. A row handler stays the right choice for
one handler of a kind.

## Choosing

| the instances are… | of which effect | route | identity is | cost |
|---|---|---|---|---|
| named when you write the type | any | `Tag` | a compile-time key | a wrapper per operation, no cast |
| made at run time | state | `Refs` | the cell | a heap and one cast |
| made at run time | any | `Instances` | the handle | a wrapper per operation, no cast |
| nested, or addressed past a handler of the same effect | any | `Lexical` | the installation (a prompt) | ~4x a row handler (capture per operation) |

Use a key when the instances can be named, `Refs` when they are cells,
`Instances` when they are instances of something bigger made from data,
and `Lexical` when they must nest, or an operation must reach one
handler past another of the same effect.

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

**Where it is asked.** `Handler.union` and `Handler.flat` compose a
split, so they require it; and since distinct-on-handlers (2026-09-24)
so does every handler that splits a PARAMETERISED signature out of an
open row — `State.handle`/`zoomWith`, `Reader.run`/`unlift`/`local`,
`Writer.run`/`collect`/`fold`/`foldWith`/`foldUntil`/`map`/`expand`,
`runEither`/`runThrows`/`runUnsafe`/`orElse`, and the kernels
`!.relay`/`translate`/`interpret` (`interpret` checks its target row as
well). So `Reader.run(7)(prog)` over `Reader % Int + Reader % String`
no longer compiles, where it used to fail at run time. A caller that
passes a handler's using clauses positionally now passes this one
first: `Writer.fold[W, S, A, F](p)(using summon)(using summon, fold)`.

One shape `Distinct` cannot see, and the handlers now get right
instead: a handler whose rest is INFERRED as the row itself.
`Writer.collect(Writer.map(p)(f))` solves map's rest as `Writer % W`
(`F | F` is `F`, so there is no pair to refuse), and `map` used to test
the rest first — every `Say` was forwarded unmapped, a silently wrong
answer. `map`, `expand`, `uncons`, the stream iterator, `Source`'s
producer and `Pipe`'s pulls all test the Writer first now.

What it compares is **the test, not the type**, and that distinction is
the whole design. `Writer % String + Writer % Int` is a good row — the
two writers collect the right elements — but only under
`Writer.byValue.writerK`, which reads the told VALUE with a
`Typeable[W]` rather than testing the class, and is an OPT-IN:

```scala
import okay.Writer.byValue.given
given writerK[W](using t: Typeable[W]): TypeableK.ByValue[Writer % W]
```

Writer's DEFAULT test (writer-typeablek-by-class, 2026-09-19) is the
class of `Say` alone — total, since `Say` is Writer's only constructor,
and free of the E092 "cannot be checked at runtime" warning a
`Typeable[Chunk[Byte]]` or `Typeable[O]` costs at every call site (it
had cost 23 `@nowarn`s across ten modules, for a two-Writer row no
module had). So without the import `Distinct` refuses
`Writer % String + Writer % Int`, exactly as it refuses the same shape
over `Reader`, where `Ask()` is one class whatever `R` is. Nothing about
the two TYPES says which is which; only the instance knows, so the
instance declares it.

Unmarked means "tests by erasure", which is the safe default: an
instance that really is finer gets refused until someone adds the
word (or the import), while the opposite default would pass a row that
misroutes.

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
  `route`, `handler`, `only`, `exhausted`; and, in the same file
  (its sibling corner of the table above), `Tag`: the case class, the
  key-testing `Effect` instance, `one`, `tag`, `untag`, `handler`
- `src/test/scala/TestInstances.scala` — two instances of a signature
  that carries nothing, instances made IN A LOOP, an already-written
  program routed to one, `only` handing one instance to `State.handle`
  at its own state, and two signatures as an ordinary row
- `src/main/scala/Refs.scala` — the cells, the heap, the one cast and
  why it is sound
- `src/test/scala/TestTag.scala` — one function at two states, a key
  telling apart what carries nothing, the comonadic handler
- `src/test/scala/TestRefs.scala` — two cells, cells in a loop, same
  type twice, other effects forwarding
- `src/test/scala/TestRowIdentity.scala` — the bare rule, still pinned
