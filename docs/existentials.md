# The cast that could not go, and how it went

For most of this library's life, resuming a continuation after a
`Writer.tell` was an assertion the compiler could not check. Six
encodings were tried against it. Five failed — four of them for one
mechanical reason worth knowing — and the sixth worked by giving up
the thing the other five were trying to preserve.

This page records what was tried, what the compiler said to each, and
what the bytecode showed. It is kept because the five failures are
the useful part: they are the shapes anyone reaching for this problem
will reach for first, and the reason they fail is not the one the
type-theoretic framing suggests.

The sections below describe the situation as it was, in the present
tense, and the outcome is at the end.

## The site

```scala
case Bind(Effect(e), k) => <|>[Take % I, Writer % M](e) match
  case Left(Take.Await())  => effect(Take.Await()).flatMap(oi => pull(k(oi))(cont))   // no cast
  case Right(w)            => cont(Some(okay.out(w)), k(Erased.resumeWith(w)))        // cast
```

`Bind` introduces an existential — call it `X`; the compiler prints
`A$2`. So `e: (Take % I + Writer % M)[X]` and `k: X => …`, and both
branches must produce an `X` to resume the continuation with.

**`Take` is a GADT.** `Await() extends Take[V, Option[V]]`, so matching
the constructor refines `X =:= Option[I]` and `oi` typechecks as `X`
with nothing asserted. The equation falls out of the pattern match.

**`Writer` is an identity signature.** `opaque type Writer[W, +A] = W`
— zero runtime representation, which is exactly what lets `tell`
allocate nothing. Its only injector is `Writer(m): Writer[M, M]`, so
the only instantiation any *value* can have is `X = M`; the equation is
true. But there is no constructor to match, because the value **is**
the `M`. Nothing can witness it.

Delete the cast and the compiler says it in two lines:

```
Found:    (m : M)
Required: A$2
```

Why an `X` is needed at all: `Bind(Effect(e), k)` means "perform `e`,
then continue with its result", and a tell's result is *the value
told* — which is why the signature can be an identity, and why nothing
has to be allocated to carry the answer. The value is in hand
(`okay.out(w): M`). Only its type is unavailable. The free `tell` and
this cast are one design decision seen from two sides.

## What was tried

### 1. Naming the existential (`Kind`, and the newtype encoding)

From [Existential Crisis: Implementing mapK in Scala 3][raquo], both
forms:

```scala
type Kind[K[_]] = { type A; type T = K[A] }
type Type[+F[_]] <: (Any { type T })
```

Both **name** an existential and give it a stable path (`v.T`). Both
were written out against the real site, and both produce the same two
lines, now reading `Found: W / Required: v.T`. Naming a thing is not
knowing what it is. The positive half does work — `mapK` over a
`Pair[F, G, A]` typechecks — which is worth knowing, and is the half
this site does not need.

[raquo]: https://dev.to/raquo/existential-crisis-implementing-mapk-in-scala-3-2fo1

### 2. Evidence, minted once (`Writer.told`)

```scala
private val refl: Any =:= Any = summon[Any =:= Any]
def told[W, A]: A =:= W = refl.asInstanceOf[A =:= W]
def answer[W, A](w: Writer[W, A]): A = told[W, A].flip(out(w))
```

This does not remove the assertion — nothing can — but it moves it to
the single place where the argument for it is written down, and gives
the theorem a name and a type. Every use site then applies it by name
and the compiler checks the application. `=:=` erases to the identity,
so the evidence costs nothing at run time.

`summon[A =:= W]` in place of the cast does not compile:
`Cannot prove that A =:= W`. `=:=` has one instance, `given [A]: A =:= A`,
so summoning it requires the compiler to already know the equation —
which is the thing that cannot be known. If `summon` worked here there
would be no cast to write.

**This is what the code does today.**

### 3. Intersection (`W & A`)

```scala
opaque type Writer[W, +A] = W & A
```

`answer` becomes a subtyping step instead of a cast, which looks like a
strict improvement. Publishing the bound as well —

```scala
opaque type Writer[W, +A] <: A = W & A
```

— makes `Writer[M, X] <: X` visible outside the file and removes **4 of
the 11** casts in `Pipe`.

It fails 22 tests with `java.lang.String cannot be cast to
scala.runtime.Nothing$`. `A` is inferred, and it is inferred as
`Nothing` wherever the answer is unused; the compiler believes the
claim and emits a checkcast to it. Removing the published bound does
not help — inside the file the subtyping is still known, and the cast
simply moves to the instantiation site.

The tempting counter-argument, that only `Writer[W, W]` values can
exist, is incomplete: covariance widens `A` upward, but **inference
narrows it downward**.

### 4. A match type as the alias

```scala
type Writer[W, +A] = A match
  case Nothing => W
  case _ => W & A
```

The `case Nothing` guard aims exactly at what killed #3, and the
instinct is right. Three things stop it.

**The language.** `Modifier opaque is not allowed for this definition`
— a match type cannot be an opaque alias. So it has to be transparent,
which gives up the only thing guaranteeing that `Writer.apply` is the
sole injector — and that guarantee *is* the soundness argument for the
phantom.

**Inference leaks.** Transparent, it compiles and removes 4 of the 11
casts, but the structure leaks into resolution elsewhere: the test
sources stop compiling (`value uncons is not a member of Producer[Long]`).

**It agrees with more than it proves.** In isolation,
`def answer[W, A](w: W3[W, A]): A = w` typechecks, though the `Nothing`
branch would need `W <: Nothing`. A stuck match type is checked loosely
enough that compiling proves nothing here; only running the suite does.

Without the guard, a match type gets *stuck* rather than lying, and the
compiler says why more precisely than prose can:

```
failed since selector A
does not match  case W => W
and cannot be shown to be disjoint from it either.
```

Match types reduce on **known** types. Under a `Bind` the answer type is
the abstract existential, so the match cannot advance. At a concrete
`A` it reduces perfectly — which is to say it works exactly where no
help is needed.

### 5. The match type inside the alias (`W & Tag[W, A]`)

```scala
type Tag[W, A] = A match
  case Nothing => W
  case _ => A

opaque type Writer[W, +A] = W & Tag[W, A]
```

The sharpest of the five. It clears the language restriction that
stopped #4 — the match type is in the *body* of the alias, not the
alias itself — so `opaque` is accepted and the injector discipline
survives. It compiles, and `answer` needs no cast.

It fails the same 22 tests, `String cannot be cast to Nothing$`.

## Why: erasure follows the representation

The four failures are one failure, and it is not about provability.
Disassembling the same source both ways shows it directly.

`answer` has the **same** erased signature either way:

```
public <W, A> A answer(W);      // (Object)Object
```

The difference is in the caller. With `opaque type Writer[W, +A] = W`,
`Writer.fold`'s inner loop calls it and lets the result flow on:

```
224: invokevirtual  okay/Writer$package$.answer:(Ljava/lang/Object;)Ljava/lang/Object;
```

Nine checkcasts appear in that method, all structural — `Free$Pure`,
`Free$Bind`, `Left`, `Right`. None concerns `A`, because the
representation does not mention `A`, so at the bytecode level the value
travels `Object → Object` and there is nothing to coerce it to.

With `W & Tag[W, A]` the compiler knows `Writer[W, A] <: Tag[W, A]`.
`answer` collapses to the identity — the call disappears from the
listing entirely — and in its place stands:

```
139: checkcast  scala/runtime/Nothing$
432: checkcast  scala/runtime/Nothing$
```

`Nothing` erases to `scala.runtime.Nothing$`, a class with no
instances. Any value reaching that instruction dies, and in
`Writer.fold` a told `String` reaches it.

So the rule is mechanical, and stronger than "the type must not claim
anything about `A`":

> **The representation must not mention `A` at all** — erasure follows
> the representation, not the claims made about it.

The `case Nothing` guard cannot save an encoding, because under a
`Bind` the selector is abstract, the match type is stuck, and the stuck
form still mentions `A`. The guard fires precisely where it is not
needed.

`told` survives for the same mechanical reason: the representation
stays `W`, `=:=` erases to the identity, `answer` remains a real
`(Object)Object` call, and there is nothing to coerce.

## The same law elsewhere

`ChunkBuf` obeys it, and was discovered the hard way there first:

```scala
opaque type ChunkBuf[A] = Array[?]      // works
opaque type ChunkBuf[A] = Array[A]      // ClassCastException
```

Both were built; the second crashes for the identical reason. `Array[?]`
does not mention the parameter, so a generically allocated
`Array[AnyRef]` standing in for a `ChunkBuf[Long]` contradicts nothing.
`Array[A]` mentions it, erasure follows, and the contradiction becomes
an instruction. A match type on the backing failed the same way.

The two places in the library that assert an element type are the two
places whose representation is deliberately blind to it.

## What actually happened: the sixth attempt worked

The five above all tried to keep the identity representation and
recover the answer type anyway. The sixth gave up the representation
instead, and that is the one that worked.

```scala
enum Writer[W, +A]:
  case Say(w: W) extends Writer[W, Unit]
```

Matching `Say(w)` refines the answer type to `Unit`, so resuming a
continuation is `k(())` and asserts nothing. It removes:

- `answer` entirely — twelve sites across `Writer`, `Pipe`, and the
  kyo/llm/agent interops;
- three of the five `Erased.unreachable` uses in `Pipe`, where a bare
  writer `Effect` ends a program and the refinement gives `Unit` there
  too;
- the `okay.out(w)` calls that went with them, now field reads;
- and the caveat the identity encoding always carried: *"a told String
  is just a String at run time, so forward only effects whose
  operations are class-distinct from W."* A `Say` is class-distinct
  from everything, so `TypeableK` tests the operation's own class
  first and the told value's class only to separate two writers in one
  row.

**Two measurements, and only one of them was the right one to decide
on.** An isolated build-and-fold of 10k tells put the wrapper at +25%
(59.8 -> 75.0us). The real `WriterBenchmark`, which also collects what
was told, came in at **198.0us against 203.2 for the identity
encoding** — no cost at all. The allocation is dwarfed by the rest of
the work, and the constructor match is no more expensive than the
class test the identity encoding needed anyway. The isolated number
was not wrong; it was the wrong fraction of the program to decide on.

(A first version of that isolated benchmark reported the wrapper as 2x
*faster*, because the identity lane also built a 10k-element `Seq` and
the tagged one counted in a `var`. Two lanes must do the same work.)

What did get slower, slightly: the accumulator dispatch in
`Writer.fold` is worth 12% now rather than 35% (95.9 against 108.4),
since the loop matches a constructor instead of reading a value.

## What is left

Eleven sites in `Pipe`, three shapes:

- **`resumeWith`** — the theorem above, applied where the compiler holds
  only the existential.
- **`reinject`** — the same equation in the other direction: an
  operation known to have come from a row the compiler has forgotten
  about.
- **`unreachable`** — a continuation that provably never runs.

All three are about forwarded effects and the rows they move between,
not about writers. The writer's own assertions are gone.

The lesson for a seventh attempt, if one is ever needed elsewhere: the
five failures all tried to make a representation that carries no tag
answer a question about types anyway, and erasure follows the
representation. The thing that worked was to carry the tag. It cost
nothing measurable here, and it is worth measuring rather than
assuming before ruling it out somewhere else.
