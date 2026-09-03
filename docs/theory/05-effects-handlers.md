# 5. Algebraic effects and handlers

## Operations, then handlers

Gordon Plotkin and John Power reframed Moggi's monads from the inside:
an effect is not a type constructor first but a set of **operations**
with equations — nondeterminism *is* `choose`, state *is* `get`/`put` —
and the monad arises from the algebraic theory \[[Plotkin & Power 2002,
2003](#ref-plotkin-2002)\]. The payoff of thinking operation-first is separation: a program
*performs* operations; what they mean is decided elsewhere.

"Elsewhere" got its name with Plotkin and Matija Pretnar's **handlers**
\[[Plotkin & Pretnar 2009](#ref-plotkin-2009)\]: a handler gives each operation an
interpretation that receives the operation's payload *and the
continuation of the program at that point*. Exceptions discard the
continuation; state threads a value through it; nondeterminism calls it
several times. Handlers generalize `try/catch` from "catch a throw" to
"catch any operation", and they compose: unhandled operations pass
through to an outer handler.

The third ingredient is making the *set of pending effects* extensible
and inferable. Kiselyov, Sabry and Swords' extensible effects
\[[Kiselyov, Sabry & Swords 2013](#ref-kiselyov-2013)\] replaced monad-transformer stacks with
a single monad over an open union of signatures — the design Okay
follows, with chapter 4's freer monad as the carrier.

## What the middle constructor decides

`Free` and `Cont` are the same three constructors. `Pure` and `Bind` are
identical in both; only the node between them differs:

```scala
case Inject(a: F[A])                 // Free.scala
case Shift (f: (A => S) => R, depth) // Cont.scala
```

`Pure` and `Bind` are the free monad's *skeleton* — returning and
sequencing — and they coincide because neither depends on what an
operation is. The middle constructor **is** the signature. And the
difference between the two is exactly the boundary this chapter opened
with: `Inject` does not mention the continuation, `Shift` receives it.

That is an equation, not a matter of style. An operation is
**algebraic** when it commutes with sequencing:

```
op(...) >>= k   =   op(... >>= k)
```

`Inject` satisfies it by construction — there is nowhere in the node to
put a `k`, because chapter 4's freer design keeps the continuation
*beside* the operation, in `Bind`. `Shift` cannot satisfy it: the
continuation is its argument. Three things follow, and all three are
visible in the library.

**It fixes the arity of the types.** `Free[F, A]` needs one index;
`Cont[A, S, R]` needs three. Answer-type modification (chapter 3) is
the price of seeing the continuation, and only the type that sees it
pays.

**It licenses rewrites.** Because algebraic operations commute with
bind, a program over `Free` may have its operations reordered, batched
or hoisted without changing meaning — the equation *is* the permission.
Over `Cont` none of that is sound. So the choice of node is not
taxonomy: it is a static marker of which transformations a program
admits.

**And it places Okay's two machines.** Programs are `Free` (`A ! F`);
handlers are `Cont` (`F !> S`, that is `F ==> ([X] =>> X /> S)`), and
`foldCont` sends the first into the second. The formal ground is
Filinski's: any monad can be represented in a language with first-class
delimited continuations \[[Filinski 1994](#ref-filinski-1994)\], which
is why the effect layer can rest on the control layer rather than
beside it. That handlers and delimited control are interdefinable is
Kammar, Lindley and Oury's \[[2013](#ref-kammar-2013)\]; the sharpest
comparison of the three ways to expose user-defined effects is Forster,
Kammar, Lindley and Pretnar's \[[2017](#ref-forster-2017)\].

## Scoped operations, and the kind that rules them out

Some operations refuse to be algebraic for a reason no encoding can
argue away: they take a *computation* as an argument, not just a
continuation. `catch`, `local`, `bracket`, `once` are the standard
examples, and handler order changes their meaning — the subject of
Wu, Schrijvers and Hinze's "effect handlers in scope"
\[[2014](#ref-wu-2014)\] and, with a semantics, Piróg, Schrijvers, Wu
and Jaskelioff's \[[2018](#ref-pirog-2018)\].

Two nodes in this library do carry computations:

```scala
case Fork(prog: Unit ! Op)            extends Op[Fiber]  // Sim.scala
case OrElse[A](a: A ! Tx, b: A ! Tx)  extends Tx[A]      // Stm.scala
```

The literature's hazard is that a handler relaying the row underneath
such a node cannot see inside it, so an operation hidden in the payload
escapes the handler that was supposed to remove it. Okay's `relay`
would indeed miss it: it walks the *spine*, and re-injects a foreign
node unchanged without descending into its payload.

It cannot happen here, and the reason is the **kind** of a signature.
`Free[F[+_], A]` takes a first-order `F`, so a signature has no way to
name the ambient row inside its own nodes: writing
`Fork(prog: Unit ! (Op + G))` would need a `G` that the declaration of
`Op` cannot mention. A payload can therefore only be closed over its
own signature — exactly what the two nodes above show — and a closed
payload cannot smuggle a foreign operation past a handler. The scoped
hazard is ruled out by the kind, not by a convention anyone must
remember.

The same kind is what limits them: you cannot fork a program that also
logs, because `Unit ! (Op + Writer % String)` is unspeakable there.
Lifting that restriction means going to **higher-order signatures** —
`F[M[+_], +A]` instead of `F[+A]`, with an operation that rewrites the
nested computations when a handler relays the row beneath them, which
is what the scoped-effects papers above construct. That is a change to
the kind of every signature in the system, and it is not made here
because no consumer has asked for it. The two comments at the nodes say
so, so that the next person to want it learns the price before paying
it by accident.

## Rows as unions, and the trusted kernel

Okay's effect row is a *type-level union of signatures*:

```scala
A ! (State % Int + Throws % String + Async)
```

`+` is genuine union, not a coproduct functor — which is why `Pure`,
the empty row, can be `Nothing` (`Effects.scala:34`): the union with
nothing added is the row itself, and a pure program `A ! Pure`
coerces into any row for free by covariance (`F[Nothing] <: F[X]`,
noted at `Effects.scala:213`).

A handler for `F` inside a row `F + G` must *split* the union: given an
operation, is it mine or the residue's? That is `<|>`
(`Effects.scala:231`), and it rests on `TypeableK[F]`
(`Effects.scala:157`) — a runtime class test for "is this value an
`F`-operation". The split's soundness argument is written where the one
cast lives (`Effects.scala:234`): *the trusted kernel, sound by the
excluded middle of the union* — a value of type `F[A] | G[A]` that the
`F`-test rejects **is** a `G[A]`, provided the two signatures' erasures
are distinguishable. That proviso is a real obligation, not a
formality: the Writer story below exists because an identity-encoded
signature erased to *its element's* class and could collide with
anything.

## Three shapes of handler, one line

`Effects.scala:424–439` states the design in a comment worth quoting
almost whole. With `F ==> H` meaning a natural transformation from the
signature to a carrier:

| shape | meaning | limits |
|---|---|---|
| `F ==> Id` — `Handler[F]` | comonadic: every operation is *answered on the spot* | cannot suspend — "Id is exactly where a suspension cannot go", so no I/O on a platform with no thread to park |
| `F ==> ([X] =>> X ! G)` — `!.translate` | tail-resumptive: answer with *more program* in another row | one walk, no `Cont`; cannot abort or resume twice |
| `F !> S = F ==> ([X] =>> X /> S)` | Cont-valued: the operation receives its delimited continuation | full Plotkin–Pretnar power — abort, multi-shot — at the price of going through `Cont` |

This is chapter 2's theorem specialized twice: the Cont-valued form is
the general handler Filinski's theorem promises; the other two are the
fast degenerate points where the continuation is used exactly once and
immediately, and Okay makes them separate constructs *because they cost
less* — `translate` forwards `G` in a single tail-recursive walk, and
`Handler[F]` is what `runWith` consumes. Choosing the weakest shape
that suffices is the library's standing advice, and the platform
constraint enforces it once: on JS there is no `CanBlock`, so a
comonadic `Handler[Async]` does not exist there *by type*, and code
must peel to `Async.runAsync` instead (`cross-platform-async.md`).

## Lowering: how a program becomes its meaning

The third handler shape needs the operation's continuation, and a
program does not hand one over: chapter 4's freer tree keeps the
continuation *beside* the operation, in `Bind`, as data. `Cont` is the
type whose whole subject is continuations. Interpreting a program is
therefore a move between the two, and `foldCont` is that move.

It is worth being exact about its status, because it is easy to read it
as an optimization. `Free[F, *]` is the **free** monad on the signature:
given an interpretation of `F` in any other monad, there is exactly one
structure-preserving map out of it. `foldCont` is that unique map,
instantiated at `Cont` with the handler as the interpretation. That is
why `Effects.scala` says a computation's *meaning* is its image in the
continuation paramonad, and why `handle` and `runWith` are defined
through it rather than beside it. The lowering is the semantics; the
fast paths below are what the library does when it can prove a cheaper
route agrees.

### The mechanism, line by line

```scala
override def foldCont[S](h: F !> S): A /> S =
  m.fold(Cont.Pure(_))([X] => e => k => h(e).flatMap(k(_).foldCont(h)))
```

Three parts, and each does one thing.

`Free.fold` supplies the *normal form*. Its rotations (chapter 4) mean
that whatever the shape of the tree, what reaches the second argument is
always an operation together with its continuation `k`. A program that
was built left-nested arrives here right-nested; the interpreter never
sees a `Bind` of a `Bind`.

`h(e)` is the handler's answer for that one operation: a `Cont`, not a
value. This is where the handler may decide to drop the continuation, to
call it once, or to call it many times, and nothing downstream has to
know which.

`k(_).foldCont(h)` lowers *the rest of the program* and binds it after.
So the Free spine is rebuilt as a `Cont` spine, one node at a time, with
the handler's own `Cont` spliced in at each. `Pure` maps to `Cont.Pure`.
Nothing is executed yet: the result is still a description, just one
whose sequencing now lives in the carrier that can talk about
continuations.

### What running it does

`Cont`'s runner is five cases, and the split between them is the whole
story:

```scala
@tailrec final infix def /(k: A => S): R = this match
  case Pure(a)              => k(a)
  case Shift(f, _)          => f(k)
  case Bind(Bind(a, f), g)  => Bind(a, f(_).flatMap(g)) / k   // rotate
  case Bind(Pure(a), f)     => f(a) / k                       // discharge
  case Bind(Shift(s, _), f) => s(f(_)(k))                     // hand over
```

The middle three are a loop: they rebalance left-nests and discharge
pure prefixes, tail-recursively, so the length of a `flatMap` chain
costs no stack. The last line is the handover, where the handler's
`s` finally receives the continuation of everything after it. Note what
this means for stack safety honestly: the *spine* is safe by
construction, and what happens inside a handler is the handler's own
business — one that resumes deep inside its own work nests there, as it
must.

One detail from chapter 4 completes the picture: `Cont.flatMap` fuses
into the `Shift` closure while a depth budget lasts and spills into
`Bind` data past it. Short chains never allocate a node; long ones
become data before the JVM stack could notice. The budget is the dial
between the two encodings, set in one place.

### Forwarding, in the same language

The other half of `handle` shows what an *unhandled* operation looks
like once lowered:

```scala
case Right(e) => shift(k => perform(e).flatMap(k))
```

Read aloud: not mine, so perform it again in the residual row and
continue with the captured continuation. That is exactly the licence
the previous section described. Because the operation is algebraic, it
commutes with what follows, so re-performing it and resuming is the
same program; a scoped node could not be forwarded this way, which is
why the two of them are interpreted by runners that descend into their
payloads instead.

### Why the common case does not go this way

`Free`'s `runWith` overrides the definition:

```scala
/** the same answer as the foldCont definition, in one pass instead of two */
override def runWith(using Handler[F]): A = runFree(m)
```

`runFree` is a five-case tail-recursive loop that walks the tree and
answers each operation on the spot. It is allowed to exist because a
comonadic `Handler[F]` uses each continuation exactly once and
immediately: there is nothing for a reified continuation to buy, so
building one is pure overhead, and the tree can be consumed in one pass
instead of being rebuilt as `Cont` and then run. The comment states the
obligation that comes with any such shortcut, which is that it must
give *the same answer* as the definition.

This is the library's standing pattern rather than a one-off: define by
the general construction, then override where a weaker handler shape
provably suffices. The three-shapes table above is the same idea read
from the other end.

### And where lowering is the identity

```scala
type Eff[F[+_], A] = [S] => F !> S => A /> S
override inline def foldCont[S](h: F !> S): A /> S = m[S](h)
```

In the Church encoding a program *is* its own `foldCont`: it was never a
tree, so there is nothing to walk, and lowering costs nothing because it
already happened at construction. What that buys and what it costs are
symmetric with `Free` — no tree means no stepping, no staged relay, and
no stack safety on a left-nested `flatMap`. `fromFree` and `reify` move
a program between the two, so the choice is not made once for the whole
library.

## The worked example: Writer's six encodings

`docs/existentials.md` is this chapter's laboratory. The Writer effect
wants a free `tell` — no allocation per emitted element — which the
freer carrier permits via an identity signature. The price surfaced as
one unprovable equation (the operation's answer type), and six
encodings were tried against it: naming the existential (two forms of
[the mapK newtype]), minted `=:=` evidence, an intersection type, a
match type as the alias, a match type inside the alias. Five failed —
four for the same mechanical reason, *erasure follows the
representation* — and the bytecode of each failure is in that page. The
sixth, a one-constructor GADT (`case Say(w: W) extends Writer[W, Unit]`),
succeeded by giving up the identity representation, and the measurement
that authorized it (no detectable cost on the real benchmark) closed
the question. Read it as this chapter's moral told concretely: in an
extensible-effects system the *signature's runtime representation* is
part of the semantics, because the row split and the handler's
refinement both read it.

## References

- <a id="ref-plotkin-2002"></a>Gordon Plotkin, John Power. *[Notions of computation determine
  monads.](https://doi.org/10.1007/3-540-45931-6_24)* FoSSaCS 2002.
- <a id="ref-plotkin-2003"></a>Gordon Plotkin, John Power. *[Algebraic operations and generic
  effects.](https://doi.org/10.1023/A:1023064908962)* Applied Categorical Structures 11(1):69–94, 2003.
- <a id="ref-plotkin-2009"></a>Gordon Plotkin, Matija Pretnar. *[Handlers of algebraic effects.](https://doi.org/10.1007/978-3-642-00590-9_7)*
  ESOP 2009.
- <a id="ref-kiselyov-2013"></a>Oleg Kiselyov, Amr Sabry, Cameron Swords. *[Extensible effects: an
  alternative to monad transformers.](https://okmij.org/ftp/Haskell/extensible/exteff.pdf)* Haskell Symposium 2013.
- <a id="ref-filinski-1994"></a>Andrzej Filinski. *[Representing
  monads.](https://doi.org/10.1145/174675.178047)* POPL 1994.
- <a id="ref-kammar-2013"></a>Ohad Kammar, Sam Lindley, Nicolas Oury. *[Handlers in
  action.](https://doi.org/10.1145/2500365.2500590)* ICFP 2013.
- <a id="ref-forster-2017"></a>Yannick Forster, Ohad Kammar, Sam Lindley, Matija Pretnar. *[On the
  expressive power of user-defined effects: effect handlers, monadic reflection,
  delimited control.](https://doi.org/10.1145/3110257)* ICFP 2017.
- <a id="ref-wu-2014"></a>Nicolas Wu, Tom Schrijvers, Ralf Hinze. *[Effect handlers in
  scope.](https://doi.org/10.1145/2633357.2633358)* Haskell Symposium 2014.
- <a id="ref-pirog-2018"></a>Maciej Piróg, Tom Schrijvers, Nicolas Wu, Mauro Jaskelioff.
  *[Syntax and semantics for operations with
  scopes.](https://doi.org/10.1145/3209108.3209166)* LICS 2018.
- <a id="ref-kiselyov-2015"></a>Oleg Kiselyov, Hiromi Ishii. *[Freer monads, more extensible
  effects.](https://okmij.org/ftp/Haskell/extensible/more.pdf)* Haskell Symposium 2015.

---

← [4 · Free and freer](04-free-freer.md) · [Contents](index.md) · [6 · Final tagless and staging](06-tagless-staging.md) →
