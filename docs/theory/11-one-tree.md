# 11. One tree: `Cont` as a facade over `Free`

*Why the continuation monad and the effect tree are the same data,
what that bought, what it cost, and how it was found — the arc of
2026-09-14 to 2026-09-16, told as theory first and as measurements
second.*

Chapters 2 and 4 described two defunctionalized enums with the same
two moves each — a node per bind, a tail-recursive runner that
rebalances left-nested binds — and said the pair was "one design at
two points". That was true and it was also a smell: two enums, two
rotation loops, and a growing list of *copies* of the rotation
(five, by the count of 2026-09-15) each documented as "the same as
the others". This chapter is about the day that smell was resolved.
The result is one enum, one rotation, and a sentence: **`Cont` is
`Free` with a function in the leaf and its answer types on the
outside.** Everything the effect layer does — lowering, handling,
forwarding, trampolining — now happens on one set of nodes.

## The theory: a shift is a leaf

Recall the two types as chapters 2 and 4 left them.

`Free[F, A]` is the *freer* monad \[[Kiselyov & Ishii
2015](#ref-kiselyov-2015)\]: a tree whose leaves are operations `F[A]`
and whose `Bind` keeps the continuation as a plain function,
`Bind(a: Free[F, X], f: X => Free[F, B])`. No `Functor F` is needed
because the continuation is *data beside* the operation rather than
mapped *into* it.

`Cont[A, S, R]` *means* `(A => S) => R` — Danvy and Filinski's
delimited control with answer-type modification \[[Danvy & Filinski
1990](#ref-danvy-filinski-1990)\], parameterised in Atkey's sense
(chapter 3). Defunctionalized, it was `Pure(a) | Shift(f) | Bind(m, k)`
with its own runner `/`.

Put the two definitions side by side and the second is an instance of
the first. A `Shift(f)` is a leaf whose payload is *a function of the
continuation*. A `Bind` is a `Bind`. `Pure` is `Pure`. So take for the
signature `F` of a freer tree the type constructor "a function of the
continuation":

```scala
// Cont.scala — the representation: the freer tree at Cont's own signature
opaque type Rep[A, S, R] = Freer[Sig, S, R, A]
```

(Until 2026-10-01 the signature was `Shift = [S, R, X] =>> (X => S) =>
R`, the shift body itself as the leaf, run by a runner of Cont's own.
Since cont-on-frames Cont runs on the same frame machine as `Shift` —
chapter 2's `shift0`/`$` calculus — and since cont-run-prompt
(2026-10-03) it is `Sig = Cont.Op`: a leaf is Cont's own operation,
`Op[S, R, A]`, typed as the leaf it is. Since cont-atm (2026-10-03) it
runs on `Delimited`, the machine every effect shares, as an effect with
answer-type modification typed on the machine's ANSWER boundaries — no
claim at all (specs/cont-atm.md). What follows holds of every stage: the
leaf is a function of the continuation either way.)

and `Cont` **is** `Freer[Sig, S, R, A]`, the same enum `Free[F, A]`
is (`Freer[Unary[F], Unit, Unit, A]`). There is nothing to convert
between them, because there is nothing between them. `Cont.Pure` is
`Free.Return` (`Cont.scala:151`); `Cont.shift(f)` is `Free.Inject`
of the leaf (`Cont.scala:155`); `flatMap` is `Free.Bind`, with one
refinement below. Read the other way, `Free` is `Cont` whose shift
body is chosen by the *handler* rather than by the program — which is
exactly what chapter 5's lowering, `foldCont`, does: it replaces each
operation leaf `Inject(e)` by the handler's `h(e)`, a shift, and the
`Bind` spine stays the spine. The lowering that chapter 5 described
as "rebuilding the Free spine as a Cont spine" turns out to rebuild
nothing: the spine is the same class of node before and after.

This is Filinski's theorem \[[Filinski 1994](#ref-filinski-1994)\]
showing up in the data: delimited control suffices for every monadic
effect, so the *tree of a program* need contain nothing a
continuation-manipulating leaf cannot express, and the same tree can
serve both.

## Where the answer types live, and what it took to put them there

The obvious objection: `Cont[A, S, R]` has three type parameters and
`Free[F, A]` has one that is not `F`. Where are `S` and `R`?

**On the nodes, since 2026-09-29** (freer-base-step-extractor). The
one enum is `Freer[G, S, R, A]`: `Return(a)` is a `Freer[G, R, R, A]`,
a leaf `Inject(a: G[S, R, A])`, and `Bind(a: Freer[G, T, R, A], f: A
=> Freer[G, S, T, B])` is a `Freer[G, S, R, B]` — a left side answering
`T => R` joined to a continuation answering `S => T`, which is answer-
type modification written on the node. `Cont` was that enum at `Shift
= [S, R, X] =>> (X => S) => R` until 2026-10-01: the leaf the shift body
at its own type, and its own runner typed by the GADT end to end. Since
cont-on-frames it was that enum at the λ$ machine's row, run by the frame
machine, the facade's `Rep[A, S, R]` making one claim at its boundary;
since cont-atm it is that enum at Cont's own `Op`, run by `Delimited`,
whose answer boundaries carry the answer types the claim stood for
(specs/cont-atm.md). `Free[F, A]` is the same enum at `Unary[F]` —
`F` on the diagonal of the two indexes, nothing off it — with both
fixed at `Unit` (one-bridge, 2026-10-04: the bridge indexed rows use
too; until then `Lift[F]`, which ignored the indexes).

It was not always so, and the two refusals on the way are why the
shape is what it is (specs/freer-base.md, stages 0 and 1):

- **On the leaf's signature** — `Free[[X] =>> (X => S) => R, A]`.
  A freer `Bind` joins a left tree and a continuation over ONE `F`.
  Answer-type modification joins a left `(X => S) => R` with a right
  `(X => S2) => S`: two different `F`s under one `Bind`. The only `S`
  at which both sides live under one `F` is `Nothing`, which is an
  erased leaf. That is why the indexes had to go on the *nodes*.
- **On the nodes, matched everywhere** — stage 1 put them there and
  was refuted at the other user of the tree, pattern matching: a
  `Bind` carries its left side's answer index `T`, a match makes it
  existential, and the effect layer's hundred-odd match sites want the
  continuation as `X => Free[F, A]`. The extractor that stage tried
  put the type variable only in `unapply`'s RESULT, which dotty infers
  as `Nothing`. For a year the answer was a facade: the tree unindexed
  (`Free[Shift, A]`, the leaf stored at `(X => Nothing) => Any`), `S`
  and `R` phantom on the signatures, and two casts in the runner
  (`Shift.at`, `pinned`) where the facade's discipline was trusted
  rather than checked.

What closed it is one extractor, `Free.Bind`, whose pattern-bound type
variables sit in its PARAMETER type — `unapply[G, T, R, X, A](b:
Bind[G, Unit, T, R, X, A]): Bind[G, Unit, Unit, R, X, A]` — so the
compiler inserts the type test that binds them, and whose result is
the node itself, a Product, so the match allocates nothing. Its one
cast says a constant thing: a `Lift` tree is built with every index
`Unit`, so the middle index a match forgot is `Unit` too. Every
`case Bind(Inject(e), k)` in the library goes through it unchanged.
The casts moved from two (trusted at two nodes of every `Cont` run)
to one (a constant claim at the effect tree), and `Cont`'s runner is
checked by the compiler again, as the separate `Cont` enum's was
before the trees were one.

Two details are load-bearing and worth knowing. The value type `A`
comes LAST in `Freer[G, S, R, A]`, because a unary constructor
inferred from a program value — `Monad[M]` from an `A ! F` — is the
type abstracted over its last parameter, and `[A] =>> Free[F, A]` is
what every instance is written for. And `Unary[F]` is a projection on
a class, `Diagonal[F]#L`, not a bare type lambda: applied to a row
`Users + F` a lambda beta-reduces to a union when two are compared,
and a union has no structure to solve `F1 + G` from, where the
projection compares by its prefix and the row's `+` is matched
application to application. Both were found by the compiler saying
so, not by design.

So the principle that settled stage 1 stands, sharpened: **the tree is
syntax; an index is a claim about syntax; a claim the tree can carry
belongs on the tree, and the one place a match cannot see it is
answered once, by name.** An indexed program for a protocol (`Prog`)
is still an opaque facade over `Free[F, A]`: its indexes say nothing
the nodes could check, so they stay where nobody matches them.

## The one refinement: absorption, exactly once

Pure defunctionalization pays a node per bind, and chapter 2 described
`Cont`'s answer as closure fusion under a depth budget of 128. That
budget was re-measured on the way here (`fuse-bench`, `fuse-depth`,
2026-09-15): fusion pays 12–25% on the Fib lanes — real — and **one
step is the whole of it**: `fuse=1` matched `fuse=128` on every lane,
and deeper absorption *costs*, because each further step nests one
more closure call per run (`statePara` 0.861 at depth 1 against
1.15–1.19 deeper).

So the budget became a bit, and the bit became a class. A fresh leaf
absorbs the first `flatMap` into itself — `Inject(Leaf.Absorbed(s,
f))`, the function `k => s(a => run(f(a))(k))` — and a leaf that has
absorbed once takes the next bind as a `Bind` node like any other
(`Cont.scala:201–236`). `map` has its own case, `Mapped`, because
spelling it as `Absorbed` over `a => Pure(f(a))` allocated a `Pure`
per element at run time (+24 B/op, 8–19% on every Fib lane — the
generator maps once per element). `Once` is an enum rather than two
classes because a single `apply` body gives the runner's call one
target the JIT inlines: worth 3.2–4.5% on every Fib lane against two
classes with two bodies, while making the same site merely bimorphic
was worth nothing — the JIT counts call *targets*, not receiver
types. `Free`'s own `flatMap` stays a plain `Bind`: effect programs
are inspected by handlers, and a fused closure cannot be split on a
row. That asymmetry is the whole of what distinguishes the two uses
of the tree.

## One rotation, one normal form, one trampoline

With one tree there is one rotation. `Free.resume` (`Free.scala:127`)
is a member of the enum — a member wins resolution, so every `.resume`
across the library reaches that one loop with nothing imported — and
it normalizes any tree to one of three head forms, `Return(a)`,
`Inject(e)`, `Bind(Inject(e), k)`, in constant stack:

```scala
@tailrec final def resume: Freer[G, S, R, A] = this match
  case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)).resume   // associativity
  case Bind(Pure(a), f)    => f(a).resume                       // left identity
  case Delay(t)            => t().resume                        // force, continue AS IS
  case Bind(Delay(t), g)   => Bind(t(), g).resume
  case a                   => a
```

Every interpreter in the library is a three-case match over what
`resume` leaves — `Free.fold`, `runFree`, `relay`, `handle`, the
stream walkers, `Async`'s loop — where there used to be five copies of
the rotation itself. `Cont.step` (`Cont.scala:299`) keeps the one
copy that composes through `bind`, so a rotated continuation can be
absorbed by the leaf it lands on; delegating it to `resume` was
measured and lost 6% on `statePara`.

The fourth case is the new one. `Delay(thunk)` is a deferred
subprogram with no continuation of its own: the interpreter forces
the thunk and continues with whatever it returns, composing nothing.
It is what `!.tailcall` builds, what `handle` reifies the rest of a
program into when a handler *captures* its continuation, and what the
codecs' trampolines past `NativeThreshold` build for a call with
nothing to do afterwards. Before it existed a tail call was `Defer(t,
pure)` — "defer, then wrap the answer in `Pure`" — and that innocent
spelling was a quadratic-looking tax in linear clothing: `Defer(t,
pure)` resumes to `Bind(t(), pure)`, and when `t()` is itself a
`Bind` the associativity rule pushes a `.flatMap(pure)` tail down
*every* bind of the deferred subprogram — one closure and one `Bind`
per bind, then a chain of `Bind(Pure(a), g)` of the same length when
the answer arrives. The tree is not the cost; the *shape* the tree is
put into is. With `Delay` there is nothing to push.

And once `Delay` existed, `Defer(t, f)` was derivable — it is
`Bind(Delay(t), f)` and the runner treated it identically — so it
went, and the tree is four cases: `Return | Inject | Bind | Delay`.

## The practice: what it measured

The whole arc was landed under one rule: **an argument is not
evidence**. Every step has a benchmark row in `src/jmh/history.tsv`
and a section in a spec; the table below is the shape of it.

| step | lane | before | after |
|---|---|---|---|
| `Cont` as a facade over `Free` (2026-09-15) | every core lane | — | within ±1%, allocation identical to the byte |
| `Once` as an enum | fib10/50/100/1000 | — | 0.955–0.968 (the JIT's one call target) |
| `handle` forwards on the tree, enters `Cont` only for a claimed op | `handlePrebuilt` | 223.3 µs, 2 869 306 B | **154.2 µs, 1 753 945 B** — the bytes `relay` allocates, to the digit |
| `Delay` for tail calls | `tailcallChain` (10 000 hops) | 127.7 µs, 1 359 969 B | **24.1 µs, 400 016 B** (one 40 B node per hop) |
| `Delay` for a capturing handler | `handleCapture` (100 captures over 10k ops) | 214.1 µs | **159.1 µs** — what `handlePrebuilt` costs; a capture is free in time |
| `Defer` removed, `resume` 495 → 323 bytes | `relayPrebuilt` | 151 µs | **142 µs** |
| the same, before `handle`'s cold arms moved out | `handlePrebuilt` | 154 µs | 177 µs (+15%, same bytes) |
| … and after | `handlePrebuilt` / `handleCapture` | 154 / 152 µs | **145.5 / 146.7 µs** |
| the price, recorded | `parseDeep` (a JSON document 2 000 deep, the codec trampoline) | 62.0 µs, 827 973 B | 65.1 µs, 906 948 B (+40 B per deferred level) |

Two of those rows are the practical lesson of the arc, and they are
about the JIT rather than the tree. Shrinking `resume` from 495 to
323 bytes put it under HotSpot's `FreqInlineSize` of 325, and it
began inlining into every loop that called it: `relay`'s 244-byte
loop gained 6%, `handle`'s 388-byte loop — already "hot method too
big" — lost 15% with a 323-byte loop pasted into it. The fix was a
move that had been tried the same morning and measured as *nothing*:
extracting `handle`'s terminal case and capturing fallback into their
own methods. It measured nothing then because nothing was inlining
into that loop yet; the same shape became the fix once `resume` fit.
The rule this leaves — a callee crossing the inlining line re-decides
every caller, and a caller-side shape that was neutral can become the
fix — is the fourth face of a rule that had three by lunchtime, and
it is why every change to `resume`'s size in this repository now
re-measures `relay` *and* `handle`. The JIT, not the algebra, is
where the last 10% lives.

## What it is better than, and what it costs

Better, concretely:

- **One concept where there were two.** A reader who understands the
  freer tree understands `Cont`; a reader who understands `Cont`
  understands why `foldCont` is a natural transformation that changes
  leaves and keeps the spine. The chapter-5 story "lowering rebuilds
  the spine in another carrier" was a description of a copy that no
  longer happens.
- **One rotation, one normal form, one trampoline**, shared by effect
  programs, delimited control, the streams, the codecs and the direct
  block's deep recursion (chapter 8). Five copies of the rotation
  became two, and the second is there for a measured reason.
- **Fewer bytes, not more.** `Cont.Pure` *is* `Free.Return`; a handler's
  answer is a node the tree already has; a captured continuation is
  one `Delay`. The numbers above are the numbers.
- **The typestate road stays open.** Stage 2 of the same spec — a
  protocol state on an effect program — is one more facade over the
  same tree, and the leak that killed the indexed enum cannot reach a
  facade.

The costs, stated as plainly:

- **A trust boundary.** `S` and `R` are not checked by the compiler
  on the tree; they are checked on the facade's signatures and
  trusted at `Shift.at` and at the runner's `Pure` case. Two casts,
  each in one function with one comment, is the operator's rule
  satisfied to the letter — and it is still two places where a
  wrong facade signature would become a `ClassCastException` rather
  than a type error. The facade is small and closed, which is what
  makes the claim checkable by reading.
- **A node per deferred call with a continuation.** `defer(t)(f)` is
  `Bind(Delay(t), f)` now, two objects where `Defer` was one: +40 B
  per level on the codec road past `NativeThreshold`, +5% on
  `parseDeep`, and nothing anywhere else.
- **A shape-sensitive loop.** `resume` is two bytes under the
  inlining line. A case added to it flips every caller back to "too
  big"; a walker converted to `split` may cross the line itself. The
  memory of that is in the CHANGELOG and in the loop's own comment,
  and the discipline is `-XX:+PrintInlining` before any change to it.

## What went with it

The arc removed more than it added, and the removals are part of the
design rather than tidying.

- **`Eff`, the Church encoding.** It proved the interface honestly
  tagless ("Free and Eff agree") and that a Church program can be
  stack-safe; both facts are in the specs with their numbers, and
  nothing outside its own tests ever built one. The "no-tree road"
  was measured at 0.58–0.86x of the fused tree loop before it went.
  `Eager` remains as the second `Effects` instance.
- **`Defer`**, derived as above. **`typeableKByClass`, `fromFree`,
  `foldIn`/`runIn`, both `Free.run`**, none with a caller.
  **`TypeableK.unapply`**: the extractor form of the row test, matched
  by nobody — `test` is the whole interface, and two casts went with
  it.
- **`!.Effect`**, the alias of `Free.Inject` kept "so the match sites
  would not move". It collided with `okay.Effect`, the `derives`
  marker, and the node's name is `Inject` everywhere now.
- **`<|>` as a second cast site.** It is `split` at `Left`/`Right`;
  the union's two casts live in `split` alone, and `<|>` remains the
  `Either` form for drains and tests, where a `match` reads better
  than two lambdas and the wrapper is scalar-replaced anyway
  (measured byte-identical on every such walker).

## References

- <a id="ref-kiselyov-2015"></a>Oleg Kiselyov, Hiromi Ishii. *[Freer
  monads, more extensible
  effects](https://okmij.org/ftp/Haskell/extensible/more.pdf)*.
  Haskell Symposium 2015.
- <a id="ref-danvy-filinski-1990"></a>Olivier Danvy, Andrzej Filinski.
  *Abstracting control*. LFP 1990. — shift/reset with answer-type
  modification.
- <a id="ref-filinski-1994"></a>Andrzej Filinski. *Representing
  monads*. POPL 1994. — delimited control suffices for every monadic
  effect, which is why one tree can carry both.
- <a id="ref-atkey-2009"></a>Robert Atkey. *[Parameterised notions of
  computation](https://bentnib.org/paramnotions-jfp.html)*. JFP 2009.
  — the three-parameter `Cont` the facade's signatures implement.
- <a id="ref-bjarnason-2012"></a>Rúnar Óli Bjarnason. *[Stackless Scala
  with free monads](https://blog.higher-order.com/assets/trampolines.pdf)*.
  Scala Days 2012. — `Delay` is his trampoline's `More`, on the tree
  the effects already use.
- The repository's own record: `specs/freer-base.md` (stages 0–2,
  the refutation and the turn), `specs/core-cleanup.md` (the review,
  `delay-node`, `defer-eff-removal`, `split-over-either`),
  `CHANGELOG.md` entries `fuse-bench` through `either-via-split`, and
  rows `fuse*`, `freer0*`, `once-*`, `cof-*`, `hff-*`, `dn-*`, `de-*`,
  `so-*` in `src/jmh/history.tsv`.
