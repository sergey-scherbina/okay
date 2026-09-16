# 2. Continuations and delimited control

## The continuation, and why undelimited is not enough

A continuation is "the rest of the computation", reified as a function.
The classical control operator `call/cc` captures *all* of it — from
here to the end of the program — which makes it both powerful and
unusable compositionally: a captured continuation never returns, so
there is no answer to hand back and no way to run a captured piece
*inside* a larger program.

Delimited control fixes this by marking where "the rest" stops.
Matthias Felleisen introduced the **prompt** \[[Felleisen 1988](#ref-felleisen-1988)\]: a
delimiter installed on the stack, and a control operator capturing only
up to it. The captured piece is then an ordinary function — it returns
— so captured continuations compose. Olivier Danvy and Andrzej Filinski
gave the now-standard pair **shift/reset** and, crucially, its typing
discipline \[[Danvy & Filinski 1990](#ref-danvy-1990); [the typing with answer types is in
their 1989 report, and its polymorphic account is Asai & Kameyama
2007](#ref-asai-2007)\]: `reset` delimits, `shift f` captures the continuation `k` up to
the nearest `reset` and runs `f(k)` in its place.

The reason this chapter comes before everything else is Andrzej
Filinski's theorem \[[Filinski 1994](#ref-filinski-1994)\]: **shift and reset can express any
monadic effect.** State, exceptions, nondeterminism — each is a
particular way of invoking (or not invoking, or twice invoking) the
captured continuation. A language, or a library, that has delimited
control at the bottom does not need to *build in* any other effect; it
needs only to program them. That is Okay's architecture in one
sentence.

## What this is in Okay: `Cont`

```scala
// Cont.scala:39 — capture the current continuation
// (Danvy–Filinski, with answer-type modification)
inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.Shift(f)
// Cont.scala:41 — delimit: run with the identity continuation
inline def reset[A, R](c: A ^ R): R = c / identity
```

`Cont[A, S, R]` *means* `(A => S) => R` — chapter 3 explains the three
parameters — and `A /> R` (`Cont.scala:34`) is its diagonal
`Cont[A, R, R]`, the ordinary continuation monad. The interesting
engineering is that `Cont` is **defunctionalized**: rather than being
the function type it means, it is a data type with one interpreter,
`/`. And since 2026-09-15 that data type is not its own: `Cont` is
the freer tree of chapter 4 at the signature "a function of the
continuation" — `opaque type Rep[A, S, R] = Free[Shift, A]`
(`Cont.scala:146`), a `shift` being a leaf `Inject(f)` and a bind a
`Bind`, with `S` and `R` phantom to the tree and carried by the
facade's signatures alone. Chapter 11 tells that story in full, with
the two attempts to put the answer types on the nodes and why the
compiler refused both. Three consequences, all load-bearing:

**Stack safety.** A directly-encoded continuation monad overflows the
stack on long `flatMap` chains — the classic problem Rúnar Bjarnason
treated for Scala with trampolines \[[Bjarnason 2012](#ref-bjarnason-2012)\]. Okay's answer is
the same normalization move chapter 1 showed for `Free`, because it
*is* `Free`'s: `Bind` is a node, and the runner (`Cont.step`,
`Cont.scala:299`) rebalances left-nested binds in a tail-recursive
loop, the same rotation `Free.resume` performs. A tail call between
two mutually recursive functions is a `Delay` node, forced by that
loop and continued as is (`Cont.delay`, `Cont.scala:173`).

**Absorption, exactly once.** Pure defunctionalization pays a node per
bind. A fresh leaf therefore *absorbs* its first `flatMap` into
itself — `Inject(Leaf.Absorbed(s, f))`, the function `k => s(a =>
run(f(a))(k))` (`Cont.scala:201–236`) — and a leaf that has absorbed
once takes the next bind as a node. This used to be a depth budget of
128; the sweep that replaced it (`fuse-depth`, 2026-09-15) found the
first step to be the whole of the 12–25% win and every deeper step a
cost, so the budget is a bit, and the bit is the leaf's class.

**Nothing to convert.** A handler's answer is a `Cont`; a program is
a `Free`; lowering one into the other (chapter 5) replaces leaves and
keeps the spine, because the spine is the same class of node on both
sides.

## Prompts as an effect: `Delim`

`shift`/`reset` capture to the *nearest* delimiter. Multi-prompt
control — capture to a *named* delimiter, possibly across intervening
ones — is strictly more expressive, and Okay implements it in the
shape of Dybvig, Peyton Jones and Sabry's monadic framework \[[Dybvig,
Peyton Jones & Sabry 2007](#ref-dybvig-2007)\]: a **prompt is a first-class tag carrying
the delimiter's answer type** (`Delim.scala:74`), `push` installs one,
and `shift(p)` captures up to the prompt `p` (`Delim.scala:90`).

Two decisions are documented in `Delim.scala`'s header because both
were "arrived at the hard way", and they are worth restating as
theory-meets-implementation:

- **`push` is an operation, not a handler application.** Capturing
  *across* an intervening delimiter is the point of multi-prompt, and
  nested handlers cannot do it — an inner handler forwarding a shift
  it does not own forwards it opaquely, leaving its own frames out of
  the captured continuation. One machine must own the whole prompt
  stack, so both `push` and `shift` reach it as operations of a single
  `Delim` signature.
- **Tags are what let several answer types share one row.** Okay's
  union splitting is by runtime class (chapter 5); a signature
  parameterised by its answer type would erase two different prompts
  to the same class. With the answer type riding inside the tag, one
  `Delim` signature suffices and tags keep prompts apart.

The generators are the everyday face of this machinery: `Generate.scala`
builds `LazyList`, `Producer` and `Teller` from one delimited-control
unfold (`Loop`/`take`/`put`), which is `shift` capturing "the rest of
the enumeration" at each element.

## Why Cont is the bottom of the tower

Filinski's theorem says delimited control *suffices*; Okay's
`Effects.scala` makes it the actual foundation: a Cont-valued handler
`F !> S = F ==> ([X] =>> X /> S)` (`Effects.scala:120`) interprets each
operation as a continuation-manipulating program, and the comment at
`Effects.scala:737–742` places the three handler shapes — comonadic
`F ==> Id`, translating `F ==> ([X] =>> X ! G)`, and Cont-valued —
"on one line". The first two are the degenerate points (never touch
the continuation; touch it only to re-inject), the third is the
general case, and it is general *because* of Filinski. Chapter 5 walks
the three shapes; the point here is that they are not three features
but one theorem, specialized twice.

## `shift` in a direct block: `Cont.direct`

`Delim.shift` captures under a handler, in a row. The bare paramonad
has the same word, and since cont-in-direct (2026-09-17) it can be
written in a direct block too:

```scala
import okay.Cont.direct.*

type Str = [X] =>> Cont[X, String, String]        // the diagonal at String

val c: String /> String = direct[Str]:
  val x: String = !shift[String](k => k("one") + " " + k("two"))
  "<" + x + ">"

reset(c)    // "<one> <two>" — the rest of the block ran once per k
```

Two things are worth naming.

**One type argument.** `shift[A]` names the captured value's type and
nothing else; the answer type comes from the block, through an
`AnswerOf[F]` witness that also re-associates `Cont[A, R, R]` to `F[A]`
by *typing* it, so the convenience costs no cast. This is the same
trick as `Delim.shift[A]`, whose answer type comes from its `Prompted`
evidence.

**Its own scope, not an overload.** Making the package-level `shift`
take one argument was tried and refused by measurement: a call with no
type arguments — `shift: k => ...`, the shape every handler in this
library writes, `runChoice` included — then resolves to the
one-argument alternative and fails for want of a `DirectCtx`. The
import is the opt-in, and it is why `import` had to become a statement
the macro passes through.

What is *not* spellable here is a block that MOVES the answer type, and
not for want of a name: a direct block is diagonal — one `F[A]` for the
whole block — while answer-type modification gives every step its own
`F`. That shape stays in `for`, with the expected type on the `reset`
(the fourth worked example below).

## Six worked examples

`TestDelimExamples` runs these, so they are checked rather than
claimed. Each is a shape the literature uses to argue that first-class
continuations earn their keep.

**Reverse-mode automatic differentiation** \[[Wang & Rompf 2018](#ref-wang-2018)\].
The forward pass is what you write; the backward pass is what the
continuation does on the way out. `times(a, b)` captures, builds the
result, runs the rest of the computation through `k`, and only then
accumulates the adjoints — no tape and no graph, because the tape IS
the continuation. Checked against the analytic derivative of
`x*x + 3x` at three points.

**A generator.** A recursive tree walk that `yield`s, read by the
caller as a sequence. The walk is ordinary recursion and nothing is
inverted: `yieldOne` captures the rest of the walk and conses onto it.
The strict version is what compiles cleanly here — a lazy one wants
the tail to be a suspended RUN of the continuation, and a mark under a
by-name argument is refused by the direct macro on purpose, since
hoisting it would change when it evaluates.

**A web dialogue** \[[Queinnec 2000](#ref-queinnec-2000)\]. The program
asks a question and the rest of the dialogue is kept as a value until
the answer arrives — no state machine, no session object. The test
answers the SAME start page twice, with different answers, which is
the point: the dialogue is a value, so it can be resumed more than
once.

**Answer-type modification** \[[Danvy & Filinski 1990](#ref-danvy-1990)\].
The block produces an `Int` and the delimiter answers a `String`.
`Cont[A, S, R]` carries that in its type; a plain monad cannot say it.

**Functional unparsing** \[[Danvy 1998](#ref-danvy-1998)\]. A format is
a value, built from directives, and the TYPE of `sprintf` is computed
from it: `str(lit(" is ")(int(done)))` has type `String => Int =>
String`, which nobody wrote down. No macro, no format-string parsing,
no varargs, and the arity and the argument types are checked — too few
arguments, or an `int` directive fed a `String`, do not compile. This
one is written in plain CPS, as the baseline the next example has to
earn its keep against.

**printf through shift/reset** \[[Asai 2007](#ref-asai-2007)\]. The same
thing, with the plumbing removed: each directive is a `shift` that
MOVES the answer type — `str` turns "the delimiter answers `T`" into
"it answers `String => T`" — and the format is their composition, so
it can be a `for`-comprehension with nothing annotated inside it:

```scala
def lit[T](s: String): Cont[String, T, T] = shift(k => k(s))
def str[T]: Cont[String, T, String => T] = shift(k => (x: String) => k(x))
def int[T]: Cont[String, T, Int => T] = shift(k => (n: Int) => k(n.toString))

val greeting: String => Int => String = reset[String, Out]:
  for
    x <- lit("Hello, ")
    y <- str
    z <- lit(" is ")
    w <- int
  yield x + y + z + w + " years old"
```

The expected type on `reset` carries the whole chain. And this is the
sharpest statement of the boundary above: those directives are exactly
what `Cont.direct` cannot express, because `S ≠ R` at every step —
a test asserts that `AnswerOf` has no instance for them.

## References

- <a id="ref-wang-2018"></a>Fei Wang and Tiark Rompf.
  *A Language and Compiler View on Differentiable Programming.* ICLR
  Workshop, 2018 ("Demystifying Differentiable Programming").
- <a id="ref-queinnec-2000"></a>Christian Queinnec.
  *The influence of browsers on evaluators, or continuations to program
  web servers.* ICFP 2000.

- <a id="ref-felleisen-1988"></a>Matthias Felleisen. *[The theory and practice of first-class
  prompts.](https://doi.org/10.1145/73560.73576)* POPL 1988.
- <a id="ref-danvy-1989"></a>Olivier Danvy, Andrzej Filinski. *A functional abstraction of typed
  contexts.* DIKU report 89/12, 1989.
- <a id="ref-danvy-1998"></a>Olivier Danvy. *[Functional
  unparsing.](https://doi.org/10.1017/S0956796898003104)* JFP
  8(6):621-625, 1998.
- <a id="ref-danvy-1990"></a>Olivier Danvy, Andrzej Filinski. *[Abstracting control.](https://doi.org/10.1145/91556.91622)* LISP and
  Functional Programming 1990.
- <a id="ref-filinski-1994"></a>Andrzej Filinski. *[Representing monads.](https://doi.org/10.1145/174675.178047)* POPL 1994.
- <a id="ref-asai-2007"></a>Kenichi Asai, Yukiyoshi Kameyama. *[Polymorphic delimited
  continuations.](https://doi.org/10.1007/978-3-540-76637-7_16)* APLAS 2007.
- <a id="ref-dybvig-2007"></a>R. Kent Dybvig, Simon Peyton Jones, Amr Sabry. *[A monadic framework
  for delimited continuations.](https://www.cs.indiana.edu/~dyb/pubs/monadicDC.pdf)* JFP 17(6):687–730, 2007.
- <a id="ref-bjarnason-2012"></a>Rúnar Bjarnason. *[Stackless Scala with free monads.](http://blog.higher-order.com/assets/trampolines.pdf)* 2012.

---

← [1 · Monads and functors](01-monads.md) · [Contents](index.md) · [3 · Parameterised monads](03-parameterised.md) →

*Shipped consumers of this chapter (2026-09-01):* `Delim` carries
Dialog's cancellable scopes (`Scope`), the streaming cut (`Cut`),
the agent stepper's pause-and-fork, and the sim scheduler's
captured-continuation feeding — the operator's adoption doctrine
(specs/delimited-control.md, Adoption) states when it is the
mechanism and when an option.
