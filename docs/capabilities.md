# Capabilities — context functions as the wiring of Okay

This page tells the whole story in one place: what a context
function is, the four words of the vocabulary (`doors`, `provide`,
`providing`, `wire`), why they add up to dependency injection with
zero framework, the theory the compiler runs on your behalf, and the
exact boundaries where the mechanism stops. Everything here is
compiled evidence: the claims trace to the numbered experiments
E1–E19 in [specs/context-functions.md](../specs/context-functions.md),
and the closing example runs as `TestShowcase` in okay-obs.

## The mechanism: three facts about `A ?=> B`

A context function `A ?=> B` is a function whose argument is passed
implicitly: the compiler supplies it from the nearest `given A` in
scope. Three mechanical facts, each verified against the compiler,
carry everything that follows.

**A stored `A ?=> B` self-applies where a given `A` is in scope**
(E8). Context functions are first-class values — returnable,
storable in a `val`, passable as arguments — and each one is a
*deferred requirement*: it waits, unapplied, until it lands in a
scope that has its `A`, and there it applies itself. No call syntax,
no `.run`.

**Nested requirements resolve to the NEAREST enclosing scope, with
no ambiguity** (E8). Inner installations shadow outer ones by
nesting depth. This is what makes overriding sound: install a stub
inside a block and every requirement in that block sees the stub;
step out and the production value is back.

**A context-function value auto-applies EAGERLY at most positions**
(E10). Ascribe one, put it in receiver position, pass it where a
plain type is expected — the compiler applies it to the ambient
given then and there. This is the sharpest edge in the design: it
defeats `Conversion`-based bridges and method syntax (the boundaries
section below), and it is also, three separate times, the thing that
makes the pleasant syntax work at all. Positions that expect a
context-function type — parameters declared `A ?=> B`, ascriptions
against a ctx-fn type — hold the value unapplied.

## The vocabulary

Four words. Two install, one consumes, one defers.

### Doors — APIs that accept requirements

A *door* is an API that takes a capability implicitly instead of
threading it by hand — either a wrapper-taking form or a factory:

```scala
// okay-security: the principal is ambient in the protected route
Secure.granted(verify, policy)(route: Principal ?=> PartialFunction[Request, Response ! Async])

// okay-obs: a handler written against `using Tracer`
Traced.route(tracer)(route: Tracer ?=> Traced.Route)

// factories: the environment arrives when the value is installed
def wired(endpoint: String): Http ?=> Engine
```

Adding a door to an existing API is two lines and never breaks the
explicit form:

```scala
def granted(...)(route: Principal ?=> R): R  =  explicit(...)(p => route(using p))
def wired(...): Http ?=> Engine              =  explicit(summon[Http], ...)
```

Doors shipped across the library: `Secure.granted`, `Traced.route`,
`McpAuth.granted`/`discover`/`connect`, `OAuth2.exchange`/`refresh`/
`clientCredentials`, `Jwks.fetch`, `Tls.served`, `Langchain4j.wired`,
`S3.wired`, `Configs.ambient`, the ambient prompts of `Scope` and
`Cut`, and `Blocking[A]` (`= CanBlock ?=> A`) beside `Async`.

### `provide` — install for an expression

`provide(a, b, ...)(body)` installs givens for exactly one
expression — no `given` line, no block nesting:

```scala
provide(prodHttp, Secrets.env) { app }     // the edge
provide(stubHttp, testSecrets) { app }     // the test — same program
```

Nesting resolves to the nearest installation (inner `provide`
shadows outer). Arities 1..22 are generated — the same answer Cats
gives for `mapN`, applied at the platform's own cap
(`ContextFunctionN` ends at 22); `tools/gen_provide.py` regenerates.
`provide` is `inline` and free of allocation — when the flat form
fits, it is the zero-cost spelling.

### `providing` and `and` — environments as values

Where `provide` is an expression, `providing` is a *value*: one
installer, composable with `and`, applied later:

```scala
val base = providing[Http](prodHttp) and providing[Secrets](Secrets.env)

base { app }                                    // install everything, run
(base and providing[Http](stubHttp)) { app }    // override just Http
```

The mechanism (E16) is currying as composition of type
constructors: one installer carries `F[X] = A ?=> X`, and `and`
composes the *constructors* — `F[G[X]] = A ?=> G[X]`, the curried
chain `A ?=> B ?=> X` assembled by values. Three consequences:

- **No nesting and no arity cap.** The 22 limit belongs to the flat
  tuple form; composition is unbounded (25 layers are in the tests).
- **The right operand of `and` is the inner layer**, so it wins
  under nearest-wins: `base and providing[Log](testLog)` *is* the
  override story, written as data.
- **Environments become ordinary values**: build a base once,
  share it, override one layer per test.

Two usage rules, both learned the hard way. Write the type argument
explicitly — `providing[Db](db)` — because inference would pick the
runtime refinement (the anonymous class), and the capability you
install is the trait. And a *conditional layer* does not typecheck
(`if debug then providing[Log](v) else base` — the branch types
differ, since the type grows with each `and`); make the *value*
conditional inside one installer instead:
`providing[Log](if debug then verbose else quiet)`.

### `wire` — the consumer, one line

```scala
inline def wire[A]: A ?=> A = summon[A]
```

`wire[Db].q` pulls the ambient capability by naming its type. The
naive `def wire[T] = summon[T]` does not compile — there is no
given at the definition site; the `A ?=> A` result type is the fix,
turning the definition into a deferred requirement. And here E10's
eagerness works *for* us: in receiver position `wire[Db].q` applies
to the nearest given and moves on; `val d = wire[Db]` lands as a
plain `Db`; a door writes point-free with no `summon` and no
parameter:

```scala
val getQ: Db ?=> String = wire[Db].q
val line: (Db, Log) ?=> String = s"${wire[Log].tag}:${wire[Db].q}"
```

A missing given stays a compile error — the guarantee below.

## The dependency-injection story

Together the four words are a DI container with the container
deleted:

- **The type is the contract.** A consumer declares its needs as
  `using` parameters, as `?=>` in its type, or via `wire[A]` — all
  three are the same declaration read at different positions.
- **Resolution is at compile time.** A missing dependency is a type
  error with the requirement in the message — never a runtime
  container exception. Tests assert this with `compileErrors`.
- **Given-scopes are the object graph.** `provide`/`providing`
  place values; the compiler connects producer to consumer by type;
  nearest-wins gives scoped overriding for free.
- **Modules are ordinary values.** An environment is a `val`; a
  test override is `and` with one more layer; there is nothing to
  configure and no reflection anywhere.

Two design rules keep it honest. **Environment vs. resource**: a
capability should be an *environment* — `Http`, `Secrets`, `Crypto`,
`ChatModel`, `Store`, `Tracer`, `Principal`, `Prompt` — something
one scope shares safely. A per-instance *resource* (a `Connection`,
a socket, a `Resp`) stays an explicit argument, because ambient
resources are how leaks happen. And **no newtypes for strings**: do
not invent `case class Port(n: Int)` just to make something
injectable; capabilities are the types the domain already has.

## The theory the compiler runs

Context functions are the Reader monad — with the compiler as its
interpreter. This is not a metaphor; it is four verified
identities.

**The Monad instance is the identity written four ways** (E13).
Define `Monad[[B] =>> A ?=> B]` and every body collapses:
`pure(b) = b`, `map(fb)(f) = f(fb)`, `flatMap(fb)(f) = f(fb)` — the
auto-application of `fb` against the ambient given *is* the Reader
diagonal. **The Applicative is the S combinator** (E15):
`ap(ff)(fb) = ff(fb)`, both sides reading the same environment —
exactly `S f g x = f x (g x)`. And the applicative chain across
*different* environments types the way you would hope (E18):

```scala
f.curried <*> wire[Db] <*> wire[Log] <*> wire[Clock]  :  Db ?=> Log ?=> Clock ?=> String
```

— the graded applicative of composed Readers, where weakening is
free (`val w: Db ?=> Log ?=> Db = wire[Db]` compiles).

The practical conclusion cuts both ways. *Direct style needs none
of it*: `summon[Env].user` replaces `user.map(...)`; juxtaposition
`f(wire[A], wire[B], wire[C])` ascribed to the curried chain *is*
`pure f <*> a <*> b <*> c`, performed by the elaborator. So Okay
ships no wrapper type and no method syntax for bare context
functions. But the *generic combinators* — `traverse`, `sequence`,
`replicateA`, written once over any `F[_]` — need an instance, and
juxtaposition cannot replace them (E19). Core therefore carries:

```scala
given ctxMonad[E]: Monad[[X] =>> E ?=> X]   // Providing.scala
```

and one import unlocks the combinators over readers:

```scala
import okay.given   // instances need the given import — `import okay.*` is not enough

val xs: Seq[Env ?=> Int] = Seq(wire[Env].uid, wire[Env].uid + 1)
val all: Env ?=> Seq[Int] = sequence(xs)     // F is INFERRED
```

`Applicative` also carries the symbolic `<*>` (an inline alias of
`app`), so generic idiom brackets read as written in the papers —
over `!`, over `?=>`, over any carrier:

```scala
def idiom[F[_]](fu: F[String], fn: F[Int])(using M: Applicative[F]): F[String] =
  M.pure((u: String) => (n: Int) => s"$u#$n") <*> fu <*> fn
```

## Reader elimination — the row's environment as a capability

A program whose row carries `Reader % E` can shed it: move the
environment into a `using` parameter and the elaborator runs the
Reader half at compile time — zero runtime, and one effect fewer to
handle. With direct blocks (docs/direct-style.md) the rewrite is a
deletion:

```scala
// the row spelling: environment as an effect, handled at run
def viaReader: Int ! (Reader % Int + W) = direct {
  val env = effect[Reader % Int + W, Int](Reader.Ask()).!?
  effect[Reader % Int + W, Unit](Writer(s"env=$env")).!?
  env + 1
}
// the elimination: environment as a capability — Reader is GONE
def viaCtx: Int ?=> Int ! W = direct {
  Writer(s"env=${wire[Int]}")
  wire[Int] + 1
}
provide(41)(viaCtx)   // same answers as Reader.run(41)(viaReader)
```

`TestCtxReaderElim` asserts the equivalence, that `provide` nesting
overrides through the effectful block (nearest-wins survives the
macro), and the two one-line bridges for migration — functions at
the call site, never Conversions (E10):

```scala
def lift[E, A](cf: E ?=> A): A ! (Reader % E) =
  effect[Reader % E, E](Reader.Ask()).map(e => cf(using e))
def unlift[E, A, F[+_]](p: A ! (Reader % E + F)): E ?=> A ! F =
  Reader.run[E, A, F](wire[E])(p)
```

When to keep the Reader row instead: when the environment must
CHANGE mid-program (`Reader.local`-style scoping inside one
program), or when a handler wants to observe the asks — a
capability is invisible to handlers by design.

## The boundaries, stated exactly

Every one of these is a compiler-verified refutation, kept because
the failures are load-bearing.

- **Method syntax on a bare context function is worse than broken**
  (E13). The receiver eagerly applies *before* extension lookup, so
  `(x: Env ?=> String).map(f)` silently dispatches to `String.map`
  over `Char`s — a wrong-method trap, not an error. Use direct
  style, or the generic combinators through `ctxMonad`.
- **The language forbids the cheap boxes** (E13): `opaque type`
  over a context-function type is rejected outright, and so is an
  `AnyVal` wrapper. Only a real allocating class could restore
  `.map`/`for` — and it would reintroduce the ceremony the feature
  exists to delete, so Okay ships none.
- **Bridges must be functions, never Conversions** (E10). A
  `Conversion` into or out of a ctx-fn type loses to eager
  application; the honest bridge is a named function with a `?=>`
  parameter.
- **Same-type linear rebinding is impossible** (E1/E2/E5): two
  `given Db` in one scope are an ambiguity, by language design.
  Overriding is done by *nesting* (provide/providing), not by
  redefinition.
- **The single tuple-typed `provide` definition is blocked** in
  parameter position (E11/E12: match types do not drive
  eta-expansion there) — which is why the flat form is generated to
  22 and the unbounded form is `providing`/`and`, whose type
  lambdas reduce where match types stall (E16).
- **Instances ride the given import**: `import okay.*` brings the
  functions, `import okay.given` brings `ctxMonad` (and friends).
  Forgetting the second is a "no given instance" error at a
  `sequence` call, not silence.
- **The ctx-monad is for width, not depth** (E22). Every bind the
  compiler runs is a stack frame — there is no trampoline — and a
  left-nested chain overflows between ~2 000 and ~5 000 binds on a
  default stack. `traverse`/`sequence` over a config or a page of
  readers is the intended scale; a 10k-deep monadic chain belongs
  to the row's `Reader % R`, which trampolines on Cont. And never
  grow a chain through a mutating `var`: the closure inserted at
  `val prev: Env ?=> A = prog` captures the var BY REFERENCE, the
  chain becomes self-referential, and it overflows at any depth —
  build chains by recursion.

## The door outside, the direct block inside (the E20 pattern)

The capability vocabulary composes with [direct style](direct-style.md)
— and the composition is the recommended shape for user code that
wants both: the door answers *what is available*, the block answers
*how it reads*.

```scala
def told: Env ?=> Int ! (Writer % String) = direct {
  Writer(s"hello ${wire[Env].user}")   // bare statement: do-notation
  Writer("bye")
  wire[Env].uid                        // the door, inside the block
}

provide(Env("ada", 7)) { !.run(Writer.run(told)) }
// Vector(hello ada, bye) -> 7
```

This works because a `direct` block is itself a context function
(`DirectCtx[F] ?=> A`), so it nests under the `Env` layer by
nearest-wins and `wire` resolves inside it. Three layers are peeled
by three different machines — `provide` by the compiler at
elaboration, the block by the macro at expansion, the effect row by
handlers at run time — and none knows of the others. `providing`
compositions and the override story work over the block unchanged
(E20; executable as `TestDirectDoors` in core).

## The payoff, on one page

One value whose needs are its type, living in three worlds without
changing a letter — executable as `TestShowcase` in okay-obs:

```scala
val api: (Principal, Tracer) ?=> Traced.Route = {
  case r if r.url.contains("/quote") =>
    okay.async {
      wire[Tracer].span("db.lookup") { () }
      Response(200, Nil, Http.one(s"for:${wire[Principal].name}".getBytes))
    }
}

// 1. production: the doors install from the wire — a verified JWT
//    becomes the Principal, a traceparent becomes the Tracer
Traced.route(tracer)(Secure.granted(verify, Policy.scoped("read"))(api))

// 2. unit test: provide installs the SAME needs directly —
//    no token, no HTTP machinery
provide(ada, tracer)(api)

// 3. environments are values: one base, one overridden layer
(base and providing[Principal](bob)) { api }
```

In all three, a missing capability does not compile. Notice what is
absent: no `.map`, no `.flatMap`, no `<*>`, no container, no
reflection — the monad is there, but the compiler is running it.

## Where to go next

- [specs/context-functions.md](../specs/context-functions.md) — the
  doctrine and the experimental base E1–E19, refutations kept.
- [guide §9](guide.md) — the capability story inside the layer tour.
- [typepedia](typepedia.md) — the two-line door recipe and the
  recurring gotchas, greppable.
- `TestProvide` / `TestProviding` / `TestWire` / `TestCtxMonad` /
  `TestApOp` (core), `TestShowcase` (okay-obs) — every claim on this
  page, running.
