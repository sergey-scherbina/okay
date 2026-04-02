# CLAUDE.md — Okay! Extensible Effects for Scala 3

## Project Overview

**Okay** is a research implementation of extensible algebraic effects for Scala 3. Its primary goal is optimisation through correct modelling: mathematical structures from category theory and denotational semantics are projected directly into the type system, so that wrong compositions are structurally inexpressible rather than caught at runtime.

This is not applied mathematics in the direct sense — it is a pragmatic projection of mathematical insights into a typed programming language. The design philosophy is **correctness by construction**: the algebra of effects, continuations, and monads is encoded in types and combinators such that the implementation is forced into the right shape. Defensive programming, validation, and error recovery are deliberately absent from the core; the model itself prevents the wrong decisions.

The library is deliberately minimal. Every abstraction earns its place by capturing a real mathematical concept (free monad, parametrised monad, natural transformation, delimited continuation). Nothing is added for convenience alone.

**Key references embedded in the codebase:**
- ["Freer Monads, More Extensible Effects"](https://okmij.org/ftp/Haskell/extensible/more.pdf) — Oleg Kiselyov
- ["Parameterised Notions of Computation"](https://bentnib.org/paramnotions-jfp.html) — Robert Atkey
- ["Stackless Scala With Free Monads / Trampolines"](https://blog.higher-order.com/assets/trampolines.pdf)

---

## Repository Layout

```
okay/
├── build.sbt                     # SBT build: Scala 3.7.2, munit, JMH
├── project/
│   ├── build.properties          # SBT version: 1.11.2
│   └── plugins.sbt               # sbt-ide-settings, sbt-jmh
├── src/
│   ├── main/scala/               # All library source (package okay)
│   │   ├── Monad.scala           # Type class hierarchy
│   │   ├── Free.scala            # Free monad
│   │   ├── Eff.scala             # Extensible effects (core DSL)
│   │   ├── Cont.scala            # Continuation monad
│   │   ├── State.scala           # State effect handler
│   │   ├── Exc.scala             # Exception effect handler
│   │   ├── Throws.scala          # Opaque throws type for error handling
│   │   ├── Put.scala             # Generator / lazy-sequence abstraction
│   │   └── Producer.scala        # Producer effect (effectful generator)
│   ├── test/scala/               # MUnit test suites (package okay)
│   │   ├── TestEff.scala         # Effects: state, lazy lists, producers
│   │   ├── TestCont.scala        # Continuation monad examples
│   │   ├── TestPut.scala         # Generator / Fibonacci tests
│   │   └── TestThrows.scala      # throws type tests
│   └── jmh/scala/okay/
│       └── FibBenchmark.scala    # JMH microbenchmarks (Fibonacci)
└── README.md
```

All library source lives in the single Scala package `okay` (no sub-packages). There is no separate `main` entry point — the library is exercised through tests and benchmarks.

---

## Build & Development Commands

```bash
# Compile
sbt compile

# Run all tests
sbt test

# Run JMH benchmarks
sbt jmh:run

# Clean build artifacts
sbt clean

# Compile + test in one shot
sbt "clean; test"
```

**SBT version:** 1.11.2 (set in `project/build.properties`)  
**Scala version:** 3.7.2  
**Compiler flags:** `-Xkind-projector -Wall` (kind-projector support + all warnings)

---

## Architecture & Module Guide

### 1. `Monad.scala` — Type class hierarchy

Defines the standard FP type class ladder, all as Scala 3 traits with `given` instances:

| Type class | Key method |
|------------|-----------|
| `Functor[F[_]]` | `fmap` / `map` extension |
| `Applicative[F[_]]` | `pure`, `app` |
| `Selective[F[_]]` | `select`, `branch`, `ifS` |
| `Monad[F[_]]` | `flatMap` / `>>=` |
| `Alternative[F[_]]` | `empty`, `append` |
| `MonadPlus[F[_]]` | `mzero`, `mplus` |
| `Comonad[F[_]]` | `extract`, `coflatMap` |
| `ParaMonad[M[_,_,_]]` | Parametrised (indexed) monad |

Also defines:
- `type Pure[A] = A` — identity comonad
- `type ==>[F[_], G[_]]` — natural transformation (rank-2 function alias)
- Kleisli composition `>>>` via extension on `A => M[B]`

### 2. `Free.scala` — Free monad

```scala
enum Free[F[+_], A] { Pure(a), Inject(a: F[A]), Bind(a, f) }
```

- `fold` is `@tailrec` — left-associated `Bind` chains are reassociated to guarantee stack safety.
- `run` interprets into any `Monad[F]` (using a natural transformation if needed).
- A `Monad[Free[F, *]]` `given` instance is provided.

### 3. `Eff.scala` — Extensible effects (core DSL)

The central module. Key definitions:

```scala
infix type ![A, F[+_]] = Free[F, A]   // "A with effect F"
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]  // effect union
```

**Smart constructors:**
```scala
pure[F, A](a)      // lift a pure value
effect[F, A](fa)   // lift an effect
```

**Effect execution helpers:**
- `resume` — tail-recursively normalises a `Bind` chain
- `unfoldF` / `foldF` — step-wise unfolding (requires `Functor[F]`)
- `next(steps)` — run `steps` evaluation steps (requires `Eval[F]`)
- `?` — run to completion (requires `Eval[F]`)
- `runEval` — full tail-recursive runner
- `run[A](e: A ! Nothing)` — run an effect-free computation

**Effect handler:**
```scala
!.handle(computation)(returnClause)(effectClause)
```
Dispatches on `F[A] | G[A]` using `Typeable`, loops tail-recursively.

**`<|>[F, G]`** — splits a union effect value into `Either[F[A], G[A]]`.

**`Eval[F]`** — type class for purely evaluating a single effect step (used in `next` and `?`).

### 4. `Cont.scala` — Continuation monad

Uses infix type aliases for readability:

```scala
infix type /[A, B] = A => B        // function
infix type \[A, R] = A / R / R     // continuation: (A => R) => R
infix type ^[A, R] = A / A / R     // identity continuation
type Cont[A, B, C] = A / B / C     // (A => B) => C
```

- `shift` / `reset` — standard delimited continuation operators
- `loop` — re-invocable continuation (coroutine-like)
- `ParaMonad[Cont]` instance provided

### 5. `State.scala` — State effect

```scala
infix type %[F[_, _], S] = F[S, *]   // partial application helper

enum State[S, +A] { Get(); Set(s: S) }
```

- `State.get[S]` / `State.set[S](s)` — effect constructors
- `State.handle(initialState)(computation)` — tail-recursive handler
- `State.run(s)(a)` — runs a pure state computation to `(S, A)`
- `State.index(seq, from)` — example: number sequence elements using state

### 6. `Exc.scala` — Exception effect

```scala
case class Exc[E, +A](e: E)
raise[E, A](e)      // inject exception effect
runExc(computation) // handle to Either[E, A] ! F
```

Lightweight algebraic exception: no `throw`, just effect dispatch.

### 7. `Throws.scala` — Opaque error type

```scala
opaque infix type throws[+A, +E <: Unsafe] = A | E | Either[E, A] | Try[A]
type Safe   = Nothing    // no error possible
type Unsafe = Throwable  // any throwable
```

Extension methods:
- `.wrap` → `Either[E | Unsafe, A]`
- `.??`   → alias for `.wrap`
- `.handle(f)` / `.?(f)` → recover with a function
- `.unwrap` / `.?` → extract or throw
- `unsafe { ... }` — catch exceptions into `throws`

Implicit `Conversion` instances allow plain `A`, `E`, `Either`, `Try` to be used where `A throws E` is expected.

### 8. `Put.scala` — Generator abstraction

```scala
trait Put[F[_]]:
  def put[A](a: A): A \ F[A]   // yield a value into continuation
```

- `generate(seed)(valueF)(nextF)` — general infinite generator
- `lazyList` — specialised for `LazyList`
- `num[N, F]` — natural numbers starting from zero
- `fib[N, F]` — Fibonacci sequence (any `Numeric` type)
- `given Put[LazyList]` — LazyList implementation using `shift`

### 9. `Producer.scala` — Effectful producer

```scala
type Produce[A] = Pure[A]      // effect functor = identity
type Producer[A] = A ! Produce  // effectful stream

produce[A](a)    // construct one producer step
given Put[Producer]  // Producer implements Put
Producer.log()   // Eval instance that prints each yielded value
```

---

## Key Scala 3 Idioms Used Throughout

### Infix type aliases for DSL readability
```scala
val x: String ! State % Int   // String with State[Int, *] effect
val y: A throws Fault          // A that may throw Fault
val f: A / B                   // function A => B
```

### `given` instances for type classes
All type class instances are `given` (not implicit). Retrieve with `summon` or via `using` clauses.

### `inline` pervasively
Most one-liners are `inline` to eliminate overhead and enable compile-time computation. Respect this convention when adding new code.

### `@tailrec` for stack safety
`Free.fold`, `!.resume`, `State.handle`, `!.next` are all `@tailrec`. **Stack safety is a hard requirement** — the test suite includes 1 000 000-iteration stress tests. Any new handler must be tail-recursive or use trampolining.

### Extension methods
Behaviour is added to types via `extension` blocks, not wrapper classes:
```scala
extension [A, E <: Unsafe](a: A throws E)
  def wrap: Either[E | Unsafe, A] = ...
```

### Enum for ADTs
`Free`, `State`, `Exc` all use Scala 3 `enum` for their algebraic data types.

### Type lambdas and kind-polymorphism
`[A] =>> F[A] | G[A]` and similar type lambdas appear frequently. The `-Xkind-projector` flag enables `*` shorthand in some positions.

---

## Testing Conventions

- Framework: **MUnit** (via JUnit runner — tests use `@Test` annotations directly from `org.junit`).
- No `FunSuite` used; test classes extend nothing, methods are annotated `@Test`.
- Tests print to stdout liberally with `println` — this is intentional for observability.
- **Stack stress tests** (`stackStress = 1_000_000`) are included in `TestEff` and must pass. Any new effect handler should have an equivalent stress test.
- `Assert.assertEquals` from JUnit is used for assertions.

Run tests:
```bash
sbt test
```

---

## Benchmarking

JMH benchmarks live in `src/jmh/scala/okay/FibBenchmark.scala`. Run with:
```bash
sbt jmh:run
```

The benchmark exercises Fibonacci generation to compare performance of effect-based vs direct computation.

---

## Design Philosophy

### Optimisation through modelling

Performance and correctness are achieved through the right model, not through tuning or guarding. Concrete examples:

- **Effect unions as Scala union types** (`F[A] | G[A]`) — effect composition is native type-level set union, not a wrapper hierarchy. The type checker enforces that every effect in a union is handled; unhandled effects are a compile error.
- **Free monad as the single interpreter target** — all effect handlers are morphisms out of `Free`. There is one execution model, not many special cases.
- **`@tailrec` as a design constraint, not an optimisation** — stack safety is guaranteed structurally by the left-reassociation in `Free.fold`. The annotation is a proof obligation, not a hint.
- **`inline` on hot-path combinators** — eliminates abstraction overhead without sacrificing the compositional model.
- **`Cont` as first-class type** — delimited continuations (`shift`/`reset`) are not encoded in a monad stack; they are the model. Generators (`Put`, `Producer`) fall out of this directly.

### What this means when reading or modifying the code

- Do not add runtime checks for things the type system already prevents.
- Do not add fallback paths or defaults for states the model cannot produce.
- If a new handler requires a non-tail-recursive loop, the model is wrong — fix the model.
- Conciseness is not style preference; it is fidelity to the underlying mathematics. A verbose rewrite is a mistranslation.
- When in doubt about an addition, ask: does this correspond to a mathematical concept? If not, it probably does not belong here.

---

## Conventions & Constraints

1. **All code belongs to package `okay`** — no sub-packages.
2. **Preserve `@tailrec` annotations** — do not break tail-recursive loops without replacing them with an equivalent trampoline.
3. **Do not add runtime `throw`** except inside `Throws.scala`'s `unwrap` (the library avoids JVM exceptions by design).
4. **Effect unions use Scala's union types (`|`)** — do not wrap in separate sealed traits.
5. **Compiler warnings are errors (`-Wall`)** — new code must compile cleanly with all warnings enabled.
6. **No external runtime dependencies** — only `munit` (test scope) and JMH (benchmark scope) are allowed.
7. **Inline liberally** — follow the existing style of marking small, hot-path methods `inline`.
8. **Academic style is intentional** — concise, symbol-heavy code mirrors the source papers. Do not refactor toward more verbose "enterprise" style.

---

## Effect Composition Pattern

The typical pattern for composing multiple effects:

```scala
// 1. Write computation with combined effects
val prog: A ! (State % S + Exc % E) = for {
  s <- State.get[S]
  _ <- if (bad) raise(Exc(err)) else pure(())
  ...
} yield result

// 2. Handle effects from outermost to innermost
val result: Either[E, (S, A)] =
  !.run(Exc.runExc(State.handle(s0)(prog)))
```

Effect handlers are applied as nested function calls; order determines semantics (state-outside-exception vs exception-outside-state).

---

## Git Workflow

- Main branch: `master`
- Feature branches follow the pattern `claude/<description>-<id>`
- Commits are short, noun-phrase style: `"Free"`, `"Throws, wrap unwrap"`, `"benchmark"`
