# Core gaps — Maybe, Either→Throws, Supply, and the review's other findings

## Overview

A review of the core (Free, Cont, Effects, Handler and the effects in
`src/main/scala`) on 2026-09-26, at the operator's request. It found three
small absences worth filling now, and a set of larger questions that each
need their own lane. This spec covers the first group, which lands as
`core-maybe-supply`. The second group is filed as `backlog.d/okay-core`
items so each can be taken up separately. The list under "Filed" is the
index to them.

### What was missing

1. **A value that may be absent had no effect of its own.** `Abort` is
   `Throws % Unit`. Operations are identified by CLASS, and a row may hold
   only ONE `Throws` (Throws.scala, TestRowIdentity). So a program that
   can find nothing AND fail with a reason (`A ! Abort + Throws % E`) was
   refused by `Distinct` unless it went through `Tag`. There was also no
   way into it from an `Option`: nothing in the repository lifts
   `Option[A]` to `A ! Abort`, and every call site wrote the `match`
   itself.
2. **No way from `Either` into `Throws`.** `catching` lifts a JVM throw
   and `raise` lifts an error value, but an `Either[E, A]` coming back
   from a library had to be spelled out as `fold(raise, pure)` each time.
3. **No source of fresh values.** A compiler pass, a builder of graphs or
   an id generator needs "a new one, different from every earlier one"
   (Launchbury's `Supply`, fused-effects' and polysemy's `Fresh`). The
   usual workaround is `State % Long` plus `modify(_ + 1)`, which exposes
   `set` to code that should only ever draw.

## Interface

```scala
// Maybe.scala (core)
final case class Maybe[+A](value: Option[A]) derives Effect

extension [A](o: Option[A]) inline def maybe: A ! Maybe   // Some → answer, None → stop

object Maybe:
  inline def none[A]: A ! Maybe
  def run[A, F[+_]](p: A ! Maybe + F): Option[A] ! F     // Nothing found → None
  def collect[X, B, F[+_]](xs: Iterable[X])(f: X => B ! Maybe + F): Vector[B] ! F  // skip the absent
  def prune[A, H[+_]](p: A ! Maybe + H): A ! Choose + H  // absent → a dead branch
  extension [A, F[+_]](p: A ! Maybe + F)
    def orElse(q: => A ! Maybe + F): A ! Maybe + F      // try q where p found nothing
    def getOrElse(a: => A): A ! F

// CanFail: a row holding Maybe drops a refuted pattern step as Maybe.none
// (priority: Choose > Maybe > Abort)

// Throws.scala
extension [E, A](e: Either[E, A]) inline def orRaise: A ! Throws % E

// Supply.scala (core)
enum Supply[S, +A] derives Effect:
  case Next() extends Supply[S, S]
type Fresh = Supply % Long

object Supply:
  inline def next[S]: S ! Supply % S
  def run[S](first: S)(step: S => S)[A, F[+_]](p: A ! Supply % S + F): (S, A) ! F
object Fresh:                                        // not a top-level `fresh`: DI owns that name
  inline def next: Long ! Fresh
  def run[A, F[+_]](p: A ! Fresh + F): A ! F       // 0, 1, 2, …
```

## Behavior

### Maybe
- [x] `Some(x).maybe` answers `x`; `None.maybe` stops the program and
      `Maybe.run` answers `None`
- [x] `Maybe.run` of a program that never stops answers `Some`
- [x] a row `Maybe + Throws % E` passes `Distinct` and each handler
      answers its own: the review's point 1, which `Abort` cannot do
- [x] forwarding: effects after a `Some` still run, and effects after a
      `None` do not
- [x] `orElse` runs the alternative only where the first program found
      nothing, and `getOrElse` answers the default
- [x] a refutable pattern in a `Maybe` row compiles and stops through
      `Maybe` (`CanFail`)
- [x] `collect` skips the elements that are not there and goes on
- [x] `prune` under `runChoice`: a branch that found nothing dies and the
      others answer, including in a row that already holds `Choose`
- [x] stack-safe: 100 000 `Some(i).maybe` binds in a row

### Either → Throws
- [x] `Right(a).orRaise` answers `a`; `Left(e).orRaise` raises `e`, and
      `runEither` answers `Left(e)`

### Supply
- [x] `Fresh.run` draws 0, 1, 2 … in program order
- [x] `Supply.run(first)(step)` answers the final seed with the value
- [x] forwarding: another effect interleaved with draws keeps its place
- [x] under `Choose` both branches draw from the same point: the handler
      threads the seed through the answer (as `State` does), so the
      residual program can be run again, and each branch continues from
      the seed it was captured with
- [x] stack-safe: 100 000 draws

## Design

**Maybe is its own class, not an alias for `Option`.** A collection is
already a signature here (`List(1,2).perform` is nondeterminism, Choice.scala),
and `Option` could be one the same way. Choice.scala names the reason not
to do that: a row needs a signature that means one thing. `Option` in a row
means whatever the reader guesses. A box costs one 16-byte allocation per
operation, the same price `Choose` pays.

**`Maybe` does not replace `Abort`.** They mean different things. `Abort`
is a failure with nothing to say, and it has `recover` and the `Throws`
family's handlers. `Maybe` is an absence. With `Maybe` and `Throws` now in
one row, the question "was it not there, or did it break?" finally has two
answers. `Abort` stays as it is because there are call sites using it
(docs/guide.md, okay2).

**Stop OR skip: the handler's scope decides** (operator's question,
2026-09-26: "otherwise it is the same Abort"). An absence ends the
scope of the handler that answers it, and nothing more. `run` over the
whole program stops it. `collect` puts a `run` around EACH element, so an
absent element is skipped and the rest go on. `prune` turns each `Maybe`
into a `Choose` (`Some(x)` is one alternative, `None` is none), so under
`runChoice` an absent branch dies and the others answer. `Abort` has
only the first reading, because its scope is always a `Throws`
handler's.

**CanFail priority: Choose > Maybe > Abort.** A searching row prunes, as
before. Between the two stopping effects, `Maybe` wins, because a refuted
pattern (`case Some(x) <-`) is exactly an absence.

**Supply threads its seed through the answer, as State does.** A mutable
counter inside the handler would be cheaper by one tuple per draw. But it
would make the residual program impossible to re-run, and it would make
two branches of a `Choose` draw different values after the split point.
The review's own finding (single-shot-row, refuted 2026-09-09) is that a
mutable cell buys little in this core. `Supply` has one operation and no
`set`, and that is the difference from `State % S` that justifies it: the
code that draws cannot rewind the supply.

## Out of scope — filed as backlog.d/okay-core items

Each is a separate problem with its own evidence, to be taken up one at a
time:

| slug | problem |
|---|---|
| `handler-single-pass` | every `handle` is one full walk of the tree, and a forwarded op is rebuilt per layer (`Inject(e).flatMap(x => again(k(x)))`); n handlers = n walks. Evidence passing (Xie & Leijen 2021) / one runner with a handler stack |
| `resume-inline-budget-guard` | `Free.resume` is 323 B against `FreqInlineSize` 325; one more case silently changes inlining in every loop. Needs a check in the gate |
| `tag-rename-pass-cost` | `Tag.tag` re-walks the whole program to rename; a many-instances row pays a pass per key |
| `bracket-forwards-no-effects` | `bracket` runs `use(r).runWith` inside, so `use` cannot forward effects; the name promises the Resource-effect semantics |
| `writer-listen-censor` | Writer has no scoped operations (`listen`, `censor`), unlike Reader's `local` and Throws' `recover` |
| `error-accumulation-effect` | `Validated` is applicative DATA; there is no EFFECT that records non-fatal errors and lets the program continue (Chronicle / `Ior` / arrow-kt `accumulate`) |
| `random-clock-signatures` | `Random` lives only inside Prob/Sim and `Clock` in Provide; a decision is needed on whether they become core signatures |

## Decisions

- 2026-09-26: Maybe is a case-class signature over `Option`, not `Option`
  as the signature. The reason is Choose's (a row names meanings).
- 2026-09-26: Supply is pure (seed threaded through the loop). This
  follows State and the single-shot-row refutation.

## Results

### Landed 2026-09-26 (core-maybe-supply)

Every behavior box above is covered by TestMaybe, TestOrRaise and
TestSupply (JVM). Found on the way: a top-level `fresh` collides with
DI's `fresh[A]` (Provide.scala, E161), so the numbers live under
`object Fresh`. `Supply.from(xs)` (drawing from a collection) was
dropped from the interface before it was written. Raising on exhaustion
would put a second `Throws` in the caller's row, and that is exactly the
conflict this lane exists to remove. `Supply.run(first)(step)` covers
every generated supply.

## Stage 2 — writer-listen-censor (2026-09-26)

Writer's SCOPED operations are the duals of `Reader.local` and
`Throws.recover`. In mtl they are `MonadWriter`'s `listen` and `censor`.
Each is a handler over a PART of the program, and the row comes out
unchanged.

```scala
object Writer:
  // run p and answer its value AND what p told; every tell still reaches
  // the outer handler at its own place, in order with the other effects
  def listen[W, A, G[+_]](p: A ! Writer % W + G): (A, Seq[W]) ! Writer % W + G
  // run p and rewrite its WHOLE output with f; p's tells are held back
  // until p ends and told there as f(all of them)
  def censor[W, A, G[+_]](p: A ! Writer % W + G)(f: Seq[W] => Seq[W]): A ! Writer % W + G
```

- [x] `listen` answers p's value and p's tells, and only p's
- [x] `listen` re-tells in place: a tell before a raise inside p reaches
      an outer Writer handler that runs after `runEither`
- [x] `censor` rewrites p's output as a whole (a summary line in place of
      many), and tells outside p are untouched
- [x] `censor` holds back: a raise inside p drops p's held tells. The test
      pins this as the documented price of seeing the whole output
- [x] both are stack-safe over 100 000 tells

**Why `censor` is the whole-output one.** A rewrite of each told value
on its own already exists: `Writer.map` (one to one) and `Writer.expand`
(one to many, and a filter when it answers nothing). Over a
sub-program, with `V = W`, they are exactly per-element censoring. They
keep every tell in its place, so they are the ones to use when the order
against other effects matters. mtl's `censor` sees the accumulated `w`,
and that is the operation that was missing. Seeing it all means waiting
for all of it, so the tells move to p's end.

**Law (specs/scoped-effects-laws.md).** Neither operation duplicates or
reorders a FORWARDED operation. `listen` does not move tells either.
`censor` moves p's tells, and only those, to p's end, and that is its
definition.

### Landed 2026-09-26 (writer-listen-censor)

TestWriterScoped covers all five boxes. `G` has to be written at the
call (`Writer.listen[String, Int, Pure](step)`), as it does for
`Writer.map`/`expand`/`run`. Left to inference, the rest of the row is
solved as the row itself (`Writer % W + Writer % W`), and `Distinct`
refuses it by name. The error is loud, never a wrong answer.

## Stage 3 — error-accumulation-effect: `Chronicle` (2026-09-26)

Between `Throws` (stop at the first error) and `Validated` (collect every
error, but no step may depend on another) sits the program that RECORDS
an error and goes on, and fails at the end, or at a point it chooses, if
anything was recorded. The literature's name is Chronicle (Haskell
`these`: `MonadChronicle`, `dictate`/`confess`/`condemn`). Its siblings
are cats' `Ior`, arrow-kt's `Raise.accumulate`/`mapOrAccumulate`, and
zio-prelude's `ZValidation` with warnings.

```scala
enum Chronicle[E, +A] derives Effect:
  case Dictate(e: E) extends Chronicle[E, Unit]    // record, go on
  case Halt()        extends Chronicle[E, Nothing] // stop with what is recorded

object Chronicle:
  def dictate[E](e: E): Unit ! Chronicle % E
  def confess[E, A](e: E): A ! Chronicle % E        // dictate(e), then halt
  def halt[E, A]: A ! Chronicle % E

  enum Verdict[+E, +A]:
    case Clean(a: A)                                // nothing recorded
    case Warned(a: A, errors: Vector[E])            // recorded, finished anyway
    case Failed(errors: Vector[E])                  // halted

  def run[E, A, F[+_]](p: A ! Chronicle % E + F): Verdict[E, A] ! F
  // every element under its own scope: one element's halt does not stop
  // the others; all errors re-dictated in element order; halt at the end
  // if any element halted
  def all[E, X, B, F[+_]](xs: Iterable[X])(f: X => B ! Chronicle % E + F): Vector[B] ! Chronicle % E + F
```

- [x] nothing recorded → `Clean(a)`
- [x] two dictates, then a value → `Warned(a, [e1, e2])`
- [x] `confess` stops: effects after it do not run → `Failed(all so far)`
- [x] `all`: an element's halt does not stop the next element, and every
      element's errors come out in element order; `Failed` at the end
- [x] `all` with warnings only → `Warned(values, warnings)`
- [x] a row `Chronicle % String + Throws % IOError`: both handlers answer
      their own
- [x] stack-safe: 100 000 dictates

**Why not `Writer % E + Throws % E`**, which the backlog item asked about
first. It does model the linear case: `Writer.run(runEither(p))` answers
`(warnings, Either[firstFatal, A])`. It fails on three points. (1) It
takes the row's ONE `Throws` slot, so a program that already raises
`Throws % IOError` cannot also accumulate validation errors, the same
class conflict that made `Maybe` its own effect. (2) The handler order is
load-bearing: with `runEither` outside, the warnings vanish on a raise.
(3) Accumulating ACROSS elements is a scoped operation (`all`) that
neither handler has. One signature with two operations and one handler
avoids all three.

**`Halt` carries no error.** `confess(e)` is `dictate(e)` followed by
`halt`, which is `these`' `condemn` shape. That lets `all` stop with the
errors it has ALREADY re-dictated rather than inventing one to confess.
A bare `halt` with nothing recorded answers `Failed(Vector())`. That is a
possible verdict, and the type does not pretend otherwise.

### Landed 2026-09-26 (error-accumulation-effect)

TestChronicle covers all seven boxes in five tests. As with
`Writer.listen`, `all`'s rest-of-row has to be written at the call when
`f`'s row is `Chronicle` alone (`Chronicle.all[String, String, Int,
Pure]`). Otherwise it is solved as the row itself, and `Distinct` refuses
it by name.

## Stage 4 — resume-inline-budget-guard (2026-09-26)

`TestInlineBudget` reads the code length of `Free.resume`, `relay`'s
loop and `Effects[Free].handle`'s loop out of the compiled classes and
fails when one exceeds HotSpot's `FreqInlineSize` (325). The failure
names the benchmark lanes to re-measure. At landing: resume 323, handle
loop 318, relay loop 266.

- [x] each hand-sized method is found by exact name (a local `loop` is
      `loop$N`; `resume$$anonfun$1` is not `resume`)
- [x] watched RED: one extra case in `resume` read 465 bytes and failed
      with the re-measure message; reverted
- [x] the instrument's control: a one-call wrapper (`relay`) reads under
      20 bytes

**Decisions.** The test uses a class-file reader of its own (JVMS §4, one
screen), not `java.lang.classfile`. dotty 3.9 cannot load that API's
sealed model types, because their permitted subclasses are `jdk.internal`
classes, and compiling the first draft crashed in `ClassfileParser`. The
test checks the UPPER line only. Shrinking a loop under the line is
case 2 of the measured history (worse, not better), so it is a question
for a benchmark, not for a guard.

## Stage 5 — bracket-forwards-no-effects (2026-09-26)

`bracket` used to run `use` to the end INSIDE, by the row's comonadic
`Handler` under `try/finally`. So `use` could perform only effects that
have such a handler (`Async`, `Produce`, `Pure`), and nothing in it
reached an outer handler. That is a strong release guarantee under a name
that promises the other thing. Every other library's `bracket` is the
forwarding form.

- `bracketNow` is the old function, unchanged, under an honest name.
  Its callers were four tests and one benchmark class, and all are
  renamed.
- `bracket` is `Resource` in one expression:
  `Resource.run(acquire.flatMap(use))` behind `Free.delay`. `use`'s
  effects are forwarded, and the release runs when `use` finishes, when
  a step throws, and when a forwarded `Async` step fails (`Failing`).
  Like `Resource`, it releases nothing when an ABORTIVE handler outside
  drops the rest of the program, and its doc says so.

- [x] `bracket` forwards a `Writer` from inside `use`, and releases after it
- [x] `bracket` releases when a forwarded `Async` step throws
- [x] built once and run twice, it acquires and releases twice. The
      first cut acquired when the program was BUILT, because
      `Resource.run` walks to the first forwarded operation when it is
      called. `Free.delay` fixes it, and the test caught it.
- [x] `bracketNow` keeps its tests, and is still refused in a `Delim` row

Also decided the same day (random-clock-signatures, surveyed): no
`Random` or `Clock` in the core. The backlog item records the numbers and
where a `Clock` goes when a test first needs it.

### Stage 5b — the release at an abort (bracket-final, operator ask, same day)

The first cut documented a hole it had inherited from `Resource.run`. A
raise inside `use`, caught by `runEither` OUTSIDE the scope, dropped the
continuation that held the finalizers, and the resource leaked. The
operator refused to keep the hole ("release must be guaranteed").

- `trait Final` (Resource.scala) marks an operation no handler resumes.
  `Throws` (so `Abort`) always; `Maybe` when `None`; `Chronicle.Halt`;
  `Choose` with no alternatives. The name is Koka's `final ctl`, the
  operation-side view of OCaml 5's `discontinue`.
- `Resource.run` releases everything BEFORE forwarding a final
  operation, and hands on a continuation holding no finalizers, so a
  handler that resumed it anyway could not release twice.

- [x] raise inside `bracket`, `runEither` outside: released; `recover`
      outside: released exactly once more
- [x] `None`, `confess`/halt, empty `choose` inside: released
- [x] a `Some` is not final: open, use, close in that order
- [x] watched RED before the change (both release tests failed)

What stays open, stated rather than hidden: a handler that decides to
drop the continuation of an operation that normally resumes. No effect
system knows that without the handler saying so (OCaml needs the
handler to call `discontinue`). `bracketNow` covers it with
`try/finally`.

## Stage 6 — release-all-finalizers (2026-09-26, operator's question "and if a resource throws, as in ZIO?")

ZIO's answer has three parts. A failing `use` still runs the release.
A failing `acquire` releases what came before it. A failing RELEASE
neither stops the other finalizers of the Scope nor hides the original
failure, because all of them are composed into one `Cause`. We had the
first two and not the third. `releaseAll` was `fin.foreach(_())`, so a
throwing finalizer skipped every later one (their resources leaked), and
during a failure its exception REPLACED the program's. `bracketNow`'s
`try … finally` did the same on the JVM level.

- `Resource.releaseAll` / `releaseAfter` call every finalizer exactly
  once. The first failure is thrown and the rest are `addSuppressed` on
  it, or, when a failure is already propagating, every release failure is
  suppressed on THAT. This is Java's try-with-resources, the JVM's shape
  for ZIO's `Cause`.
- Used by `Resource.run`, `Resource.open` (its closer and its
  acquire-failure path), `bracketNow`, and the Async `Failing.guard`
  (a thrown `Run` and a failed `Await` keep their error).

- [x] a throwing finalizer: the others still run, in reverse order, and
      its error is thrown
- [x] a failing use + a failing release: the use's error, with the
      release's suppressed on it
- [x] the same for `bracketNow`, for `Resource.open`'s closer, and for a
      failing Async step
- [x] watched RED before the change: the first four tests (the Async one
      was written together with its fix)

## Stage 7 — cancel-releases-resource (2026-09-26)

In ZIO, a timeout or a lost race INTERRUPTS the fiber, and interruption
runs every finalizer. The test asked the same of us, and two cases failed
first: a `bracket` whose `use` was parked on an `Await` (`Async.sleep`)
leaked its resource when `Async.timeout` cancelled it, and again when it
lost an `Async.race`. Cancelling a parked wait called the canceller its
registration answered with, and nothing else. The scope waiting there
could never continue, and its finalizers went with it.

- The Async `Failing` guard now wraps the Await's CANCELLER too.
  Cancelling an open wait releases the scope, once across all three
  doors (answer Left, thrown Run, cancel). It does not release after the
  wait has ANSWERED: the callback drive keeps that canceller and calls it
  on a later cancel, when the scope is still running.
- A `use` blocked inside a `Run` (`Thread.sleep`) was already released by
  the interrupt the virtual-thread fiber takes. The same test pins it.

- [x] timeout around a `use` parked on an Await: released (watched RED)
- [x] timeout around a `use` blocked in a Run: released
- [x] a `bracket` losing a race: released (watched RED)

What stays open: the callback drive (`Drive`, JS and `Schedulers.own`)
stops BETWEEN two operations when cancelled while running non-blocking
code, and the residual it drops is not discontinued. The next wait would
have released the scope, but the drive never reaches it. A virtual-thread
fiber (the JVM default) takes that cancel as an interrupt at its next
blocking point, which does release it.
