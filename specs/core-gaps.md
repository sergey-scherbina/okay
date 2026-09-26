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
