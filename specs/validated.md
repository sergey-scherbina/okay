# `Validated` — every error, not the first

## Overview

`Throws` is monadic, so it stops at the first error. That is right for
a computation whose later steps need the earlier ones' answers, and
wrong for the two cases this library meets most often: a configuration
being read and a form being filled. A user who mistyped four fields
should be told about four fields.

The classic answer is the one rung down the ladder this repository
just finished putting to work (specs/applicative-static.md, theory
ch. 12): an APPLICATIVE cannot bind one leaf's answer into another's
body, so it has no way to stop early, so it can collect. `Validated[E,
A]` is `Either[E, A]` with an applicative that accumulates on the left
instead of short-circuiting, and it is the smallest useful thing the
applicative rung buys that Okay does not already have.

Three consumers exist today and each is currently worse for the lack:

- **`okay-conf`** reads a configuration and reports the first missing
  key, so a misconfigured deployment is fixed one key per run.
- **Schema validation** (`Validate`, specs/schema-fold.md) already
  walks a whole structure; `form-errors-on-validate` sits in BACKLOG
  under okay-codec waiting for exactly this type.
- **`okay-openapi`** rejects a request body at its first bad field.

## Interface

```scala
// Validated.scala (core)
enum Validated[+E, +A]:
  case Valid(a: A)
  case Invalid(e: E)          // E is a SEMIGROUP, not a list

object Validated:
  /** the instance: pure is Valid, app CONCATENATES two Invalids */
  given [E: Semigroup]: Applicative[[A] =>> Validated[E, A]]

  /** the two doors between the roads */
  def fromEither[E, A](e: Either[E, A]): Validated[E, A]
  extension [E, A](v: Validated[E, A])
    def toEither: Either[E, A]
    /** the short-circuiting road, when a later step needs this answer */
    def andThen[B](f: A => Validated[E, B]): Validated[E, B]

/** how two errors of one kind combine — the ONE thing a caller supplies */
trait Semigroup[A]:
  extension (x: A) def combine(y: A): A
```

Nothing else. `traverse`, `sequence`, `replicateA`, `*>`, `<*` and
every other generic combinator already written against `Applicative`
work at this carrier the day the instance exists, which is the whole
argument for having built them generically.

## Behavior

- [ ] Two invalid leaves under `traverse` report BOTH errors, in
      program order; the monadic road over the same leaves reports the
      first. The pair is the test.
- [ ] `Valid` composes as `Either`'s `Right` does: same answers for
      every all-valid program.
- [ ] The Applicative laws hold (identity, homomorphism, interchange,
      composition) for a `Semigroup` that is not commutative, so a
      test that passes by accident on `List` concatenation cannot.
- [ ] `andThen` short-circuits, and the type says so by NOT being
      `flatMap`: there is deliberately no `Monad[Validated]`, because
      the monad-applicative consistency law would force `app` to stop
      at the first error, which is the behaviour this type exists to
      refuse. The law is the reason, and the test asserts the two
      differ.
- [ ] A `Semigroup` instance is required, not a `List` assumed: the
      accumulation works for `Chunk`, for a count, for a
      `Map[Field, Seq[Problem]]`, and the test uses a non-list one.
- [ ] `okay-conf` reports every missing key of a configuration in one
      run — the first real consumer, and the one that decides whether
      the type earns its place.
- [ ] Cost, predicted before measuring: an all-valid `traverse` over
      1 000 leaves allocates within 10% of the same traverse at
      `Either`, because the happy path builds the same number of
      nodes. If it does not, the encoding is wrong.

## Out of scope

- A `Monad[Validated]`. Refused on purpose, above.
- Replacing `Throws`. The two answer different questions and both
  stay; `Throws` is the road for a computation that cannot go on, and
  it has the `into` absorption and the JVM bridges that a validator
  does not want.
- A general `Semigroup`/`Monoid` hierarchy. One trait, one method,
  declared where it is used. `Aggregator` already carries the merge
  contract for folds and is not being generalized into this.
- Error POSITIONS (a path into the structure). That belongs to the
  schema walk, which already has paths, not to this type.

## Design

**Why `E` and not `Seq[E]`.** Making the error type a semigroup rather
than fixing a list lets the caller decide what accumulation means:
concatenation for a form, a count for a sampler, a map keyed by field
for an API. Fixing `Seq` would force every consumer to allocate one,
and `okay-codec`'s walk already has a better shape for its own errors.

**Why an enum and not `Either` with a different instance.** Two
instances for one type cannot both be given; the carrier has to differ.
An enum also makes the intent readable at the call site, which a
newtype over `Either` does not.

## Decisions

- **No `Monad`** — chosen because the consistency law would force the
  short-circuit this type exists to avoid; `andThen` gives the same
  power under a name that says the branch is deliberate. Rejected: a
  Monad instance for convenience (it makes `traverse` silently stop
  collecting, which is the whole defect).
- **A `Semigroup` parameter, not a fixed `Seq`** — see Design.
  Rejected: `Validated[Seq[E], A]` everywhere (allocates a sequence per
  leaf and fixes a shape three consumers disagree about).

## Results

Stage 0 (this spec): written 2026-09-18, out of the strategy review in
ROADMAP P13. The predictions above are the bars.
