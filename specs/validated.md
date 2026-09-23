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

- [x] Two invalid leaves under `traverse` report BOTH errors, in
      program order; the monadic road over the same leaves reports the
      first. The pair is the test.
- [x] `Valid` composes as `Either`'s `Right` does: same answers for
      every all-valid program.
- [x] The Applicative laws hold (identity, homomorphism, interchange,
      composition) for a `Semigroup` that is not commutative, so a
      test that passes by accident on `List` concatenation cannot.
- [x] `andThen` short-circuits, and the type says so by NOT being
      `flatMap`: there is deliberately no `Monad[Validated]`, because
      the monad-applicative consistency law would force `app` to stop
      at the first error, which is the behaviour this type exists to
      refuse. The law is the reason, and the test asserts the two
      differ.
- [x] A `Semigroup` instance is required, not a `List` assumed: the
      accumulation works for `Chunk`, for a count, for a
      `Map[Field, Seq[Problem]]`, and the test uses a non-list one.
- [x] `Validate.validated(schema)(json)` reads okay-codec's accumulating
      schema walk as a `Validated[Validate.Errors, A]`: equal to
      `decode` as an `Either`, and two walks combined under `app` keep
      both sides' errors with their paths (TestValidate, the bridge —
      two-accumulating-validators, 2026-09-23)
- [x] `okay-conf` reports every BAD variable of a configuration in one
      run — the first real consumer, and the one that decides whether
      the type earns its place.
- [x] Cost, predicted before measuring: an all-valid `traverse` over
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

- **The bridge, not a merge** (two-accumulating-validators,
  2026-09-23). `Validate.gather` stays `Either` by hand — it is the
  schema hot path and carries paths — and `Validate.validated` is one
  `fromEither` on the way out; `Errors` being a `Vector`, the
  `Semigroup` is concatenation and the instance resolves from the
  companions with no import. What the bridge buys is a schema walk as
  a LEAF: two schemas' errors in one `app`, a config of sections each
  walked, a form whose fields are schemas — without a second
  accumulator being taught.


Stage 0 (this spec): written 2026-09-18, out of the strategy review in
ROADMAP P13. The predictions above are the bars.

### Landed 2026-09-18

**A CORRECTION TO THE INTERFACE, made while implementing.** The spec
proposed a new `Semigroup` trait. `Monoid` already existed
(Fold.scala) with the same `combine`, so `Semigroup` was split out
ABOVE it instead and `Monoid extends Semigroup` — additive, every
existing instance still answers both. One thing that was not obvious
and is now written down: the instances live in `object Monoid`, which
is in the implicit scope of `Monoid` and NOT of `Semigroup`, so
`Semigroup.fromMonoid` bridges given search or `Validated` refuses the
vector monoid sitting three lines below it.

**THE CONSUMER IS THE RESULT.** `okay-conf`'s `fromEnv` collected its
parts and then did `parts.collectFirst { case Left(m) => Left(m) }` —
one bad environment variable per run. It is a `traverse` at
`Validated` now, and three mistyped variables come back in one
message (TestLayered, "THREE bad variables are reported in one run,
not one per run"). Honest note for the next reader: for a FLAT list of
parts like this one, a hand-rolled `collect` would have done the same
thing in one line. The type pays where the walk is nested or generic —
which is why the schema validator, not this call site, is the
consumer that will decide its long-term keep.

**THE COST PREDICTION IS REFUTED, in the favourable direction.**
ValidatedBenchmark, 1 000 leaves, `-f 3 -prof gc`:

| lane | µs/op | B/op |
|---|---|---|
| eitherAllRight | 23.902 ± 1.018 | 275 488 |
| validatedAllValid | 18.826 ± 16.745 | **147 528** |
| validatedAllInvalid (every leaf bad) | 23.067 ± 0.702 | 277 512 |

Predicted "within 10% of Either"; measured **46% LESS**, 147 528
against 275 488 B/op. The prediction assumed the happy path builds the
same number of nodes. It does not: `Either`'s `traverse` goes through
the MONAD-DERIVED `app` (`f.flatMap(g => fmap(a, g))`, Monad.scala),
which builds an intermediate per element, while `Validated`'s `app` is
one match with nothing between. An applicative written directly beats
one derived from a monad, and this is the number that says by how
much.

Accumulating every error costs 277 512 B/op, 0.7% over Either's happy
path — so the collecting behaviour is not paid for by the programs
that never fail.

The times are not readable: the run fell at load 35 and
`validatedAllValid` came back ±16.745 on 18.826, an 89% bar. The bytes
are exact to the third decimal and are the verdict, which is this
repository's standing rule for that situation.
