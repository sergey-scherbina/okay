# cats-kernel-bridge — Semigroup, Monoid, Group across cats and okay

Status: done, 2026-10-02. Owner lane: `cats-kernel-bridge`.
From the cats-depth audit (backlog okay-cats).

## Goal

okay's `Semigroup`/`Monoid`/`Group` (Fold.scala) and cats-kernel's are
separate classes, so `catsValidated` asks okay's `Semigroup` and
`okayCatsValidated` cats'. okay has no `Eq`/`Order`, so nothing to
bridge there.

## Interface

- `Combine[E]` (default import): a combiner from either library, okay's
  first; both `Validated` instances of okay-cats ask it.
- `FromCatsKernel`: okay's `Group` ⇐ cats' `Group`, `Monoid` ⇐ `Monoid`,
  `Semigroup` ⇐ `Semigroup`, in that priority; `ToCatsKernel` the other
  way. One import each, never both.

## Behavior

- [x] default import: okay's `Validated` under cats' `traverse` with only
      a cats `Semigroup` (`NonEmptyList`); cats' `Validated` under okay's
      `traverse` with only an okay one; `String`, which both have, with
      `okay.given` in scope, no ambiguity
- [x] FromCatsKernel: okay's own `traverse` over okay's `Validated` with
      only a cats `Semigroup`; a cats `Group` for a type okay has none for
- [x] ToCatsKernel: `combineAll` and cats' own `Validated` over an okay
      `Monoid` cats never heard of; cats-kernel-laws `MonoidTests` on it

## Decisions

- **Not in FromCats/ToCats — measured.** The first cut put the three
  bridges in those objects. With `okay.given` beside `FromCats.given`,
  `okay.Group[Int]` was AMBIGUOUS: okay's generic numeric `Group[N]` and
  `FromCats.group[A]`, two generic lexical givens with no priority
  between them. A FromCats user importing it for `NonEmptyList`'s monad
  would have lost every numeric monoid. So the kernel bridges are their
  own imports, and the common need, a `Validated` combining with what
  the caller holds, got `Combine`: one owner, a priority chain, no tie
  possible.

## Results

- 179 tests across okay-cats' bridge, class and law suites, green; the
  new ones are TestCombine (3), TestFromCatsKernel (2), TestToCatsKernel
  (2 plus cats-kernel-laws' monoid rules).
