# cats-kernel-bridge — Semigroup, Monoid, Group across cats and okay

Status: in progress, 2026-10-02. Owner lane: `cats-kernel-bridge`.
From the cats-depth audit (backlog okay-cats).

## Goal

okay's `Semigroup`/`Monoid`/`Group` (Fold.scala) and cats-kernel's are
separate classes, so `catsValidated` asks okay's `Semigroup` and
`okayCatsValidated` cats'. okay has no `Eq`/`Order`, so nothing to
bridge there.

## Interface

In the existing one-direction objects, by the same rule (never both):

- `FromCats`: okay's `Group` ⇐ cats' `Group`, `Monoid` ⇐ `Monoid`,
  `Semigroup` ⇐ `Semigroup`, in that priority;
- `ToCats`: cats' `Group`/`Monoid`/`Semigroup` from okay's.

## Behavior

- [ ] FromCats: `okay.Validated` accumulates with ONLY a cats `Semigroup`
      in scope (a `NonEmptyList`), through cats' and okay's `traverse`
- [ ] ToCats: cats' `Validated` accumulates with only an okay `Semigroup`;
      `combineAll` over an okay `Monoid` cats never heard of
- [ ] a type both sides have (`String`) resolves without ambiguity under
      either import, with `okay.given` in scope too
- [ ] cats-kernel-laws' `MonoidTests` on a bridged okay monoid

## Decisions

## Results
