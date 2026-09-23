# row-inference-ergonomics — what a row's type actually needs, verified

## Overview

A day's worth of core code spelled row unions by hand at several
sites, and the backlog entry that filed this lane wrote down what the
friction FELT like from memory. Tested before writing anything else:
one of its own claims — "`Once + Async + Pure` written out because
`X + Pure` and `X` do not unify without help" — does not appear
literally anywhere in the tree (checked by grep), and the underlying
claim is FALSE once probed (`ProbeRowInference`, shape 1 and its
reverse): a bare `X` satisfies an `X + Pure` slot, and an `X + Pure`
satisfies a bare `X` slot, both by plain ascription, no `.at` at all.
This spec keeps only what direct evidence from today's own work
supports, each pinned as a `compileErrors` probe rather than asserted.

## Behavior

- [x] **`.at`/`.plus` need an explicit import, even inside package
      `okay` — the single most common trap of the day, hit five
      times, and the error it produces (a missing `Free.directColor`
      `DirectCtx`) never mentions the missing import
- [x] **`X` satisfies an `X + Pure` slot, and the reverse, by plain
      ascription** — REFUTES the backlog's own framing; no `.at`
      needed either direction (`ProbeRowInference`, shape 1 and its
      reverse test)
- [x] **`flatMap` between two different effects needs BOTH operands
      widened to the same row** — the real, frequent trap; widening
      only the receiver (or only the continuation) still fails
- [x] **a method expecting `R ! (F + G)` does not recover that shape
      from an argument already typed as the expanded union
      `[A] =>> F[A] | G[A]`**, without explicit type arguments at the
      call site — `Delim.Stacked`'s `push[R, F](...)`/`run[R, F](...)`
      needed exactly this fix; reproduced minimally
- [x] **a union's ACI is FULL, not just associativity**: re-
      parenthesizing `(F + G) + H` into `F + (G + H)`, AND swapping
      the order of two members (`F + G` into `G + F`), both satisfy a
      plain ascription with no `.at` — dotty's `|` normalizes a
      union's members for type equality. The first draft of this
      spec's own probe assumed commutativity did NOT hold and was
      wrong; corrected against the compiler, not left standing.

## Out of scope

- **The spike this lane's item (3) asked for — a match type or
  `given` normalizing `X + Pure` to `X` — is ANSWERED as unnecessary,
  not built.** The friction it would have fixed does not exist: shape
  1 above shows the compiler already accepts both directions with no
  help. Building a normalizer for a problem that is not there would
  be a solution in search of a problem.
- **A one-line overload following `SharedOnce.run`/`runIn`'s
  pattern**, item (2) of the filing entry — `SharedOnce` already IS
  this pattern (`run` for the narrow row, `runIn` for the general
  one), cited as the example rather than something to add to. No
  OTHER site repeating the same spelling was found; if one turns up,
  the pattern is already named and ready to copy.

## Decisions

- **Every claim in this spec is pinned as a `compileErrors` probe**
  (`ProbeRowInference.scala`, kept permanently, matching the
  `ProbeRowCrash.scala` precedent AGENTS.md names) rather than stated
  in prose alone — the backlog entry this lane answers was ITSELF
  wrong on its most specific claim, which is the whole argument for
  never repeating that mistake.
- **docs/typepedia.md's "Recurring gotchas" gained a full section**
  rather than a new page: the six (now five, one refuted) shapes are
  short enough to sit beside the existing gotcha list, and splitting
  them into their own page would separate two things that answer the
  same question ("what do I need to spell by hand").

## Results

Landed as written. `ProbeRowInference` (9 tests, all green): the
missing-import trap, the flatMap-both-sides trap, the union-argument-
recovery trap — each verified — and the ACI/Pure claims, TWO OF WHICH
turned out to be non-issues once tested rather than the friction the
filing entry assumed. docs/typepedia.md's new section states only
what is now verified.
