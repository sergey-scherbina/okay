# pattern-binds — a step the program may decline

## Overview

`for case Some(old) <- find(id)` is the shortest correct spelling of a
lookup that must not continue when there is nothing to look up, and it
did not compile. This spec is the design that made it compile, and the
one decision underneath it: what a for-comprehension pattern MEANS
when it does not match.

It arrived from a bug. The custom-effect demo's `rename` said `find`
then `save` in a for-comprehension; a for-comprehension SEQUENCES and
does not branch, so a missing id still reached the handler's upsert and
created the user. The demo printed the bug in its own output for a day
before anyone read it: `MISS None / row 99 is now hopper`. A fold fixes
that program. It does not fix the class of it — the branch is still
written by hand, and the day someone forgets it the type says nothing.

## Decisions

- **A refutable pattern and an `if` guard are the same question, and
  it is not about patterns.** Scala desugars both into `withFilter`.
  So what a program needs in order to write either is the right to
  DROP a step. A plain `A ! F` has no such right — nothing in `Free`
  declines to answer — and must not pretend to: a silently skipped
  step is a bug that reads like a feature, which is exactly the bug
  this began with.

- **The evidence is MEMBERSHIP of an effect that can fail, not
  `MonadPlus`.** Shipped first as `MonadPlus[[X] =>> X ! F]`, which
  looked right and is not: the instance for `Choose + F` never matches
  a concrete row, because unifying `[A] =>> Choose[A] | F[A]` against
  `[A] =>> Choose[A] | (Writer % String)[A]` is a higher-order
  unification the compiler declines — it finds the given and reports
  "does not match". Measured on `Choose + Writer % String`, which is
  the shape anyone actually writes; the bare `Choose` row it did serve
  is the row nobody writes. `In[Choose, F]` — the same membership
  witness `.at` uses (specs/writer-covariance.md, rowlift) — resolves
  where that fails, and says less: containment, not a whole algebra.

- **Two effects can fail, and they mean different things.**

  | row carries | a failed pattern means | the handler answers |
  |---|---|---|
  | `Choose` | this BRANCH dies, the search goes on | `runChoice`, the branches that matched |
  | `Abort` | the PROGRAM stops | `runOption`, `None` |

  A lookup has no other branch to continue into, so pruning is the
  wrong meaning for it; a search has, so stopping is the wrong meaning
  there. Both instances exist and the row picks.

- **Where a row carries both, `Choose` wins** (`viaChoose` in
  `CanFail`, `viaAbort` in the low-priority parent). In a searching
  row `guard` already means prune, and one syntax must not mean two
  things in one row. Tested: `Choose + Abort` keeps the branches that
  matched rather than answering `None` for the whole search.

- **`Abort` is `Throws % Unit`, and needs no new machinery.** Not
  every failure carries a reason — a lookup that found nothing, a
  pattern that did not match, a guard that did not hold — so the error
  type is `Unit` and the only new names are `abort` and `runOption`
  (`runEither(...).map(_.toOption)`). No new node, no new interpreter,
  no new handler.

- **The Option MOVES, it does not disappear.** `rename` stops being
  `Option[String] ! Users` and becomes `String ! (Users + Abort)`; the
  Option comes back at the end as `runOption`'s answer. That is the
  gain worth having: `save` cannot run for a missing id because it is
  not REACHABLE, not because a branch remembered to skip it.

## Results

The demo, unchanged in output and shorter in every line that matters:

    type Renaming = Users + Abort

    def rename(id: Long, to: String): String ! Renaming =
      for
        case Some(old) <- Users.find(id).at[Renaming]
        _              <- Users.save(id, to).at[Renaming]
      yield old

    MISS  None / row 99 is now - / both worlds agree: true / log=find(99)

`log=find(99)` with no `save` is the whole claim, checked by the
recording handler rather than by reading SQL.

- [x] a pattern binds in a row containing `Abort`, and the following
      step does not run
- [x] the value passes through when it matches
- [x] an `if` guard is a precondition in a row that can stop
- [x] a pattern binds in a row that merely CONTAINS `Choose`
      (`Choose + Writer % String`) — the case the first design could
      not do
- [x] a row carrying both prunes rather than stops
- [x] a row that can do neither is a compile error, and the message
      names both ways to fix it and the fold that needs neither

## Refuted

- **`MonadPlus` as the evidence** — see above. It is not that
  MonadPlus is wrong about the algebra; it is that the instance cannot
  be FOUND for the rows people write.
- **Making `Free` fail on its own** (a `withFilter` that raises a
  `MatchError` when the row cannot express failure). It would compile
  everywhere and lie everywhere: the type would stop saying whether a
  program can stop, which is the only thing this feature is for.

## Open

- `guard` outside a for-comprehension still asks for `MonadPlus`, so
  it does not work in an `Abort` row. A `CanFail`-shaped counterpart
  is two lines; it is not written because the `if` in a
  for-comprehension already covers every use so far.
- `Alternative` for `Abort` rows — `append` as recovery, `x <|> y` —
  is implementable (`runEither(x).at[R].flatMap(_.fold(_ => y, pure))`,
  probed) and not shipped: nothing needs it yet, and it installs a
  handler per `append`.
