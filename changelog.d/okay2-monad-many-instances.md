## okay2-monad-many-instances - the monad classes and the ways to tell instances apart, for the Scala 2 core

At the operator's ask (spec stage 13). okay2 gains the Scala 3 core's
Monad.scala: `Functor`, `Applicative`, `Selective`, `Monad`,
`Alternative`, `MonadPlus`, `Comonad`/`Id`, `ParaMonad` (which
`Control` now extends) and `DiagonalMonad`; `traverse`, `sequence`,
`replicateA`, `guard`, `ensure`, `>=>`, `*>`/`<*`/`<*>`/`>>=`,
`whenS`/`unlessS`. Instances need no import: programs in `Free`'s
companion, `Option` in `Functor`'s, `Choose.monadPlus`, `Comonad.id`.
`withFilter` by `CanFail` — an `if` or a refutable pattern prunes in a
Choose row, stops in an Abort row, and does not compile elsewhere, with
the core's message.

And the core's ways to hold several instances of one signature: `Tag`
(`one`/`tag`/`untag`/`handler`), `Instances` (`handle`/`at`/`route`/
`handler`/`only`/`exhausted`), `TypeableK.ByValue` and
`Writer.byValue.writerK` — every Writer handler now takes its
`TypeableK` from the call site so the import reaches it. `Distinct`
reads all three as the Scala 3 macro does (a key, a handle, a by-value
test), and its message no longer recommends `Distinct.unchecked` — which
on a class-tested row is the very ClassCastException it guards against —
but names Tag, Instances, Writer.byValue and Delim prompts.

Found: scalac 2's partial unification reads the `!` alias with its
parameters reversed (`F = [R] Int ! R`), so the generic combinators do
not reach a program typed `A ! R`; program-shaped twins
(`!.traverse`/`!.sequence`/`!.replicateA`, `*>`/`<*`/`>>=`/`ifS` on
every program) do, and TestMonad pins the refusal. docs/okay2.md §15
and §16; the stale "Distinct is not checked" bullet in §8 corrected.
