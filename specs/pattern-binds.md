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

    def rename(id: Long, to: String): String ! (Users + Abort) =
      for
        case Some(old) <- Users.find(id).plus[Abort]
        _              <- Users.save(id, to).plus[Abort]
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

## Also landed

Both were listed here as open and cost about the length of their own
doc comments, so they went in together.

- **`ensure[R](cond)`** — the `if` guard, outside a
  for-comprehension. `guard` (Monad.scala) is the MonadPlus one and
  serves any carrier with an algebra; this one serves a ROW, known by
  membership, which is how every real row is known. Same three
  meanings as the pattern: prune, stop, or a compile error.

        for
          balance <- State.get[Int].at[R]
          _       <- ensure[R](balance >= amount)
          _       <- State.set[Int](balance - amount).at[R]
        yield balance - amount

  Refused, the state proves it: `(10, None)` — the `set` was never
  reached, not skipped.

- **`p.recover(h)` and `p.orElse(q)`** — the Alternative structure of
  a failing row (`empty` is `abort`, `append` is this), spelled as
  METHODS rather than as an instance, for the same reason `CanFail` is
  not MonadPlus: an `Alternative[[A] =>> A ! (Throws % E + F)]` would
  never be found. A method's receiver is unified rather than searched
  for, and that does work — including with the row written in the
  other order, which is the part that was not obvious and is now
  tested. Both are `runEither` applied to a PART of the program: the
  handler is installed there, the failure is answered there, and the
  row comes out unchanged, so what follows neither knows nor cares.
  Not free — one handler per call — so wrap the smallest piece that
  can fail.

  Written for `Throws % E` generally, not for `Abort` alone: `orElse`
  is just `recover` where the error had nothing to say.

## Open

- Nothing outstanding on this feature. The one thing deliberately NOT
  done is an `Alternative`/`MonadPlus` INSTANCE for failing rows: it
  would state the algebra, and implicit search would never find it for
  a row anyone writes. The methods say the same thing and resolve.

## declaring an effect (2026-09-08)

The boilerplate an effect used to carry was three things: the
operations, a `TypeableK` instance, and a constructor per operation.
Two of them are gone.

- **`derives Effect`** is what a signature says about itself, and it
  is the one to write. `Effect[F] extends TypeableK[F]`, so everything
  that asks for the row-split test finds this instance in the
  signature's own companion, and the declaration reads as a
  declaration rather than as a mechanism. It is a trait and not an
  alias so that it has room: whatever joins it later has to be
  DERIVABLE from the declaration alone, which rules out most things
  and is the point.

  One candidate is deliberately out: `Direct.Effect`, the marker that
  lets a signature's operations auto-color inside a `direct` block.
  Bundling it would be convenient and would quietly move a decision
  the design put elsewhere — auto-coloring is gated per PROJECT, not
  per library (specs/direct-auto-coloring.md), and an effect's author
  would be deciding it for every consumer.

- **`derives TypeableK`** (which `Effect.derived` delegates to) writes
  the instance. No macro: a
  `ClassTag[F[Any]]` IS the erasure of F, which is exactly what
  `typeableK` wants, and the compiler synthesizes it for any concrete
  signature. Same instance as the hand-written
  `typeableK(classOf[Users[?]])`, same totality — complete when the
  answer type is the signature's only parameter, partial for
  `State % S` and friends, which say so themselves. Tested by
  splitting a real row with `relay`, since splitting is what the
  instance is FOR.

- **`op.perform`** removes the constructors. The answer type comes
  from the CASE — `Find` extends `Users[Option[String]]` — so
  unifying the receiver against `F[A]` recovers both the signature and
  what it answers, with nothing written twice.

        enum Db[+A] derives TypeableK:
          case Get(k: String) extends Db[Option[Int]]
          case Put(k: String, v: Int) extends Db[Unit]

        Db.Get("a").perform   :  Option[Int] ! Db

  Named constructors are still worth writing for an effect other
  people will use: they are its API and read better at every call
  site, at one line each. The demo keeps its two for that reason and
  says so.

- **What `TypeableK` is FOR, since the question keeps coming.** A row
  is an untagged union (`[A] =>> F[A] | G[A]`) and unions ERASE, so
  when a handler for F meets an operation in a row `F + G` it has to
  decide at run time whether that operation is its own or must be
  forwarded. There is no tag to read: the decision is a class test,
  and `TypeableK` is that test. The alternative — a tagged coproduct,
  `Inl`/`Inr` — needs no test but allocates a wrapper per operation,
  which is exactly the allocation `rowlift` spent its effort deleting.
  So the cost of the cheap representation is one `isInstance` per
  dispatch, and one instance per signature.

- **An effect with NOTHING declared already works**, and this is worth
  knowing before writing any of it down: the generic instance in
  `TypeableK`'s companion derives from a synthesized
  `Typeable[F[Nothing]]`, which is the same class test. Measured: the
  row splits correctly with no companion, no given, no derives. The
  only thing it costs is a warning, "the type test for Db[Nothing]
  cannot be checked at runtime", at every use site — unactionable
  where it appears and, in a build that keeps zero warnings,
  intolerable. That warning IS the whole reason effects declare an
  instance, and `derives TypeableK` is how to stop paying it.

- **Not done: making it fully automatic** by giving `TypeableK` a
  ClassTag-based instance and dropping `derives` too. It is unsound
  for a ROW, and the measurement is the argument: `ClassTag[(Choose +
  Writer % String)[Any]]` is `interface java.io.Serializable` and
  `ClassTag[(Db + Writer % String)[Any]]` is `interface
  scala.reflect.Enum` — LUBs that every operation in the program
  matches. An automatic instance would shadow the correct generic one
  and send every operation left in silence. So `derived` is a macro
  for exactly one reason: it REFUSES a union, at compile time, with a
  message saying a row needs no instance of its own. A blacklist of
  such classes was tried first and is whack-a-mole — the two LUBs
  above are already different.

- **Not done: generating the constructors.** It needs
  `MacroAnnotation`, which is experimental, and would make every user
  of the library experimental with it. `perform` gets the same
  boilerplate to zero without that price.

- **`h.tracing(log)` makes any handler a recording one.** The
  operations are already data, so "what did this program ask for, and
  in what order" is a decorator, not a second handler that can drift
  from the first. The demo's test handler stopped writing its own log
  strings, and its MISS line now traces the SQLite handler — recording
  is not a test-only trick.

- **`perform` applies to any `F[A]`, and that turns out to be a
  feature.** The first draft of this section called `List(1, 2)
  .perform` a meaningless thing that compiles. It is not: a freer
  monad takes ANY type constructor as a signature, and a `List[A]`
  already means "several A", so that expression is nondeterminism
  without a wrapper.

        val pairs: (Int, Int) ! List =
          for
            x <- List(1, 2).perform
            y <- List(10, 20).perform
          yield (x, y)

        runSeq[List, (Int, Int), Pure](pairs)
        // Seq((1,10), (1,20), (2,10), (2,20))

  `runSeq` is `runChoice`'s handler unchanged, because there was never
  anything else in it: `Choose[+A](as: Seq[A])` is a box around a Seq
  of alternatives, and this is the same thing without the box. An
  empty list prunes, as failure should. `Choose` keeps its name and
  its place in a row — a row wants a signature that means
  nondeterminism and nothing else, while `List` in a row means
  whatever the reader guesses — but the two being one handler is the
  clearest statement of what this library is.
