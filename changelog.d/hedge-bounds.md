## hedge-bounds - the hedge tests assert hedging, not the scheduler

`TestHedgeStart` has failed four landing gates from four lanes that
could not have caused it — dataflow-numbers, continuations-audit,
workflow-suspended-driver, and today a lane whose entire diff is one
line of `build.sbt`. Its own entry has said since the third sighting
that the owner's choice was overdue, and priced the waiting at five
gate cycles. Option (a) is taken: assert the CONDITION, not the
deadline; assert BOUNDS, not exact counters.

WHAT WAS WRONG. `until` spun on `Thread.yield()` against a
FIVE-SECOND wall clock and then failed. Beside ninety other module
runs that is not a statement about hedging, it is one about the
scheduler. Two things changed:

  - the budget is a TRIPWIRE, not a claim. Sixty seconds is the line
    past which "slow" has become "hung"; the assertion is still the
    condition, and the clock only says when to stop waiting for it.
  - it SLEEPS instead of spinning. `Thread.yield()` on a loaded box
    can hand the core straight back to the one thread that has
    nothing to do; a millisecond of sleep gives it to the fibre being
    waited on.

`TestResilienceTimed`'s hedge counters become what hedging promises:
a hedge happened (`starts >= 2`) and EVERY LOSER IS CANCELLED
(`cancelled == starts - 1`). That last one is the only assertion in
the test that is about hedging rather than about timing — the old
`starts == 2` fails whenever a loaded box lets a third attempt start,
with hedging working perfectly.

A CORRECTION TO THE ENTRY, found by running it: it names both suites
as though both reached the gate. `TestResilienceTimed` is already
`Live`-tagged and excluded from `sbt test`, so all four sightings are
`TestHedgeStart` alone and the timed suite's change is an improvement
rather than a fix.

AND A SECOND REFUTATION OF THE BURNER THEORY, recorded because it
cost an hour and the entry had already warned about it. 16 burners at
load 93 did not reproduce the failure with the OLD code either: 0 of
4, against 0 of 4 new, with the detector verified on a known-good run
BEFORE the comparison. (The first attempt at that comparison was
worthless in a way worth naming: it grepped for `Failed: Total 0`, a
line sbt never prints on success, so every round read as a failure
and both columns said "4 of 4". An instrument that cannot see a pass
cannot see a regression either.) CPU pressure is not the condition.
Every sighting was inside a full matrix, which is why the entry's
DONE is a matrix and not burners.

Accepted the way the entry asks: the full affected matrix three times
back to back, all GREEN, with the suite in every one.

AND WHAT THAT DOES NOT SHOW, because the criterion says "under load":
those three ran beside a docker image build and sibling landings —
ordinary traffic, not the four-matrix crush every sighting came from
— and the old code also passed 4 of 4 at load 93, so no run of this
size discriminates. What carries the change is that the assertions no
longer name the scheduler. Three green matrices say it did not
regress; if a fifth sighting ever comes, it will be a claim about
hedging that failed, which is worth reading.
