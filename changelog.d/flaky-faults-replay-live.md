## flaky-faults-replay-live — TestFaults' replay-by-seed law moved to integrationTest

The composite-under-a-drawn-plan law in okay-resilience's TestFaults went
red in a ci-runner whole build under load and green alone, holding the
push (after flaky-suites-live cleared the three before it). The run is not
a pure function of its seed — its 1 s budget and 2 ms slow calls use the
wall clock — so the test is `Live`-tagged with that reason in place, and
the fix (an injectable clock for the budget, driven virtually) is backlog
okay-core/faults-replay-wall-clock.
