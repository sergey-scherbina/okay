## ci-runner-shape-flakes - two core tests that only fail on a 4-vCPU runner

The push CI on 8016e3b6 was red on two tests the local whole-family
gate had passed twice that night, and neither was about the code
that landed. `TestSupervised` took its fibers to the common
`ForkJoinPool`, whose parallelism on GitHub's 4-vCPU runner is 3: the
three sleeping children held every thread and the child that throws
waited in the queue behind them for the full 3 s. Reproduced on JDK
21 with `-Djava.util.concurrent.ForkJoinPool.common.parallelism=3`;
the suite owns a 16-thread pool now. `TestMonadic`'s "modest chain"
of 1_000 strict binds overflowed the runner's 1 MB default stack —
and `-Xss512k` cold on this box — so it was one JIT decision from
red on any JVM; the chain is 200 binds, which is what "modest"
means, and the spec says so. Neither test ran on CI since the
nightly JVM family last completed, which is why the push that
touched the core was the first to see them.

Landed 2026-09-25 from a claim idle since the 20th, after `TestMonadic`
failed the push CI again that morning. Both failures re-reproduced on
today's tree first, and both fixes gated under the same conditions:
`-Xss512k` for the core suite, and for `TestSupervised` the common
pool at parallelism 3 — which needs `set ... okayPlatformJVM / Test /
fork := true` as well, because okay-platform runs its tests inside
sbt's own JVM and a `-D` in `javaOptions` never reaches them (the
first control run "passed" for exactly that reason).
