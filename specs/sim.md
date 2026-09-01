# Deterministic simulation: interleavings as a value

## Overview

On 2026-09-01 this repository found three real concurrency bugs in
one day: `runCmd` lost command answers to a close race, two
H2-carrying modules raced DriverManager's per-classloader driver
registry, and a family of port/readiness races filled a flake
ledger. Every one was found by a FLAKE — a test that happened to
lose the coin toss on a loaded machine — and none by design. A
flake is luck wearing a lab coat: it proves the bug exists and
teaches almost nothing about where, and its fix is verified by the
same coin toss that missed the bug for weeks.

The systematic answer is the one FoundationDB made famous:
DETERMINISTIC SIMULATION. Run the whole concurrent system — many
fibers, channels, clocks, failures — inside one single-threaded
scheduler whose every choice (who wakes next, which message is
delayed, when a clock fires) is drawn from a SEED. Then an
interleaving is a value: a found bug is a seed, a fix is verified
by replaying that seed, and a night of CI explores millions of
interleavings instead of the handful the OS scheduler happens to
serve.

This stack is unusually close to having it, because its effects are
already reified: a fiber is a freer-tree program, its suspension
points are OPERATIONS (`Async.Run`, `Async.Await`, `Channel`
send/receive under them), and multi-prompt delimited control
(`Delim`, Dybvig–Peyton Jones–Sabry) is precisely the machinery
for holding many suspended programs and choosing which to resume:
each simulated fiber runs under its own `Prompt`; reaching a
scheduling point SHIFTS to the scheduler; the scheduler holds the
captured continuations and the seed decides which one runs next.
The user asked whether Delim was useful beyond streams — this spec
is the answer written down.

## The model

- **SimFiber**: a program `A ! F` adopted by the simulator; its
  identity is its Prompt. Spawning inside the simulation creates a
  new prompt under the same scheduler.
- **Scheduling points**: where a fiber can lose the CPU. The
  simulator intercepts the ASYNC layer — `Async.Run` (runs the
  thunk, then yields), `Async.Await` (parks until some other fiber
  completes the registration), and the sleep/timer path (which
  advances the VIRTUAL clock instead of waiting). Channel
  operations need no special casing: on the JVM leg they live
  inside Run/Await thunks already; the SIMULATED channel is a pure
  reimplementation over the scheduler (park-on-empty,
  park-on-full) so sends and receives become visible scheduling
  points. One decision recorded (room discussion 2026-09-01):
  intercept at the ASYNC operations, not above them — it is the
  narrowest waist every concurrent primitive already passes
  through.
- **The virtual clock**: `sleep` and lease/timeout logic read
  simulated time; the scheduler advances it only when every fiber
  is parked (the classic rule: time moves when nothing else can).
  Code under test takes its clock as a parameter — the house
  already does this (`Election`, `Cache.memory`), which is why it
  is simulable at all.
- **The seed drives**: among runnable fibers the scheduler picks
  by seeded PRNG; optional fault plans (drop this send, delay that
  wakeup, kill fiber N at step k) come from the same seed. A run's
  RESULT is `(seed, outcome, trace)`; a failing seed reproduces
  byte for byte.

## What it is not

- Not a model checker: it samples interleavings, it does not
  exhaust them. Sampling with seeds still beats the OS scheduler
  by orders of magnitude, and a found seed never rots.
- Not a new runtime: production code keeps the real `Async`
  handlers; the simulator is a TEST-scope interpreter of the same
  operations. Code changes required of the system under test:
  none beyond the clock-as-parameter discipline already in force.
- Not cross-platform initially: the JVM leg first (that is where
  the concurrent machinery lives); the pure parts run anywhere.

## Behavior

- [x] a seeded run of N fibers over a simulated channel is
      REPRODUCIBLE: same seed, same interleaving, same trace —
      asserted on a scenario whose outcome depends on order
- [x] different seeds explore different interleavings (a
      counter-scenario whose outcomes differ by order shows both
      outcomes across a seed sweep)
- [x] the virtual clock: a fiber sleeping 5s and a fiber sleeping
      1s wake in order with NO wall time spent; a lease-expiry
      scenario (Election's) runs entirely on simulated time
- [x] the runCmd close race REPRODUCES under some seed within a
      bounded sweep — the regression test for the bug this spec
      was born from — and the fixed code survives the same sweep
      (at the MODEL level, honestly: the close-rule protocol
      modeled over SimChannel; the old rule loses the answer, the
      shipped rule survives 200 seeds; porting the real runCmd
      under the sim is the next sophistication)
- [x] a fault plan from the seed (delay one send) flips a
      scenario's outcome, and the seed replays it
- [x] the simulated channel passes the real Channel's contract
      shape (send/receive/close/drain semantics) under the
      scheduler

## Out of scope

- exhaustive model checking / partial-order reduction — a later
  sophistication over the same capture points
- simulating the wire protocols' byte level — the simulator's
  fault plan drops/delays MESSAGES (operations), not TCP segments
- distributed multi-process simulation — one process, many
  simulated nodes, which is exactly how FoundationDB does it too

## Decisions

- **Capture at the Async waist, via Delim** — every concurrent
  primitive in this stack already narrows to Run/Await; prompts
  hold suspended fibers first-class, and multi-prompt capture
  across intervening delimiters is the one machinery that lets a
  fiber suspend THROUGH its own handlers (the exact property Delim
  was built for; the dialog-delim scopes landed the same day prove
  prompt-per-scope composes). Rejected: instrumenting Channel and
  every future primitive one by one (a treadmill); rejected: byte
  code/agent-based interception (a dependency and a platform).
- **The clock is a parameter, everywhere** — already house
  practice; the simulator is why it must stay that way. Rejected:
  a mockable global clock.
- **Sampling over exhaustion** — millions of seeded runs a night
  beat both the OS scheduler and the tractability wall of
  exhaustive checking. Rejected v1: model checking (revisit if a
  bug class demands it).

## Results

Landed (sim-harness, 2026-09-01): `Sim` in the core, pure and
cross-platform-shaped (a seeded Random, a virtual clock, freer
trees — nothing else). One lesson the first run taught: a
continuation must be applied when its fiber is SCHEDULED, not when
it is enqueued — eager k-application ran map-closures' side
effects at park time and reordered the world; every task is a
thunk now. Deadlock is an OUTCOME (a mutual-receive scenario
returns Deadlock(2) instead of hanging). The headline: the runCmd
close race, modeled, LOSES the answer under seeds found within a
200-sweep and the shipped rule survives all 200 — the day's
flake became a replayable regression test, which is the entire
point of the module. Eight tests. Next sophistications, filed
with the slug left open: porting real components (runCmd itself,
Replicated/Election) under the sim via a simulated Async handler,
and the multi-prompt Delim road when fibers carry inner
delimiters.
