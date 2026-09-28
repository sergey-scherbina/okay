# okay-diagnose and okay-test — diagnosis for code and tests

## Overview

A failing test here says what it expected and what it got, and nothing
else. Twice in one day (2026-09-27) a whole-build gate failed on a test
that passed alone: SentinelChannel's close-races-offers law, and TestPool's
repeat POST. Both times the diagnosis had to be written into the test
after the fact, and then the failure did not come back to read it. The
fix is not to write the diagnosis for the NEXT failure. It is to have
every test carry it from the start, so the first failure is the one that
explains itself.

The operator's ask (2026-09-27): a standard toolkit in a module of its own,
used everywhere, and grown with every tool that proves useful in testing
or diagnosis.

## Interface

Two modules, split at the operator's word (2026-09-27): diagnosis is not
a testing concern. A flight recorder, a failure's diagnosis and a
component's described state serve library code at run time as much as a
test. Only stress rounds, CPU load and framework adapters are testing.

**okay-diagnose** (package `okay.diagnose`, cross) depends on NOTHING.
Every module can use it in main code.

```scala
final class Flight(capacity: Int)                 // bounded, thread-safe event log
final class Diagnostics:                          // one run's diagnosis
  def note(msg: => String): Unit
  def onFailure(snapshot: => String): Unit        // evaluated ONLY on failure
  def report: String
object Diagnostics:
  def around[A](d: Diagnostics)(body: => A)(using FailureFormat): A
trait FailureFormat:                              // how a diagnosis joins a throwable
  def extend(e: Throwable, diagnosis: String): Throwable
  // ours, the default: suppressed exception, class and cause kept
trait Diagnosable[-A]:                            // a component describes its state
  def describe(a: A): String
extension (d: Diagnostics) def snapshot[A: Diagnosable](a: A): Unit
// JVM
object Threads:    stateOf(t), dump(filter)
object LateOrLost: join(t, first, grace)(snapshot): OnTime | Late(at) | Starved(at) | Lost(at)
```

**okay-test** (package `okay.testkit`, cross) depends on okay-diagnose,
and on munit OPTIONALLY. It is the operator's rule, 2026-09-27: every
dependency sits behind an abstraction and is optional.

```scala
object Munit:                        // the adapter; the only place munit is named
  def missing: Option[String]        // refused by name (JVM; JS/Native link statically)
  given failures: FailureFormat      // withMessage: munit's diff kept
  trait Diagnosed extends FunSuite   // per-test Diagnostics, every body wrapped
  trait LiveTests extends FunSuite   // one spelling of the Live tag
// JVM
object Load:   burners(n)(body)      // every core busy beside the body
object Stress: repeat(n, parallel)(round => Option[diagnosis]): Report
```

## Behavior

- [ ] a passing test pays only its `note`/`onFailure` calls: snapshots are
      by-name and never evaluated
- [ ] a failing test's message ends with the recorder and the snapshots
- [ ] a munit comparison failure keeps its class and diff; the diagnosis
      is appended to its message
- [ ] the recorder is per test: a note from test A never appears in test
      B's failure
- [ ] `Flight` keeps the newest `capacity` notes, in order, from many threads
- [x] `LateOrLost`: a thread that ends inside `first` is OnTime, one that
      ends inside `grace` is Late (with the snapshot taken at `first`), one
      that does not end at all is Lost
- [x] `LateOrLost`: a thread still RUNNABLE at the last deadline is
      Starved, not Lost (sentinel-single-consumer-lost-end, 2026-09-28):
      a liveness law asks whether a wakeup was lost, and a runnable
      thread has not missed one — it has no carrier, which is the
      whole-build JVM's condition, not the channel's. `at` carries both
      snapshots, the first deadline's and the last's.
- [ ] `Stress.repeat` counts failed rounds and keeps the first diagnoses
- [ ] `Load.burners` stops its threads when the body ends, even by a throw

## Adoption

- The three suites that failed in whole-build gates on 2026-09-27 adopt it
  in this lane: TestChannelLaws (its hand-written late/lost becomes
  `LateOrLost`), TestPool (the store's checkpoint saves become the
  failure's history), and TestOwnMonitor (each run's thread count noted).
- `liveTest` is defined in three modules. Those move to `LiveTests` when
  their suites are next touched.
- AGENTS.md: a NEW suite mixes in `Diagnosed`, and an existing one does
  when it is next edited. Every tool invented while debugging a test goes
  into okay-test, not into the test that needed it.

## Decisions

- 2026-09-28 (sentinel-single-consumer-lost-end): `Starved` is its own
  outcome, not a `Late` with a flag and not a `Lost`: the six-channels
  channel law fails on `Lost` only, so a starved consumer in a loaded
  whole build is logged with its two snapshots and the channel's own
  flags instead of turning a gate red with a bare munit timeout — which
  is what every sighting of that law had been, three in one night,
  because the law's own 65 s wait never fit munit's 30 s.

- 2026-09-27: munit is OPTIONAL (`optional;test`) and is named only by
  `okay.testkit.Munit`. The first draft had it as a compile dependency,
  and the operator refused that: every dependency sits behind an
  abstraction and is optional. `FailureFormat` is that abstraction, and
  ours is the default.
- 2026-09-27: `okay.testkit`, not `okay.test`, because that name would
  collide with munit's `test` method.
- 2026-09-27: the failure is extended, not replaced. `FailExceptionLike`
  gets `withMessage`, so munit's diff printing stays. Any other throwable
  gets the diagnosis as a suppressed exception, so the cause and class a
  caller matches on are unchanged.
