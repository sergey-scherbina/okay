# okay-test — the testing and diagnostics toolkit

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

Module `okay-test`, package `okay.testkit`, cross (JVM/JS/Native), depending
on munit alone, so any module's tests can use it, the core's included.
It is not `okay.test` because that would collide with munit's `test`
method.

```scala
// shared
trait Diagnosed extends munit.FunSuite:
  def note(msg: => String): Unit                 // this test's flight recorder
  def onFailure(snapshot: => String): Unit       // evaluated ONLY if the test fails
  // on failure the message gains: the recorder (newest last) and every snapshot

final class Flight(capacity: Int):               // bounded, thread-safe event log
  def note(msg: String): Unit
  def dump: String

trait LiveTests extends munit.FunSuite:          // one spelling of the Live tag
  def liveTest(name: String)(body: => Any): Unit

// JVM
object Threads:
  def stateOf(t: Thread, frames: Int = 12): String   // state + stack
  def dump(filter: Thread => Boolean): String

object LateOrLost:                                // "slow under load" vs "never"
  def join(t: Thread, first: Duration, grace: Duration)(snapshot: => String): Outcome
  enum Outcome { case OnTime; case Late(at: String); case Lost(at: String) }

object Load:
  def burners[A](n: Int = cores)(body: => A): A   // every core busy beside body

object Stress:
  def repeat(n: Int, parallel: Int = 1)(round: Int => Option[String]): Report
  // a round answers None when it held, Some(diagnosis) when it did not
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
- [ ] `LateOrLost`: a thread that ends inside `first` is OnTime, one that
      ends inside `grace` is Late (with the snapshot taken at `first`), one
      that does not end at all is Lost
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

- 2026-09-27: munit is a COMPILE dependency of okay-test, since a test
  library's API is munit's. Modules depend on it `% Test`.
- 2026-09-27: the failure is extended, not replaced. `FailExceptionLike`
  gets `withMessage`, so munit's diff printing stays. Any other throwable
  gets the diagnosis as a suppressed exception, so the cause and class a
  caller matches on are unchanged.
