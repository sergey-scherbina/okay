# okay-diagnose

What to say when something fails: a flight recorder of recent events, a
snapshot of a component's state taken only on failure, and the two
attached to the exception that leaves. It has no dependency and runs on
the JVM, Scala.js and Scala Native. It is not a test library; a test
framework is one consumer ([okay-test](okay-test.md) is munit's), a
service's error path is another (specs/okay-diagnose.md).

| | |
|---|---|
| `Diagnostics` | per-run notes plus lazy failure snapshots; `report` renders them |
| `Diagnostics.around` | runs a body and, if it throws, extends the exception with the report |
| `FailureFormat` | how a report is attached; the default adds it as a suppressed `Diagnosis`, keeping the class and cause |
| `Flight` | a bounded, thread-safe log: `+ms [thread] message`, and how many older notes fell off |
| `Diagnosable[A]` | a typeclass for "describe your state"; `d.snapshot(a)` records it |
| `Threads` (JVM) | one thread's state and top frames, or a filtered dump |
| `LateOrLost` (JVM) | joins a thread and tells a slow one (`Late`) from a lost one (`Lost`), with its state at the first deadline |

## Using it

```scala
    val d = Diagnostics()
    d.note("offered 3")
    d.onFailure("state: closing=true")
```

A snapshot is by-name and evaluated only when the body fails, so an
expensive one costs nothing on the passing path. A component can carry
its own description:

```scala
    given Diagnosable[Gauge] = Diagnosable.of(g => s"gauge=${g.n}")
    d.snapshot(Gauge(7))
```

`Flight` keeps the newest notes in order, which is what a race needs:
okay-pool's tests record every checkpoint save and print them when an
assertion fails.

```scala
    onFailure(s"checkpoint saves, in order:\n${store.saves.dump}")
```

## Literature

- The flight-recorder idea is JDK Flight Recorder's (JEP 328): always
  on, bounded, read after the fact.
- `Throwable.addSuppressed` (Java 7, JLS 14.20.3) is how the report
  rides along without replacing the original exception.
