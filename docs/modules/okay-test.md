# okay-test

Test tooling over [okay-diagnose](okay-diagnose.md). munit is an
OPTIONAL dependency (`% "optional;test"`): the module compiles against
it, and `Munit.missing` names the jar when it is absent instead of
failing with a `NoClassDefFoundError`.

| | |
|---|---|
| `Munit.Diagnosed` | mix into a `FunSuite`: every test gets a fresh `Diagnostics`, and a failing test's message ends with its diagnosis |
| `Munit.failures` | munit's `FailureFormat`: appends to a `FailException`'s message, keeping its class and diff |
| `Munit.LiveTests` | `liveTest(name)` tags a test `Live` (out of the default gate) |
| `Load.burners` (JVM) | CPU burners around a body, stopped in `finally` |
| `Stress.repeat` (JVM) | runs a round many times, in parallel, keeping the first few diagnoses |

## Using it

```scala
class TestPool extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
```

Inside a test, `note(...)` records and `onFailure(...)` registers a
snapshot. A passing test never evaluates the snapshot. A failing one
prints, under munit's diff:

```
--- diagnosis ---
snapshot:
  checkpoint saves, in order:
    +21ms [pool-214-thread-14] myrun.meta e=0 56 B
    +66ms [] myrun e=1 83 B
```

A flake is measured, not guessed:

```scala
    val r = Stress.repeat(100, parallel = 4, keep = 2)(i => if i % 10 == 0 then Some(s"bad $i") else None)
    assertEquals(r.failed, 10)
```

## Literature

- munit's `TestTransform` (the hook `Diagnosed` uses) is documented in
  munit's "Customizing tests" guide.
