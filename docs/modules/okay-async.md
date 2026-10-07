# okay-async

The portable `Async` effect and its callback-based runtime semantics:
`Run`, `Await`, `Handoff`, `Par`, `Retry` and the `Failing[Async]`
instance that lets a forwarded `Async` operation report failure to the
scope that forwarded it.

It deliberately supplies no platform default instances — no
`CanBlock`, no `Scheduler`. Those are `okay-platform`'s job, on each
of the three targets it actually runs on.

## Using it

```scala
lazy val myModule = (project in file("my-module"))
  .dependsOn(okay.jvm, okayAsync.jvm)
```

Reach for it when you want to describe asynchronous programs — build
`Fiber`s, race, retry — without committing to a runtime yet. Add
`okay-platform` (or your own `CanBlock`/`Scheduler`) when you actually
need to run one.

## On the machine

`AsyncCont` is Async for the core's `A ! R` (the machine's programs,
`import okay.*`): the same `Async.Run`/`Async.Await` operations,
`async`/`await`/`awaitEither` as `Op`s, `blocking` (an `Answering`
handler: each operation in place, no capture) with `run`, the callback
drive `runAsync` (a Run and an Await answered during its registration
in place, only a pending Await stops the machine — 1.15x the classic
drive), and the two
bridges to the classic tree, `toClassic` and `fromClassic`, one crossing
per program. Not yet there: `attempt`, cancel scopes, fibers, the
poll-then-park drive (backlog: cont-first-module-findings).

