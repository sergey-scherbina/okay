# okay-platform

The concrete JVM, JavaScript and Native runtimes for `okay-async`'s
effect, plus the system facades built on them: `CanBlock` and the
schedulers (JVM: Loom-backed; Native: wait/notify; JS: none — a
blocking join is not offered there), `Net`, `Scoped`, and the
per-platform `Platform` object each target gets.

This is where "the code compiles" becomes "the code runs": `okay` and
`okay-async` describe programs without committing to a target, and
this module supplies the given instances a target needs to execute
them.

## Using it

```scala
lazy val myModule = (project in file("my-module"))
  .dependsOn(okay.jvm, okayAsync.jvm, okayPlatform.jvm)
```

Reach for it whenever a program is actually going to run rather than
just being described — a test that calls `.runWith`, a service that
blocks a thread, anything that needs `CanBlock` or a `Scheduler` in
scope.
