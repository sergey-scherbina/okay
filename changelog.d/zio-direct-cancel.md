## zio-direct-cancel — ZIO in okay's direct blocks, and a cancellable way back

`import okay.zio.given` makes every `ZIO[R, E, _]` okay's `Monad`, so
`direct[Task] { val a = z.?; val b = w.reflect; a + b }` is a `Task` bound
by ZIO's own `flatMap` (stack-safe: 100 000 nested binds). `fromZIO` is now
an `Async.await` on a forked fiber instead of `unsafe.run` in place: the
callback runner parks no thread, and cancelling the okay side interrupts
the ZIO fiber and runs its finalizers. `p.asZIO` / `z.asOkay` are the two
doors as extensions.

Refuted: the prefix mark `!z` on a ZIO value. ZIO has its own deprecated
`unary_!` (Boolean negation), and a member beats an extension, so on ZIO
the marks are `.?` and `.reflect`. Watched red: the old `fromZIO` hung the
"parks no thread" test until the gate's stall watchdog killed it.

Spec: specs/zio-direct-cancel.md. Docs: docs/modules/okay-zio.md. Next:
`direct-foreign-mark`, `zio-typed-row` (sprint queue).
