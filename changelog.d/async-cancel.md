## Async on the machine: cancellation, cancel scopes, attempt

Lane async-cancel. `AsyncCont.runAsyncCancellable` is the machine's callback
drive with the classic's cancellation semantics (stop at the next operation,
unregister the pending Await, release the open scopes, fail the future with
`CancellationException`); `AsyncCont.enter`/`exit` open and close an
`Async.CancelScope`; `AsyncCont.attempt` runs a program as a unit of its own,
its failure a `Left`, cancelled with the program around it. The drive polls an
Await once before registering. AsyncDriveBenchmark.contRunAsync10k: 278 us,
unchanged within noise. Also: the JMH sources' unused imports (left by the
okay-std move) are gone.
