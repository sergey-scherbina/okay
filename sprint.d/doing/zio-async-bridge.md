## Non-blocking cancellable ZIO bridge

Add an `okay-zio` conversion that drives `A ! Async` through callbacks instead
of parking a ZIO blocking-pool thread. The bridge must propagate ZIO
interruption to the active Okay `Await` registration's canceller, preserve
success and failure values, and leave the existing `toZIO` blocking bridge
available for its current synchronous contract. Specify the public Async
driver cancellation handle before implementation; add focused interop tests
for completion, failure, and cancellation.

Done when the public bridge is documented, its cancellation behaviour is
tested, and the affected ZIO suites pass.
