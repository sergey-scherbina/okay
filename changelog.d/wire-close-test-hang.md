## wire-close-test-hang - a Live end-to-end test hung, undetected, on the client side

`wire-server-close`'s new "end to end, SERVER-initiated" test in
`TestWire` was never actually run before it landed — `sbt test`
excludes `Live`-tagged suites by default, and this one is. Run for
real with `sbt integrationTest`, it hung forever: `feed.close()` does
not end `Wire.client`'s `forward` loop, which only ever stops on its
own `Event.Closed`, so `fiber.join()` parked with no timeout. The fix
sends `Closed` as the client's last event, matching every other test
in the file, and drops a trailing press that raced two independently
scheduled fibers rather than testing `serveClosing`. `okay-ui/BUGS.md`
has the full account, including the lesson: a changelog line
describing what a test does is not evidence that it ran.

`sbt integrationTest`, scoped to `okayUiJVM/testOnly okay.ui.TestWire`
with the class's Live tag disabled locally to drive it directly: 5
repeats clean (was 1-for-1 hung before). The full monorepo `sbt test`
afterward: 0 failures.
