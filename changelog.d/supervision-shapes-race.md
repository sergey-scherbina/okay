## supervision-shapes-race - a scope cancels children forked after its failure; TestSupervisionShapes made deterministic

`compare`'s TestSupervisionShapes went red three times out of three at
load ~12 on untouched master, and green three out of three a minute
later. Reading it found two different things.

- **A defect in `Async.Nursery`.** A child forked AFTER the scope's first
  failure (the body still running) was never cancelled, because
  `cancelAll` reaches only the kids it saw. A new test waited 5 s for its
  cancellation and failed. Now `cancelAll` marks the scope failed before
  reading `kids`, and `fork` cancels a joining child when the scope has
  failed.
- **Two races in the test, not in the code.**
  - The failing branch could throw before a healthy sibling reached its
    `await`, leaving no canceler to call (a count of 0 though the shape
    was right). A latch now orders them, with a bounded wait so that a
    sequential scheduler fails the count instead of hanging.
  - A cancel that meets a child between its registration and the drive
    storing the canceler is delivered by the child's own drive a moment
    later (6 of 9 had arrived when `supervised` answered). The test now
    waits, bounded, for the cancellations to ARRIVE.

The remaining gap, that a failing scope answers before its cancelled
children have stopped, is filed as supervised-waits-on-failure, with the
reason it is not fixed here: JS's `PromiseDrive` might never settle a
cancelled child.
