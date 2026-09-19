# okay-actor

An actor is a mailbox, a loop that reads it one message at a time, private state that needs no locks BECAUSE of that, and an address others can send to.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-actor.md`](../docs/modules/okay-actor.md) | what it is, and the reasoning |
| [`specs/actor.md`](../specs/actor.md) | the design and its decisions |
