# okay-cluster

The remote channel: the Channel discipline with a socket underneath.

- `Remote.listen(serverSocket)` — accepted chunks land in an ordinary
  local `Channel[Chunk[A]]`; downstream code cannot tell it is remote.
- `Remote.connect(host, port)` — a `Sender`: one Schema-encoded JSON
  frame per line (CBOR when its dialect lands); closing the wire
  closes the channel after the drain.
- The cross-node contract is the Aggregator merge: a variance folded
  half locally and half across the wire merges to the local run's
  answer. A damaged frame is dropped as data; the stream lives.

Actors (a Stage with a mailbox Channel), worker recompute and the
JS-client acceptance test are the stated next steps (specs/cluster.md).
