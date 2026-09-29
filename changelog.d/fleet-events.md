## fleet-events - the agents record, typed, as a feed

- `Fleet.Event` (Spawned | Phased | Stepped | Turned | Finished) and
  `Fleet.event(json)`, THE decoder of the `agents` record — `restore` folds
  through it too, so a feed and a restart cannot read one record two ways.
- `Fleet.events(topic, from)`: `Streams.tail` mapped to events, in any
  process that can read the topic (a `RemoteStore` included) — what a
  workspace UI folds instead of polling (nadia BACKLOG NAD-14/NAD-19).
- `fleet.events()`: the in-process feed, a channel per listener offered
  under the fleet's lock; a slow listener is dropped, the fleet never held.
- `TestFleetEvents` (3, JVM). Spec: specs/agent-fleet.md "Events".
