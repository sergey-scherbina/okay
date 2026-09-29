- [~] fleet-events — what a workspace UI folds instead of polling an agent
      (nadia BACKLOG NAD-14 answer, NAD-19): `Fleet.Event` = Spawned | Phased |
      Stepped | Turned | Finished, `Fleet.event(json)` the one decoder (restore
      folds through it too), `Fleet.events(topic, from)` = `Streams.tail` mapped to
      events, and in-process `fleet.events: Source[Event]` (a channel per
      subscriber, told as the record is written). Additive; spec
      specs/agent-fleet.md gains an "Events" section. Gate: module suite +
      affected compile.
