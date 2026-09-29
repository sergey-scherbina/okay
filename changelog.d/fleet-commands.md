## fleet-commands - the control plane as a topic the service folds

- `Fleet.Command` = Spawn(spec, by) | Send(id, control, by), `commandJson`
  and the total `command`; `spawn(spec, by)` and `Event.Spawned` carry the
  principal; `Event.Refused(seq, by, why)` is a command the service would
  not apply — on the agents record, where the sender is already listening.
- `fleet.commands(topic, allow, from, applied)`: follows the `commands`
  topic (Streams.tail), asks `allow(by, command)`, applies or refuses, and
  reports every settled offset. A screen in another process appends through
  a RemoteStore with the session's principal (nadia NAD-14/NAD-20).
- `TestFleetCommands` (2, JVM). Spec: specs/agent-fleet.md "Commands".
