- [~] fleet-commands — the control plane as a topic (nadia BACKLOG NAD-14
      answer, NAD-20): `Fleet.Command` = Spawn(spec, by) | Send(id, control,
      by), `Fleet.command(json)`/`commandJson`, and `fleet.commands(topic,
      allow, from, applied)` — a program that tails the `commands` topic and
      applies each, asking `allow(by, command)` first; a refusal is
      `Event.Refused(seq, by, why)` on the agents record so the UI sees why;
      `Event.Spawned` gains `by`. The UI appends through a RemoteStore with
      the session's principal. Depends on fleet-events. Additive.
