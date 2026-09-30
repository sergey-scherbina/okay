- [ ] okay2-join-within — the okay2 port of stage 2 of
      specs/stream-join.md: `WindowJoin` (the machine), `WindowJoin.stage`
      and `Source.joinWithin` in okay2-stream, the event-time windowed
      join by key. okay2's `Source` has no `either`; the port either adds
      it over okay2's `merge` (tag each side with `Writer.mapAt`, then
      merge) or drives the machine from two channels by readiness — the
      spec's Decisions say which and why. Tests: the core's TestWindowJoin
      and TestSourceJoinWithin, minus the release law okay2 has no scope
      for. (2026-09-30, operator ask)
