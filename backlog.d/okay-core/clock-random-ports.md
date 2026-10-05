- [ ] clock-random-ports — stage 2 of specs/audit-ready.md: `okay.Clock`
      (`Now`) and `okay.Random` (`Next(bits)`) as effects in the core,
      operations only; `SystemClock`/`SystemRandom` handlers in okay-platform;
      the journal handlers answer them from the record so a replay carries
      the run's time, not the replay's. Replayable.scala already names both as
      the effects that are NOT replayable — which is why they must be ports
      answered by a handler. Additive; no existing signature changes.
