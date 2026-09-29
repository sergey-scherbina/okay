- [~] fleet-test-race — `TestFleet` "Stop/Kill" and "restore" spawn two agents
      over ONE tick channel and send two ticks; agent a can take both, b never
      reaches step 1, `until` times out (seen in the full gate of agent-fleet,
      which was landed as 654a4f98b BEFORE its RED was read — the operator's
      rule is read the verdict first; recorded here so the next reader knows
      master carried two flaky tests for the minutes between). Fix: a channel
      per agent (`Ticking(ticksFor: Spec => Channel)`). Test-only lane.
