## fleet-test-race - TestFleet: a tick channel per agent

- Two suites spawned two agents over ONE tick channel and sent two ticks;
  the first agent could take both and the second never reached step 1 —
  a 5 s timeout under a loaded gate. Now a `Ticks` table hands each agent
  its own channel by task name. Three consecutive module runs green.
- Recorded honestly: agent-fleet (654a4f98b) was landed while its full
  gate was RED on exactly these two — the verdict was not read before
  `land.sh`. The fleet itself was not at fault.
