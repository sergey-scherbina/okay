- [ ] route-headers-adopt — REDUCED 2026-09-11 after looking properly.
      `McpHttp.route` is a TOTAL `Request => Response` by design —
      `McpAuth` depends on that totality, and its comment says so — and
      it answers any verb on one path, which a `Router` entry cannot
      say. okay-script's header reads are all inside `Site`, a page
      server rather than a table. So there is no clean seat for stage
      A beyond the tests; the stage-B half found its consumer instead
      (okay-admin, then okay-demo). Left open in case McpHttp ever
      becomes a table, not as work waiting to be done.
