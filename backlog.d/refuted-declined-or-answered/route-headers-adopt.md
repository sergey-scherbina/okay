- route-headers-adopt — CLOSED 2026-09-19, per its own 2026-09-11
  reduction: `McpHttp.route` is a TOTAL `Request => Response` by
  design (`McpAuth` depends on that totality) and answers any verb
  on one path, which a `Router` entry cannot say; okay-script's
  header reads all live inside `Site`, a page server rather than a
  table. No clean seat for stage A beyond the tests, and stage B
  already found its consumer (okay-admin, then okay-demo). Reopens
  only if McpHttp ever becomes a table — not work waiting to be done
  today.
