- [x] optics-outside-routes-adopt — DONE (2026-09-11). Every
      hand-written router in the tree is a `Router` now: okay-ops and
      okay-script (stage 4), okay-http's acceptance fixture (stage 1),
      okay-demo (stage 8's lane), okay-chat, and finally okay-admin's
      `/admin/replay` and okay-demo's `/whoami`. Each conversion found
      something; the list is in the CHANGELOG entries.

      TWO WERE LEFT ALONE ON PURPOSE, so nobody converts them later by
      matching on shape. okay-acme is already correct — it has its own
      `path(url)` cutting the query before it compares, the only module
      that did. okay-security's `McpAuth` matches
      `startsWith("/.well-known/oauth-protected-resource")`, which
      looks like the `/person` → `/personal` sloppiness and is not:
      RFC 9728 allows the resource's path as a suffix, so the prefix
      is probably deliberate and converting it would break spec
      compliance.
