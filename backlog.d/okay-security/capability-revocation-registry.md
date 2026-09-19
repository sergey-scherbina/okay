- [ ] capability-revocation-registry — the handle exists
      (`Caveat.Agent`, `checking`'s `revoked` predicate, and the MCP
      door asking it); the LIST does not, and that is on purpose. What
      store, what TTL, who writes to it and how it reaches several
      verifiers are answers a deployment gives, and designed blind
      they are what this repository defers elsewhere. Until then
      `revoked` is a function and the caller brings its own. The note
      that matters when somebody picks this up: the door already asks
      about the root id AND each `Agent` caveat, so the registry only
      has to be a set of strings — the hard part is where it lives,
      not what it holds. Filed by capability-revocation-handle
      (security.md stage 7).
