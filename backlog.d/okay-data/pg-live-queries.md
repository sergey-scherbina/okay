- [ ] pg-live-queries — live queries over ANY Postgres, Neon included, with
      no vendor in between: a query's result kept current as its tables
      change. The road: logical replication in okay-pg (the replication
      connection, `START_REPLICATION … LOGICAL` over pgoutput, a slot and a
      publication), changes as a `Source`, fanned out through okay-live's
      `Hub`/`Registry`, to a browser through okay-foreign-ws and okay-ts.
      The authorization model to borrow is Neon Realtime's "sealed" query:
      the SERVER defines and signs what a client may subscribe to, which is
      our audit's rule. Neon Realtime itself (beta 2026-10-15) is not a
      dependency to take: its protocol is changing and undocumented, there
      are TypeScript SDKs only, and no RLS. An okay-ts adapter over its
      `client.subscribe(sealed)` is for after its beta. okay-pg has neither
      logical replication nor LISTEN/NOTIFY today.
