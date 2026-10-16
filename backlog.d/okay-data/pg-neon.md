- [ ] pg-neon — okay-pg against Neon (serverless Postgres, Lakebase), proven
      rather than assumed. By the code it should work. It speaks the v3
      wire. TLS goes through SSLRequest, and `Tls.client` passes the host
      name, so the JDK sends the SNI Neon routes by. SCRAM-SHA-256 is in
      place. Every statement is UNNAMED (`Parse` with ""), so PgBouncer's
      transaction pooling does not break it. None of this has run against
      Neon. A LIVE suite `TestPgNeon` (skips without `OKAY_NEON_URL` and
      `OKAY_NEON_POOLED_URL`, read from ~/.config/okay/neon.env, never
      committed) should cover TLS at `require` and at `verify-full` (Neon's
      certificates are from a public CA), SCRAM, `select version()`, a table
      created, written, read and dropped, commit and rollback, the same over
      the `-pooler` endpoint, and the FIRST connect's time to a
      suspended compute (a cold start: what the default connect timeout must
      allow). Known differences to document: the pooler endpoint has no
      LISTEN/NOTIFY, session `SET`, advisory locks or temp tables (use the
      direct endpoint), and the proxy drops idle connections, so a pool
      reconnects. Waits for the operator: a free Neon project with a test
      branch (eu-central-1), its two connection strings in that file.
      Asked 2026-10-16 (neon.com/blog/neon-now-has-realtime).
