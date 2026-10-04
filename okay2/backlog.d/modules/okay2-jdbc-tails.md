- [ ] okay2-jdbc-tails: the rest of okay-jdbc and okay-sql's drivers on
      okay2. okay2-sql (stage 42) ported the seam, the typed layer, Query,
      Tx and Pool, okay2-jdbc ported `JdbcSql`, and okay2-jdbc-migrate
      (2026-10-04) `Migrate`, `BulkLoad` and `JdbcInterop`. Still
      unported: `Writes` (the intent-first write bridge over okay-persist,
      and TestSqlite's crash-window case), `Poll`, `SqlStore` — all three
      need an okay2-persist log, whose typed view needs okay2-codec's CBOR
      (okay2-codec-cbor) — the pg wire driver (okay-pg), and the Live pg
      suites. (2026-09-25)
