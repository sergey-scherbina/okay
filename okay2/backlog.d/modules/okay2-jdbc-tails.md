- [ ] okay2-jdbc-tails: the rest of okay-jdbc and okay-sql's drivers on
      okay2, once a caller needs them. okay2-sql (stage 42) ported the
      seam, the typed layer, Query, Tx and Pool, and okay2-jdbc ported
      `JdbcSql`. Still unported: `Writes` (the intent-first write bridge
      over okay-persist, and TestSqlite's crash-window case), `Migrate`,
      `Poll`, `SqlStore`, `BulkLoad`, `JdbcInterop`, the pg wire driver
      (okay-pg), and the Live pg suites. (2026-09-25)
